library(sqldf)
library(dplyr)
library(stringr)
library(ggplot2)

### Final model. Combine BPI and seed info ###


## First, generate seed based probabilities for 1985 - 2011, the year up until which we have BPI data

setwd("/home/john/Kaggle/Machine Learning Mania 2015/kaggle data")
tourney_results <- read.csv("tourney_compact_results.csv",header=TRUE,stringsAsFactors=FALSE)
tourney_seeds <- read.csv("tourney_seeds.csv",header=TRUE,stringsAsFactors=FALSE)
tourney_seeds$seed2 <- str_sub(tourney_seeds$seed,2,3)

tourney_results <- filter(tourney_results,season <= 2011)
tourney_seeds <- filter(tourney_seeds,season <= 2011)

# Match one side
results_seeds <- sqldf('
                       select
                       a.season,
                       a.wteam,
                       a.lteam,
                       b.seed2 as wseed
                       from
                       tourney_results as a,
                       tourney_seeds as b
                       where
                       a.season = b.season
                       and a.wteam = b.team')
results_seeds2 <- sqldf('
                        select
                        a.season,
                        a.wteam,
                        a.lteam,
                        a.wseed,
                        b.seed2 as lseed
                        from
                        results_seeds as a,
                        tourney_seeds as b
                        where
                        a.season = b.season
                        and a.lteam = b.team')

results_seeds2$matchup <- ifelse(as.numeric(results_seeds2$wseed) < as.numeric(results_seeds2$lseed),
                                 paste(results_seeds2$wseed,results_seeds2$lseed,sep = "-"),
                                 paste(results_seeds2$lseed,results_seeds2$wseed,sep = "-"))
matchups <- results_seeds2 %>%
  group_by(matchup,wseed,lseed) %>%
  summarise(counts = n())

totals <- results_seeds2 %>%
  group_by(matchup) %>%
  summarise(count = n())

matchup_probs <- sqldf('
                       select
                       a.*,
                       b.count
                       from
                       matchups as a,
                       totals as b
                       where
                       a.matchup = b.matchup')

matchup_probs2 <- mutate(matchup_probs,
                         wprob = counts/count,
                         team = wseed,
                         opp = lseed) %>%
  select(matchup,
         team,
         opp,
         wprob,
         count)

## Now need to do some manual editing, such as 01 vs 01 is a 50-50 not 100% matchup, and other 100% matchups are actually
## 100-0 matchups

## need to create vectors that will become the appended rows  

m <- character(0) #matchup
t <- character(0) #team
o <- character(0) #opponent
p <- numeric(0)   #winprob
j <- 1            #index 

for(i in 1:dim(matchup_probs2)[1]){
  if(matchup_probs2$wprob[i] == 1){
    if(matchup_probs2$team[i] == matchup_probs2$opp[i]){
      matchup_probs2$wprob[i] <- 0.5
    }
    else{
      m[j] <- matchup_probs2$matchup[i]
      t[j] <- matchup_probs2$opp[i]
      o[j] <- matchup_probs2$team[i]
      p[j] <- 0
      j <- j+1
    }
  }
}

rows_to_append <- data.frame(cbind(m,t,o,p,rep(0,length(m))))
colnames(rows_to_append) <- c("matchup","team","opp","wprob","count")
probs_final <- rbind(matchup_probs2,rows_to_append)

## Also need the case that a brand new matchup, never before seen, such as 01-15, happens. 
seeds <- data.frame(c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16"),
                    stringsAsFactors = FALSE)
colnames(seeds) <- "seed"

seeds_cart_product <- sqldf('
                            select
                            a.seed as team,
                            b.seed as opp
                            from
                            seeds as a,
                            seeds as b
                            ')
## now bring in our empiriccal probabilities ##

matchup_probs_final <- sqldf('
                             select
                             a.*,
                             b.wprob,
                             b.count
                             from
                             seeds_cart_product as a
                             left join
                             probs_final as b
                             on
                             a.team = b.team
                             and a.opp = b.opp')

for(k in 1:dim(matchup_probs_final)[1]){
  if(is.na(matchup_probs_final$wprob[k])){
    matchup_probs_final$wprob[k] <- 0.5
    matchup_probs_final$count[k] <- 0
  }
}

head(matchup_probs_final)

write.csv(matchup_probs_final,"empirical_seed_probs_1985_2011.csv")

#### Next step, bring in BPI data ####

bpi_tourney <- read.csv("bpi_tourney.csv",header=TRUE,stringsAsFactors=FALSE)
## get seed info for bpi_teams
tourney_seeds <- read.csv("tourney_seeds.csv",header=TRUE,stringsAsFactors=FALSE)
tourney_seeds$seed2 <- str_sub(tourney_seeds$seed,2,3)
tourney_seeds <- filter(tourney_seeds,season >= 2012)

bpi_tourney_seeds1 <- sqldf('
                            select
                              a.*,
                              b.seed2 as team_seed
                            from
                              bpi_tourney as a,
                              tourney_seeds as b
                            where
                              a.season = b.season
                            and a.teamid = b.team')
bpi_tourney_seeds2 <- sqldf('
                            select
                              a.*,
                              b.seed2 as opp_seed
                            from
                              bpi_tourney_seeds1 as a,
                              tourney_seeds as b
                            where
                              a.season = b.season
                            and a.opp = b.team')
## Now fold in seed based probabilities

seed_probs <- read.csv("empirical_seed_probs_1985_2011.csv")

tourney_seed_probs1 <- sqldf('
                             select
                              a.*,
                              b.wprob,
                              b.count,
                              sqrt(b.count) as sqrt_count,
                              log(b.count + 0.01) as log_count
                             from
                              bpi_tourney_seeds2 as a,
                              seed_probs as b
                             where
                              a.team_seed = b.team
                             and a.opp_seed = b.opp')

write.csv(tourney_seed_probs1,"bpi_seed_data.csv")


#### Now fit some models and cross validate

log_loss <- function(fit){
  n <- fit$df.null + 1
  log_loss <- -1/n * sum(((fit$data$win*log(fit$fitted.values))+
                            (1-fit$data$win)*log(1-fit$fitted.value)))
}

# Generate the different test and CV datasets
cvdata <- list()
traindata <- list()
cvindex <- c()
N <- 201
N10 <- floor(N/10)
for(i in 1:10){
  if(i == 1){
    cvdata[[i]] <- sample(c(1:N),N10,replace=FALSE)
    traindata[[i]] <- c(1:N)[-cvdata[[i]]]
    cvindex <- c(cvindex,cvdata[[i]])
  }  
  else if(i != 1){
    cvdata[[i]] <- sample(c(1:N)[-cvindex],N10,replace=FALSE)
    traindata[[i]] <- c(1:N)[-cvdata[[i]]]
    cvindex <- c(cvindex,cvdata[[i]])
  }
}

# initialize a list for the log_loss on fitted values
log_loss_matrix <- matrix(nrow=10,ncol=5)



log_loss <- function(fit,pred){
  n <- length(pred)
  val <- -1/n * sum(((tourney_seed_probs1$win[cvdata[[i]]]*log(pred))+
                       (1-tourney_seed_probs1$win[cvdata[[i]]])*log(1-pred)))
  return(val)
}

for(i in 1:10){
  fit1 <- glm(data=tourney_seed_probs1[traindata[[i]],],win~bpi_diff-1,family="binomial")
  fit2 <- glm(data=tourney_seed_probs1[traindata[[i]],],win~bpi_diff+wprob,family="binomial")
  fit3 <- glm(data=tourney_seed_probs1[traindata[[i]],],win~bpi_diff+wprob*count,family="binomial")
  fit4 <- glm(data=tourney_seed_probs1[traindata[[i]],],win~bpi_diff+wprob*sqrt_count,family="binomial")
  fit5 <- glm(data=tourney_seed_probs1[traindata[[i]],],win~bpi_diff+wprob*log_count,family="binomial")
  
  pred1 <- predict.glm(fit1,newdata=tourney_seed_probs1[cvdata[[i]],],type="response")
  pred2 <- predict.glm(fit2,newdata=tourney_seed_probs1[cvdata[[i]],],type="response")
  pred3 <- predict.glm(fit3,newdata=tourney_seed_probs1[cvdata[[i]],],type="response")
  pred4 <- predict.glm(fit4,newdata=tourney_seed_probs1[cvdata[[i]],],type="response")
  pred5 <- predict.glm(fit5,newdata=tourney_seed_probs1[cvdata[[i]],],type="response")
  
  log_loss_matrix[i,1] <- log_loss(fit1,pred1)
  log_loss_matrix[i,2] <- log_loss(fit2,pred2)
  log_loss_matrix[i,3] <- log_loss(fit3,pred3)
  log_loss_matrix[i,4] <- log_loss(fit4,pred4)
  log_loss_matrix[i,5] <- log_loss(fit5,pred5)
}

loss_df <- data.frame(log_loss_matrix)

colnames(loss_df) <- c("fit1","fit2","fit3","fit4","fit5")

plot <- ggplot(data=loss_df,
               aes(x=1:10,y=loss_df[,1])) +
  geom_line(colour="black") +
  geom_line(aes(x=1:10,y=loss_df[,2]),colour="red") +
  geom_line(aes(x=1:10,y=loss_df[,3]),colour="blue") +
  geom_line(aes(x=1:10,y=loss_df[,4]),colour="green") +
  geom_line(aes(x=1:10,y=loss_df[,5]),colour="gray")

plot

loss_df %>% summarise(avg1 = mean(fit1),
                      avg2 = mean(fit2),
                      avg3 = mean(fit3),
                      avg4 = mean(fit4),
                      avg5 = mean(fit5))

fit1
