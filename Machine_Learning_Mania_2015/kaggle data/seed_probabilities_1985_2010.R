library(sqldf)
library(dplyr)
library(stringr)

setwd("/home/john/Kaggle/Machine Learning Mania 2015/kaggle data")
tourney_results <- read.csv("tourney_compact_results.csv",header=TRUE,stringsAsFactors=FALSE)
tourney_seeds <- read.csv("tourney_seeds.csv",header=TRUE,stringsAsFactors=FALSE)
tourney_seeds$seed2 <- str_sub(tourney_seeds$seed,2,3)

tourney_results <- filter(tourney_results,season <= 2010)
tourney_seeds <- filter(tourney_seeds,season <= 2010)

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
         wprob)

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

rows_to_append <- data.frame(cbind(m,t,o,p))
colnames(rows_to_append) <- c("matchup","team","opp","wprob")
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
## now bring in our empiracle probabilities ##

matchup_probs_final <- sqldf('
                             select
                             a.*,
                             b.wprob
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
  }
}

head(matchup_probs_final)

write.csv(matchup_probs_final,"empirical_seed_probs_1985_2010.csv")
                        