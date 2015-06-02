setwd("/home/john/Kaggle/Machine Learning Mania 2015/kaggle data")

library(dplyr)
library(sqldf)
library(ggplot2)

bpi_tourney <- read.csv("bpi_tourney.csv",header=TRUE,stringsAsFactors=FALSE)

## quick and dirty logit model
dirty.fit <- glm(win~team_bpi+bpi_diff,data=bpi_tourney,family="binomial")
dirty.fit2 <- glm(win~bpi_diff,data=bpi_tourney,family="binomial")

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


predict.glm(dirty.fit,newdata=bpi_tourney[c(1,5,10,15,20),])

### there are three models I want to fit. They are
#   1) bpi_diff
#   2) baseline bpi and bpi diff
#   3) with an interaction term

# initialize a list for the log_loss on fitted values
log_loss_matrix <- matrix(nrow=10,ncol=3)
log_loss_matrix_adj <- matrix(nrow=10,ncol=3)


log_loss <- function(fit,pred){
  n <- length(pred)
  val <- -1/n * sum(((bpi_tourney$win[cvdata[[i]]]*log(pred))+
                            (1-bpi_tourney$win[cvdata[[i]]])*log(1-pred)))
  return(val)
}

for(i in 1:10){
  fit1 <- glm(data=bpi_tourney[traindata[[i]],],win~bpi_diff,family="binomial")
  fit2 <- glm(data=bpi_tourney[traindata[[i]],],win~team_bpi+bpi_diff,family="binomial")
  fit3 <- glm(data=bpi_tourney[traindata[[i]],],win~team_bpi+bpi_diff+team_bpi*bpi_diff,family="binomial")

  pred1 <- predict.glm(fit1,newdata=bpi_tourney[cvdata[[i]],],type="response")
  pred2 <- predict.glm(fit2,newdata=bpi_tourney[cvdata[[i]],],type="response")
  pred3 <- predict.glm(fit3,newdata=bpi_tourney[cvdata[[i]],],type="response")
  
  log_loss_matrix[i,1] <- log_loss(fit1,pred1)
  log_loss_matrix[i,2] <- log_loss(fit2,pred2)
  log_loss_matrix[i,3] <- log_loss(fit3,pred3)
  
  fit1_adj <- glm(data=bpi_tourney[traindata[[i]],],win~bpi_diff -1 ,family="binomial")
  fit2_adj <- glm(data=bpi_tourney[traindata[[i]],],win~team_bpi+bpi_diff -1,family="binomial")
  fit3_adj <- glm(data=bpi_tourney[traindata[[i]],],win~team_bpi+bpi_diff+team_bpi*bpi_diff -1,family="binomial")
  
  pred1_adj <- predict.glm(fit1_adj,newdata=bpi_tourney[cvdata[[i]],],type="response")
  pred2_adj <- predict.glm(fit2_adj,newdata=bpi_tourney[cvdata[[i]],],type="response")
  pred3_adj <- predict.glm(fit3_adj,newdata=bpi_tourney[cvdata[[i]],],type="response")
  
  log_loss_matrix_adj[i,1] <- log_loss(fit1_adj,pred1_adj)
  log_loss_matrix_adj[i,2] <- log_loss(fit2_adj,pred2_adj)
  log_loss_matrix_adj[i,3] <- log_loss(fit3_adj,pred3_adj)
}

loss_df <- data.frame(log_loss_matrix)
loss_df_adj <- data.frame(log_loss_matrix_adj)

colnames(loss_df) <- c("fit1","fit2","fit3")
colnames(loss_df_adj) <- c("fit1","fit2","fit3")

plot <- ggplot(data=loss_df,
               aes(x=1:10,y=loss_df[,1])) +
        geom_line(colour="black") +
        geom_line(aes(x=1:10,y=loss_df[,2]),colour="red") +
        geom_line(aes(x=1:10,y=loss_df[,3]),colour="blue") +
        geom_line(aes(x=1:10,y=loss_df_adj[,1]),
                  colour="black",
                  linetype = "dashed") +
        geom_line(aes(x=1:10,y=loss_df_adj[,2]),
                  colour="red",
                  linetype = 2) +
        geom_line(aes(x=1:10,y=loss_df_adj[,3]),
                  colour="blue",
                  linetype = 2) 

plot

## summarise

loss_df %>% summarise(mean_fit1 = mean(fit1),
                      mean_fit2 = mean(fit2),
                      mean_fit3 = mean(fit3))
loss_df_adj %>% summarise(mean_fit1 = mean(fit1),
                          mean_fit2 = mean(fit2),
                          mean_fit3 = mean(fit3))

str(cvdata)
## Looks like the intercept only moidels works the best, and only on bpi_diff.
## This is a very simple model. Let's pull the coefficients.
coef <- rep(0,10)
for(i in 1:10){
  fit1_adj <- glm(data=bpi_tourney[traindata[[i]],],win~bpi_diff -1 ,family="binomial")
  coef[i] <- fit1_adj$coef[[1]]
}
coef
mean(coef)

