setwd('/home/john/Kaggle/West Nile Virus Prediction/Data')
library(dplyr)
library(glmnet) #for lasso regression


training_full <- read.csv("training_full_final.csv", stringsAsFactors = FALSE, header = TRUE)

training_full <- training_full %>%
    select(cut,
           species2,
           c10,
           avg_may_temp,
           num_may_rain,
           avg_may_dew,
           avg_may_wet,
           avg_avg,
           avg_min,
           avg_max,
           avg_dew,
           avg_wet,
           wet_con,
           temp_range,
           temp30,
           rain14,
           dew30,
           wet30,
           range1,
           wnvpresent)
str(training_full)
training_full$cut <- as.factor(training_full$cut)
training_full$species2 <- as.factor(training_full$species2)
training_full$c10 <- as.factor(training_full$c10)
str(training_full)

Y <- as.factor(training_full$wnvpresent)
X <- model.matrix(wnvpresent~.-1,data=training_full)

set.seed(20150515)
lasso_fit <- cv.glmnet(X,Y,family='binomial',alpha=1,type.measure='auc')
plot(lasso_fit)
summary(lasso_fit)
str(lasso_fit)
lasso_fit$lambda.min
lasso_fit$lambda.1se
cbind(lasso_fit$lambda,lasso_fit$nzero,lasso_fit$cvm)
dim(coef(lasso_fit))



## for predictions
lambda1 <- lasso_fit$lambda.1se
lasso_fit$cvm[which(lasso_fit$lambda == lambda1)]
## AUC = 0.81437
lambda2 <- lasso_fit$lambda.min
lasso_fit$cvm[which(lasso_fit$lambda == lambda2)]
test_final <- read.csv("test_final.csv",stringsAsFactors = FALSE, header = TRUE)
# 0.8225344

test_final <- test_final %>%
                    select(cut,
                      species2,
                      c10,
                      avg_may_temp,
                      num_may_rain,
                      avg_may_dew,
                      avg_may_wet,
                      avg_avg,
                      avg_min,
                      avg_max,
                      avg_dew,
                      avg_wet,
                      wet_con,
                      temp_range,
                      temp30,
                      rain14,
                      dew30,
                      wet30,
                      range1)
str(test_final)
test_final$cut <- as.factor(test_final$cut)
test_final$species2 <- as.factor(test_final$species2)
test_final$c10 <- as.factor(test_final$c10)

Xp <- model.matrix(~.-1,data=test_final)


predict1 <- predict(object = lasso_fit,
                              newx= Xp,
                              s = lambda1,
                              type = 'response')
head(predict1)

lasso1se_submission <- read.csv("lasso1se_submission.csv",
                                stringsAsFactors = FALSE,
                                header = TRUE)
cbind(head(predict1),head(lasso1se_submission))
## So these values are slightly different, it may be due to the fact that there 
## were two intercept columns. I'm fixing that here. 


test_final <- read.csv("test_final.csv",stringsAsFactors = FALSE, header = TRUE)
lasso1se_resubmission <- data.frame(test_final$id,predict1)
colnames(lasso1se_resubmission) <- c("Id","WnvPresent")
write.csv(lasso1se_resubmission,"lasso1se_resubmission.csv",row.names=FALSE)

predict2 <- predict(object = lasso_fit,
                    newx= Xp,
                    s = lambda2,
                    type = 'response')
head(predict2)
lassomin_resubmission <- data.frame(test_final$id,predict2)
colnames(lassomin_resubmission) <- c("Id","WnvPresent")
write.csv(lassomin_resubmission,"lassomin_resubmission.csv",row.names=FALSE)



## Lasso With Lat/Lon rather than region
setwd('/home/john/Kaggle/West Nile Virus Prediction/Data')
library(dplyr)
library(glmnet) #for lasso 


training_full <- read.csv("training_full_final.csv", stringsAsFactors = FALSE, header = TRUE)

training_full <- training_full %>%
    select(cut,
           species2,
           latitude,
           longitude,
           avg_may_temp,
           num_may_rain,
           avg_may_dew,
           avg_may_wet,
           avg_avg,
           avg_min,
           avg_max,
           avg_dew,
           avg_wet,
           wet_con,
           temp_range,
           temp30,
           rain14,
           dew30,
           wet30,
           range1,
           wnvpresent)
str(training_full)
training_full$cut <- as.factor(training_full$cut)
training_full$species2 <- as.factor(training_full$species2)

str(training_full)

Y <- as.factor(training_full$wnvpresent)
X <- model.matrix(wnvpresent~.-1,data=training_full)

set.seed(201505152)
lasso_fit2 <- cv.glmnet(X,Y,family='binomial',alpha=1,type.measure='auc')
plot(lasso_fit2)
summary(lasso_fit2)
str(lasso_fit2)
lasso_fit2$lambda.min
lasso_fit2$lambda.1se #s26
lasso_fit2$cvm[which(lasso_fit2$lambda == lasso_fit2$lambda.min)]
lasso_fit2$cvm[which(lasso_fit2$lambda == lasso_fit2$lambda.1se)]

cbind(lasso_fit$lambda,lasso_fit$nzero,lasso_fit$cvm)
dim(coef(lasso_fit))

coef(lasso_fit2)

## for predictions
lambda_v2 <- lasso_fit2$lambda.1se


test_final <- read.csv("test_final.csv",stringsAsFactors = FALSE, header = TRUE)
test_final <- test_final %>%
    select(cut,
           species2,
           latitude,
           longitude,
           avg_may_temp,
           num_may_rain,
           avg_may_dew,
           avg_may_wet,
           avg_avg,
           avg_min,
           avg_max,
           avg_dew,
           avg_wet,
           wet_con,
           temp_range,
           temp30,
           rain14,
           dew30,
           wet30,
           range1)
str(test_final)
test_final$cut <- as.factor(test_final$cut)
test_final$species2 <- as.factor(test_final$species2)

Xp <- model.matrix(~.-1,data=test_final)


predict_v2 <- predict(object = lasso_fit2,
                    newx= Xp,
                    s = lambda_v2,
                    type = 'response')

head(predict_v2)
lassoV2_submission <- read.csv("lassoV2_submission.csv",
                               stringsAsFactors = FALSE,
                               header = TRUE)
cbind(head(predict_v2),head(lassoV2_submission))
## Again slightly different. I'll resubmit
test_final <- read.csv("test_final.csv",stringsAsFactors = FALSE, header = TRUE)
lassoV2_resubmission <- data.frame(test_final$id,predict_v2)
colnames(lassoV2_resubmission) <- c("Id","WnvPresent")
write.csv(lassoV2_resubmission,"lassoV2_resubmission.csv",row.names=FALSE)
