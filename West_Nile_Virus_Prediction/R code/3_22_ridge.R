setwd('/home/john/Kaggle/West Nile Virus Prediction/Data')
library(dplyr)
library(glmnet) #for ridge regression

### Fit with long/lat
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

set.seed(201505162)
ridge_fit <- cv.glmnet(X,Y,family='binomial',alpha=0,type.measure='auc')
plot(ridge_fit)
summary(ridge_fit)
str(ridge_fit)
ridge_fit$lambda.min
ridge_fit$lambda.1se
cbind(ridge_fit$lambda,ridge_fit$nzero,ridge_fit$cvm)
dim(coef(ridge_fit))
coef(ridge_fit)


ridge_lambda <- ridge_fit$lambda.1se
ridge_fit$cvm[which(ridge_fit$lambda == ridge_lambda)]

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


predict_ridge <- predict(object = ridge_fit,
                      newx= Xp,
                      s = ridge_lambda,
                      type = 'response')

head(predict_ridge)


test_final <- read.csv("test_final.csv",stringsAsFactors = FALSE, header = TRUE)
ridge_resubmission <- data.frame(test_final$id,predict_ridge)
colnames(ridge_resubmission) <- c("Id","WnvPresent")
write.csv(ridge_resubmission,"ridge_resubmission.csv",row.names=FALSE)
