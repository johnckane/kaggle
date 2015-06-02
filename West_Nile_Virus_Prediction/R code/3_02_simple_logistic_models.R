setwd('/home/john/Kaggle/West Nile Virus Prediction/Data')
library(Metrics) #to calculate AUC of ROC curve
library(dplyr)
library(ggplot2)



training_final <- read.csv("training_final.csv", stringsAsFactors = FALSE, header = TRUE)
cv_final <- read.csv("cv_final.csv", stringsAsFactors = FALSE, header = TRUE)

colnames(training_final)
colnames(cv_final)

## First let's play with some really simple models.
species_fit <- glm(data = training_final,
                   wnvpresent ~ species2,
                   family = 'binomial')
species_cv <- predict(object = species_fit,
                      newdata = cv_final,
                      type = "response")
auc(actual = cv_final$wnvpresent,
    predicted = species_cv)
# this gives an AUC of .6588838
day_of_year_fit <- glm(data = training_final,
                       wnvpresent ~ day_of_year,
                       family = 'binomial')
day_of_year_cv <- predict(object = day_of_year_fit,
                          newdata = cv_final,
                          type = 'response')
auc(actual = cv_final$wnvpresent,
    predicted = day_of_year_cv)
# this gives an AUC of 0.649423
doy_cut_fit <- glm(data = training_final,
                   wnvpresent ~ factor(cut),
                   family = 'binomial')
doy_cut_cv <- predict(object = doy_cut_fit,
                      newdata = cv_final,
                      type = 'response')
auc(actual = cv_final$wnvpresent,
    predicted = doy_cut_cv)
# this gives AUC of 0.7299017
may_weather_fit <- glm(data = training_final,
                       wnvpresent ~ avg_may_temp + num_may_rain + avg_may_wet,
                       family = 'binomial')
may_weather_cv <- predict(object = may_weather_fit,
                          newdata = cv_final,
                          type = 'response')
auc(actual = cv_final$wnvpresent,
    predicted = may_weather_cv)
#gives an auc of 0.6821394
summary(may_weather_fit)


## play with regions 
## let's start with a simple data set
region_data_train <- select(training_final,c2,c3,c4,c5,c6,c7,c8,c9,c10,wnvpresent)
region_data_cv <- select(cv_final,c2,c3,c4,c5,c6,c7,c8,c9,c10,wnvpresent)
plot_data <- matrix(0,nrow=9,ncol=2)

region_data_train$c2 <- as.factor(region_data_train$c2)
region_data_train$c3 <- as.factor(region_data_train$c3)
region_data_train$c4 <- as.factor(region_data_train$c4)
region_data_train$c5 <- as.factor(region_data_train$c5)
region_data_train$c6 <- as.factor(region_data_train$c6)
region_data_train$c7 <- as.factor(region_data_train$c7)
region_data_train$c8 <- as.factor(region_data_train$c8)
region_data_train$c9 <- as.factor(region_data_train$c9)
region_data_train$c10 <- as.factor(region_data_train$c10)

region_data_cv$c2 <- as.factor(region_data_cv$c2)
region_data_cv$c3 <- as.factor(region_data_cv$c3)
region_data_cv$c4 <- as.factor(region_data_cv$c4)
region_data_cv$c5 <- as.factor(region_data_cv$c5)
region_data_cv$c6 <- as.factor(region_data_cv$c6)
region_data_cv$c7 <- as.factor(region_data_cv$c7)
region_data_cv$c8 <- as.factor(region_data_cv$c8)
region_data_cv$c9 <- as.factor(region_data_cv$c9)
region_data_cv$c10 <- as.factor(region_data_cv$c10)

i <- 1
## 2

    fit <- glm(data = region_data_train,
               wnvpresent ~ c2,
               family = 'binomial')
    fit_predict <- predict(object = fit,
                           newdata = region_data_cv,
                           type = 'response')
    plot_data[i,1] <- i+1    
    plot_data[i,2] <- auc(actual = region_data_cv$wnvpresent, predicted = fit_predict)

i <- i + 1

## 3

fit <- glm(data = region_data_train,
           wnvpresent ~ c3,
           family = 'binomial')
fit_predict <- predict(object = fit,
                       newdata = region_data_cv,
                       type = 'response')
plot_data[i,1] <- i+1    
plot_data[i,2] <- auc(actual = region_data_cv$wnvpresent, predicted = fit_predict)

i <- i + 1

## 4

fit <- glm(data = region_data_train,
           wnvpresent ~ c4,
           family = 'binomial')
fit_predict <- predict(object = fit,
                       newdata = region_data_cv,
                       type = 'response')
plot_data[i,1] <- i+1    
plot_data[i,2] <- auc(actual = region_data_cv$wnvpresent, predicted = fit_predict)

i <- i + 1

## 5

fit <- glm(data = region_data_train,
           wnvpresent ~ c5,
           family = 'binomial')
fit_predict <- predict(object = fit,
                       newdata = region_data_cv,
                       type = 'response')
plot_data[i,1] <- i+1    
plot_data[i,2] <- auc(actual = region_data_cv$wnvpresent, predicted = fit_predict)

i <- i + 1

## 6

fit <- glm(data = region_data_train,
           wnvpresent ~ c6,
           family = 'binomial')
fit_predict <- predict(object = fit,
                       newdata = region_data_cv,
                       type = 'response')
plot_data[i,1] <- i+1    
plot_data[i,2] <- auc(actual = region_data_cv$wnvpresent, predicted = fit_predict)

i <- i + 1

## 7

fit <- glm(data = region_data_train,
           wnvpresent ~ c7,
           family = 'binomial')
fit_predict <- predict(object = fit,
                       newdata = region_data_cv,
                       type = 'response')
plot_data[i,1] <- i+1    
plot_data[i,2] <- auc(actual = region_data_cv$wnvpresent, predicted = fit_predict)

i <- i + 1

## 8

fit <- glm(data = region_data_train,
           wnvpresent ~ c8,
           family = 'binomial')
fit_predict <- predict(object = fit,
                       newdata = region_data_cv,
                       type = 'response')
plot_data[i,1] <- i+1    
plot_data[i,2] <- auc(actual = region_data_cv$wnvpresent, predicted = fit_predict)

i <- i + 1

## 9

fit <- glm(data = region_data_train,
           wnvpresent ~ c9,
           family = 'binomial')
fit_predict <- predict(object = fit,
                       newdata = region_data_cv,
                       type = 'response')
plot_data[i,1] <- i+1    
plot_data[i,2] <- auc(actual = region_data_cv$wnvpresent, predicted = fit_predict)

i <- i + 1

## 10

fit <- glm(data = region_data_train,
           wnvpresent ~ c10,
           family = 'binomial')
fit_predict <- predict(object = fit,
                       newdata = region_data_cv,
                       type = 'response')
plot_data[i,1] <- i+1    
plot_data[i,2] <- auc(actual = region_data_cv$wnvpresent, predicted = fit_predict)

i <- i + 1



i
plot_data <- data.frame(plot_data)
colnames(plot_data) <- c("regions","auc")
plot(plot_data$regions,plot_data$auc)
plot_data
## 10 regions does really well, though that seems like a lot. 


## lags
lag1 <- glm(data=training_final,
            wnvpresent ~ range1 + temp1 + rain1 + dew1 + wet1,
            family = 'binomial')
cv_lag1 <- predict(object = lag1,
                   newdata= cv_final,
                   type = 'response')
auc(actual = cv_final$wnvpresent, predicted = cv_lag1)
# 0.6711348

lag7 <- glm(data=training_final,
            wnvpresent ~ temp7 + rain7 + dew7 + wet7,
            family = 'binomial')
summary(lag7)
cv_lag7 <- predict(object = lag7,
                   newdata= cv_final,
                   type = 'response')
auc(actual = cv_final$wnvpresent, predicted = cv_lag7)
# 0.7294016

lag14 <- glm(data=training_final,
            wnvpresent ~ temp14 + rain14 + dew14 + wet14,
            family = 'binomial')
summary(lag14)
cv_lag14 <- predict(object = lag14,
                   newdata= cv_final,
                   type = 'response')
auc(actual = cv_final$wnvpresent, predicted = cv_lag14)
# 0.7480152
lag30 <- glm(data=training_final,
             wnvpresent ~ temp30 + rain30 + dew30 + wet30,
             family = 'binomial')
summary(lag30)
cv_lag30 <- predict(object = lag30,
                    newdata= cv_final,
                    type = 'response')
auc(actual = cv_final$wnvpresent, predicted = cv_lag30)
#0.7645629




