setwd('/home/john/Kaggle/West_Nile_Virus_Prediction/Data')
library(Metrics) #to calculate AUC of ROC curve
library(dplyr)
library(ggplot2)
library(bestglm)

## Need to write my own function
## I'll be using code I found here:
## https://ryouready.wordpress.com/2009/02/06/r-calculating-all-possible-linear-regression-models-for-a-given-set-of-predictors/
##
## There are over 1000 combinations to use all variables, including the ones you can only 
## use once

## Just to see, I want to know how well a "saturated" model would do
sat_fit <- glm(data=training_final,
               wnvpresent ~ cut + species2 + as.factor(c10) + avg_may_temp + num_may_rain +
                   avg_may_dew + avg_may_wet + avg_avg + avg_min + avg_dew + avg_max +
                   avg_wet + wet_con + temp_range + temp1 + temp7 + temp14 + temp30 +
                   rain1 + rain7 + rain14 + rain30 + dew1 + dew7 + dew14 + dew30 +
                   wet1 + wet7 + wet14+ wet30 + range1,
               family = 'binomial')
sat_predict <- predict(object = sat_fit,
                       newdata = cv_final,
                       type = 'response')
auc(actual = cv_final$wnvpresent,
    predicted = sat_predict)

## Let's find the best temp, rain, dew and wet lagged variables in the univarite case
## First up temp
temp1_fit <- glm(data = training_final,
             wnvpresent ~ temp1,
             family = 'binomial')

temp1_predict <- predict(object = temp1_fit,
                         newdata = cv_final,
                         type = 'response')
auc(actual = cv_final$wnvpresent,
    predicted = temp1_predict)
#0.56
temp7_fit <- glm(data = training_final,
                 wnvpresent ~ temp7,
                 family = 'binomial')

temp7_predict <- predict(object = temp7_fit,
                         newdata = cv_final,
                         type = 'response')
auc(actual = cv_final$wnvpresent,
    predicted = temp7_predict)
#0.54
temp14_fit <- glm(data = training_final,
                 wnvpresent ~ temp14,
                 family = 'binomial')

temp14_predict <- predict(object = temp14_fit,
                         newdata = cv_final,
                         type = 'response')
auc(actual = cv_final$wnvpresent,
    predicted = temp14_predict)
#0.62
temp30_fit <- glm(data = training_final,
                  wnvpresent ~ temp30,
                  family = 'binomial')

temp30_predict <- predict(object = temp30_fit,
                          newdata = cv_final,
                          type = 'response')
auc(actual = cv_final$wnvpresent,
    predicted = temp30_predict)
#0.72, wow

## Now rain
rain1_fit <- glm(data = training_final,
                 wnvpresent ~ rain1,
                 family = 'binomial')

rain1_predict <- predict(object = rain1_fit,
                         newdata = cv_final,
                         type = 'response')
auc(actual = cv_final$wnvpresent,
    predicted = rain1_predict)
#0.55
rain7_fit <- glm(data = training_final,
                 wnvpresent ~ rain7,
                 family = 'binomial')

rain7_predict <- predict(object = rain7_fit,
                         newdata = cv_final,
                         type = 'response')
auc(actual = cv_final$wnvpresent,
    predicted = rain7_predict)
#0.60
rain14_fit <- glm(data = training_final,
                  wnvpresent ~ rain14,
                  family = 'binomial')

rain14_predict <- predict(object = rain14_fit,
                          newdata = cv_final,
                          type = 'response')
auc(actual = cv_final$wnvpresent,
    predicted = rain14_predict)
#0.62
rain30_fit <- glm(data = training_final,
                  wnvpresent ~ rain30,
                  family = 'binomial')

rain30_predict <- predict(object = rain30_fit,
                          newdata = cv_final,
                          type = 'response')
auc(actual = cv_final$wnvpresent,
    predicted = rain30_predict)
#0.61
## Rain 14 actual does the best here. 

## Dew points!!
dew1_fit <- glm(data = training_final,
                 wnvpresent ~ dew1,
                 family = 'binomial')

dew1_predict <- predict(object = dew1_fit,
                         newdata = cv_final,
                         type = 'response')
auc(actual = cv_final$wnvpresent,
    predicted = dew1_predict)
#0.65
dew7_fit <- glm(data = training_final,
                 wnvpresent ~ dew7,
                 family = 'binomial')

dew7_predict <- predict(object = dew7_fit,
                         newdata = cv_final,
                         type = 'response')
auc(actual = cv_final$wnvpresent,
    predicted = dew7_predict)
#0.68
dew14_fit <- glm(data = training_final,
                  wnvpresent ~ dew14,
                  family = 'binomial')

dew14_predict <- predict(object = dew14_fit,
                          newdata = cv_final,
                          type = 'response')
auc(actual = cv_final$wnvpresent,
    predicted = dew14_predict)
#0.72
dew30_fit <- glm(data = training_final,
                  wnvpresent ~ dew30,
                  family = 'binomial')

dew30_predict <- predict(object = dew30_fit,
                          newdata = cv_final,
                          type = 'response')
auc(actual = cv_final$wnvpresent,
    predicted = dew30_predict)
#0.76 !!

## wet bulb
wet1_fit <- glm(data = training_final,
                wnvpresent ~ wet1,
                family = 'binomial')

wet1_predict <- predict(object = wet1_fit,
                        newdata = cv_final,
                        type = 'response')
auc(actual = cv_final$wnvpresent,
    predicted = wet1_predict)
#0.63
wet7_fit <- glm(data = training_final,
                wnvpresent ~ wet7,
                family = 'binomial')

wet7_predict <- predict(object = wet7_fit,
                        newdata = cv_final,
                        type = 'response')
auc(actual = cv_final$wnvpresent,
    predicted = wet7_predict)
#0.64
wet14_fit <- glm(data = training_final,
                 wnvpresent ~ wet14,
                 family = 'binomial')

wet14_predict <- predict(object = wet14_fit,
                         newdata = cv_final,
                         type = 'response')
auc(actual = cv_final$wnvpresent,
    predicted = wet14_predict)
#0.71
wet30_fit <- glm(data = training_final,
                 wnvpresent ~ wet30,
                 family = 'binomial')

wet30_predict <- predict(object = wet30_fit,
                         newdata = cv_final,
                         type = 'response')
auc(actual = cv_final$wnvpresent,
    predicted = wet30_predict)
#0.75 !!