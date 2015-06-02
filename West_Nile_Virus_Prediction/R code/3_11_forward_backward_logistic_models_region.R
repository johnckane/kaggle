## Let's try straightforward forward and backward selection ##
## We'll save each model (hopefully) then do CV on each to determine the best ## 
## model ##

setwd('/home/john/Kaggle/West Nile Virus Prediction/Data')

library(Metrics) #to calculate AUC of ROC curve
library(dplyr)
library(ggplot2)
library(bestglm) #to do forward and backward selection


training_final <- read.csv("training_final.csv", stringsAsFactors = FALSE, header = TRUE)
cv_final <- read.csv("cv_final.csv", stringsAsFactors = FALSE, header = TRUE)

training_final <- training_final %>%
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

str(training_final)
training_final$species2 <- as.factor(training_final$species2)
training_final$cut <- as.factor(training_final$cut)
training_final$c10 <- as.factor(training_final$c10)
cv_final <- cv_final %>%
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

cv_final$species2 <- as.factor(cv_final$species2)
cv_final$cut <- as.factor(cv_final$cut)
cv_final$c10 <- as.factor(cv_final$c10)
    

full <- glm(data = training_final,
            wnvpresent ~ .,
            family = 'binomial')
backward <- step(full)


null_model = glm(data=training_final,
                 wnvpresent~1,
                 family='binomial')
full_model <- formula(glm(wnvpresent~.,data=training_final, family = 'binomial'))
forward = step(null_model, direction='forward', scope=full_model)


backward_model <- glm(data=training_final,
                      wnvpresent ~ cut + species2 + c10 + avg_may_temp + num_may_rain +
                          avg_may_dew + avg_avg + temp30 + wet30 + range1,
                      family = 'binomial')
backward_model_predict <- predict(object = backward_model,
                                  newdata= cv_final,
                                  type = 'response')
auc(cv_final$wnvpresent,backward_model_predict)
# 0.7902341

forward_model <- glm(data=training_final,
                     wnvpresent ~ dew30 + avg_may_wet + c10 + species2 + avg_may_temp +
                         cut + avg_may_dew + avg_dew + range1,
                     family = 'binomial')
forward_model_predict <- predict(object = forward_model,
                                 newdata = cv_final,
                                 type = 'response')
auc(cv_final$wnvpresent,forward_model_predict)
#0.7900818, just lessthan backward

## Let's fit to the test data, then submit a file!! 
test_final <- read.csv("test_final.csv", header = TRUE, stringsAsFactors = FALSE)
test_final$c10 <- as.factor(test_final$c10)
test_final$cut <- as.factor(test_final$cut)

predict_forward <- predict(object = forward_model,
                           newdata = test_final,
                           type = 'response')
forward_submission <- data.frame(test_final$id,predict_forward)
head(forward_submission)
colnames(forward_submission) <- c("Id","WnvPresent")
rownames(forward_submission) <- NULL
predict_backward <- predict(object = backward_model,
                            newdata = test_final,
                            type = 'response')
backward_submission <- data.frame(test_final$id,predict_backward)
head(backward_submission)
colnames(backward_submission) <- c("Id","WnvPresent")
rownames(backward_submission) <- NULL

write.csv(forward_submission,"forward_submission.csv", row.names = FALSE)
write.csv(backward_submission,"backward_submission.csv", row.names = FALSE)


