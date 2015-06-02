setwd('/home/john/Kaggle/West Nile Virus Prediction/Data')

library(Metrics) #to calculate AUC of ROC curve
library(dplyr)
library(ggplot2)

training_final <- read.csv("training_final.csv", stringsAsFactors = FALSE, header = TRUE)
cv_final <- read.csv("cv_final.csv", stringsAsFactors = FALSE, header = TRUE)

colnames(training_final)
training_final <- training_final %>%
    select(c10,
           latitude,
           longitude,
           cut,
           species2,
           day_of_year,
           avg_may_temp,
           num_may_rain,
           avg_may_dew,
           avg_may_wet,
           avg_max,
           avg_min,
           avg_avg,
           avg_dew,
           avg_wet,
           wet_con,
           temp_range,
           temp1,
           temp7,
           temp14,
           temp30,
           rain1,
           rain7,
           rain14,
           rain30,
           dew1,
           dew7,
           dew14,
           dew30,
           wet1,
           wet7,
           wet14,
           wet30,
           range1,
           wnvpresent)
cv_final <- cv_final %>%
    select(c10,
           latitude,
           longitude,
           cut,
           species2,
           day_of_year,
           avg_may_temp,
           num_may_rain,
           avg_may_dew,
           avg_may_wet,
           avg_max,
           avg_min,
           avg_avg,
           avg_dew,
           avg_wet,
           wet_con,
           temp_range,
           temp1,
           temp7,
           temp14,
           temp30,
           rain1,
           rain7,
           rain14,
           rain30,
           dew1,
           dew7,
           dew14,
           dew30,
           wet1,
           wet7,
           wet14,
           wet30,
           range1,
           wnvpresent)

training_final$species2 <- as.factor(training_final$species2)
training_final$cut <- as.factor(training_final$cut)
training_final$c10 <- as.factor(training_final$c10)
cv_final$species2 <- as.factor(cv_final$species2)
cv_final$cut <- as.factor(cv_final$cut)
cv_final$c10 <- as.factor(cv_final$c10)
null_model = glm(data=training_final,
                 wnvpresent~1,
                 family='binomial')
full_model_sat <- formula(glm(wnvpresent~.,data=training_final, family = 'binomial'))
forward_sat = step(null_model, direction='forward', scope=full_model_sat)

forward_sat_final <- glm(data=training_final,
                         forward_sat$formula,
                         family = 'binomial')
predict_forward_sat <- predict(object = forward_sat_final,
                               newdata = cv_final,
                               type = 'response')
auc(cv_final$wnvpresent,predict_forward_sat)
#0.826

test_final <- read.csv("test_final.csv", stringsAsFactors = FALSE, header = TRUE)
test_final$c10 <- as.factor(test_final$c10)
test_final$cut <- as.factor(test_final$cut)
test_final$species2 <- as.factor(test_final$species2)

test_sat_predict <- predict(object = forward_sat_final,
                            newdata = test_final,
                            type = 'response')
forward_sat_submission <- data.frame(test_final$id,test_sat_predict)
colnames(forward_sat_submission) <- c("Id","WnvPresent")

write.csv(forward_sat_submission,"forward_sat_submission.csv",row.names = FALSE)
