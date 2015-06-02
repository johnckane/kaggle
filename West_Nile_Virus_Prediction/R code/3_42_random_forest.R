setwd('/home/john/Kaggle/West_Nile_Virus_Prediction/Data')

library(Metrics) #to calculate AUC of ROC curve
library(dplyr)
library(ggplot2)
library(randomForest) #for classification tree


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


set.seed(22)

rf <- randomForest(as.factor(wnvpresent) ~.,
                    data = training_final,
                    importance = TRUE )
importance(rf)

rf_predict <- predict(object = rf,
                        newdata = cv_final,
                        type = 'prob')
head(rf_predict)

auc(cv_final$wnvpresent,rf_predict[,2])
# 0.6677036

## Let's try it on the test data
test_final <- read.csv("test_final.csv", stringsAsFactors = FALSE, header = TRUE)
test_final$c10 <- as.factor(test_final$c10)
test_final$cut <- as.factor(test_final$cut)
test_final$species2 <- as.factor(test_final$species2)

predict_test <- predict(object = rf,
                        newdata = test_final,
                        type = 'prob')

rf_submission <- data.frame(test_final$id, predict_test[,2])
head(rf_submission)
colnames(rf_submission) <- c("Id","WnvPresent")
hist(rf_submission$WnvPresent)
write.csv(rf_submission, 'rf_submission.csv',row.names = FALSE)


### Now a random forest using lat/lon rather than region
training_final <- read.csv("training_final.csv", stringsAsFactors = FALSE, header = TRUE)
cv_final <- read.csv("cv_final.csv", stringsAsFactors = FALSE, header = TRUE)

training_final <- training_final %>%
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

str(training_final)
training_final$species2 <- as.factor(training_final$species2)
training_final$cut <- as.factor(training_final$cut)

cv_final <- cv_final %>%
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

cv_final$species2 <- as.factor(cv_final$species2)
cv_final$cut <- as.factor(cv_final$cut)


set.seed(28)

rf2 <- randomForest(as.factor(wnvpresent) ~.,
                   data = training_final,
                   importance = TRUE )

rf_predict2 <- predict(object = rf2,
                      newdata = cv_final,
                      type = 'prob')
head(rf_predict2)

auc(cv_final$wnvpresent,rf_predict2[,2])
# 0.76

## Let's try it on the test data
test_final <- read.csv("test_final.csv", stringsAsFactors = FALSE, header = TRUE)
test_final$cut <- as.factor(test_final$cut)
test_final$species2 <- as.factor(test_final$species2)

predict_test <- predict(object = rf2,
                        newdata = test_final,
                        type = 'prob')

rf_submission2 <- data.frame(test_final$id, predict_test[,2])
head(rf_submission2)
colnames(rf_submission2) <- c("Id","WnvPresent")
hist(rf_submission2$WnvPresent)
write.csv(rf_submission2, 'rf_submission2.csv',row.names = FALSE)
