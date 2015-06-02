setwd('/home/john/Kaggle/West_Nile_Virus_Prediction/Data')

library(Metrics) #to calculate AUC of ROC curve
library(dplyr)
library(ggplot2)
library(tree) #for classification tree


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


tree1 <- tree(as.factor(wnvpresent) ~ .,
               data = training_final)
summary(tree1)
plot(tree1)
text(tree1, pretty =0)
tree1

## It doesn't appear that any pruning is necessary here, only 4 variables make the cut

tree_predict <- predict(object = tree1,
                    newdata = cv_final,
                    type = 'vector')
head(tree_predict)

auc(cv_final$wnvpresent,tree_predict[,2])
# Gives 0.77. 
# let's apply it to the test data

test_final <- read.csv("test_final.csv", stringsAsFactors = FALSE, header = TRUE)
test_final$c10 <- as.factor(test_final$c10)
test_final$cut <- as.factor(test_final$cut)
test_final$species2 <- as.factor(test_final$species2)

predict_test <- predict(object = tree1,
                        newdata = test_final,
                        type = 'vector')

tree_submission <- data.frame(test_final$id, predict_test[,2])
head(tree_submission)
colnames(tree_submission) <- c("Id","WnvPresent")
write.csv(tree_submission, 'tree_submission.csv',row.names = FALSE)


## Let's try a tree using lat and lon rather than region
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
tree2 <- tree(as.factor(wnvpresent) ~ .,
              data = training_final)
summary(tree2)
plot(tree2)
text(tree2, pretty =0)
tree2

## It doesn't appear that any pruning is necessary here, only 4 variables make the cut

tree_predict2 <- predict(object = tree2,
                        newdata = cv_final,
                        type = 'vector')
head(tree_predict2)

auc(cv_final$wnvpresent,tree_predict2[,2])
# Gives 0.796
# let's apply it to the test data

test_final <- read.csv("test_final.csv", stringsAsFactors = FALSE, header = TRUE)
test_final$cut <- as.factor(test_final$cut)
test_final$species2 <- as.factor(test_final$species2)

predict_test2 <- predict(object = tree2,
                        newdata = test_final,
                        type = 'vector')

tree_submission2 <- data.frame(test_final$id, predict_test2[,2])
head(tree_submission2)
colnames(tree_submission2) <- c("Id","WnvPresent")
write.csv(tree_submission2, 'tree_submission2.csv',row.names = FALSE)
