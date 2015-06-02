setwd('/home/john/Kaggle/West Nile Virus Prediction/Data')

library(Metrics) #to calculate AUC of ROC curve
library(dplyr)
library(ggplot2)
library(glmnet) #for classification tree


training_final <- read.csv("training_final.csv", stringsAsFactors = FALSE, header = TRUE)
cv_final <- read.csv("cv_final.csv", stringsAsFactors = FALSE, header = TRUE)


colnames(training_final)
training_final <- select(training_final,-X,-x,-X.x,-X.1,-X.y,-year.x,-year.y)
colnames(cv_final)
cv_final <- select(cv_final,-X.2,-X.x,-year.x,-X.y,-X.1,-X,-year.y,-unique_id,-join)

training_full <- bind_rows(training_final,cv_final)

training_full$species2 <- as.factor(training_full$species2)
training_full$cut <- as.factor(training_full$cut)
training_full$c10 <- as.factor(training_full$c10)

write.csv(training_full,'training_full_final.csv',row.names = FALSE)
