setwd('/home/john/Kaggle/West_Nile_Virus_Prediction/Data')
library(dplyr)
library(caret)
set.seed(145)

train <- read.csv('train.csv', stringsAsFactors = FALSE, header = TRUE)
colnames(train) <- tolower(colnames(train))
train$date <- as.Date(train$date)
train$year <- as.numeric(format(train$date, "%Y"))
train$month <- format(train$date, "%b")
train$month_day <- format(train$date, "%m %d")
train$day_of_year <- as.numeric(format(train$date, "%j"))


train_indices <- createDataPartition(train$wnvpresent,
                                     p = 0.67,
                                     list = FALSE)
training <- train[train_indices,]
cv <- train[-train_indices,]

# quick compare similarity of training data and full train dataset
colnames(train)

prop.table(table(train$wnvpresent))
prop.table(table(training$wnvpresent))

summary(train$day_of_year)
summary(training$day_of_year)

prop.table(table(train$month))
prop.table(table(training$month))

prop.table(table(train$year))
prop.table(table(training$year))

prop.table(table(train$species))
prop.table(table(training$species))

cbind(prop.table(table(train$block))-
prop.table(table(training$block)))

prop.table(table(train$addressaccuracy))
prop.table(table(training$addressaccuracy))

cbind(prop.table(table(train$trap))-
prop.table(table(training$trap)))


## save files for later use
write.csv(training,"training.csv")
write.csv(cv,"cv.csv")
