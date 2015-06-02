library(ggplot2)

data <- read.csv("/home/john86/Kaggle/Titanic/Data/train.csv")
colnames(data) <- tolower(colnames(train))
data$cabin_level <- substr(data$cabin,1,1)
data$cabin_number <- as.numeric(substr(data$cabin,2,4))
for(i in 1:length(data$survived)){
  if(data$cabin_level[i] != ""){data$cabin_listed[i] <- 1}
  if(data$cabin_level[i] == ""){data$cabin_listed[i] <- 0}
}


# Split data into 80%/20% for training and testing 
train_obs <- sample(891,0.8*891,replace=FALSE)
test_obs  <- c(1:891)[-train_obs]

train_data <- data[c(train_obs),]
test_data  <- data[c(test_obs),]


#Compare training data to test data
table(train_data$survived)/712
table(train_data$pclass)/712
summary(train_data$sex)/712
summary(train_data$age)
table(train_data$sibsp)/712
table(train_data$parch)/712
summary(train_data$fare)
table(train_data$cabin_level)/712

table(test_data$survived)/179
table(test_data$pclass)/179
summary(test_data$sex)/179
summary(test_data$age)
table(test_data$sibsp)/179
table(test_data$parch)/179
summary(test_data$fare)
table(test_data$cabin_level)/179

write.csv(train_data,"/home/john86/Kaggle/Titanic/Data/training_set.csv")
write.csv(test_data,"/home/john86/Kaggle/Titanic/Data/test_set.csv")








