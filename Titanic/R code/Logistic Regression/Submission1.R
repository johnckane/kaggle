test.submission <- read.csv("/home/john86/Kaggle/Titanic/Data/test_submission.csv")
#create new variables
colnames(test.submission) <- tolower(colnames(test.submission))
test.submission$cabin_level <- substr(test.submission$cabin,1,1)
test.submission$cabin_number <- as.numeric(substr(test.submission$cabin,2,4))
for(i in 1:length(test.submission$passengerid)){
  if(test.submission$cabin_level[i] != ""){test.submission$cabin_listed[i] <- 1}
  if(test.submission$cabin_level[i] == ""){test.submission$cabin_listed[i] <- 0}
}

for(i in 1:length(test.submission$passengerid)){
  if(test.submission$sibsp[i] == 1){test.submission$one_sibsp[i] <- 1}
  if(test.submission$sibsp[i] != 1){test.submission$one_sibsp[i] <- 0}
}

for(i in 1:length(test.submission$passengerid)){
  if(test.submission$parch[i] == 0 & test.submission$sibsp[i] == 0){test.submission$alone[i] <- 1}
  if(test.submission$parch[i] != 0 | test.submission$sibsp[i] != 0){test.submission$alone[i] <- 0}
}

y.predict <- predict.glm(my.fit,test.submission,type='response')
table(y.predict)
#set this value since it's a man in third class 
y.predict[153] <- 0


# calculate accuracy on test set

prediction=rep(-1,length(test.submission$passengerid))
for(j in 1:length(test.submission$passengerid)){
    if(y.predict[j] < 0.49){prediction[j]=0}
    if(y.predict[j] >= 0.49){prediction[j]=1}
  }

PassengerID <- test.submission$passengerid
Survived <- prediction
my.submission <- cbind(PassengerID,Survived)
write.csv(my.submission,"/home/john86/Kaggle/Titanic/Data/johnkane_submission1.csv",row.names=FALSE)
