prediction=rep(-1,length(test.submission$passengerid))
for(j in 1:length(test.submission$passengerid)){
  if(y.predict[j] < 0.62){prediction[j]=0}
  if(y.predict[j] >= 0.62){prediction[j]=1}
}

PassengerID <- test.submission$passengerid
Survived <- prediction
my.submission <- cbind(PassengerID,Survived)
write.csv(my.submission,"/home/john86/Kaggle/Titanic/Data/johnkane_submission2.csv",row.names=FALSE)


prediction=rep(-1,length(test.submission$passengerid))
for(j in 1:length(test.submission$passengerid)){
  if(y.predict[j] < 0.555){prediction[j]=0}
  if(y.predict[j] >= 0.555){prediction[j]=1}
}

PassengerID <- test.submission$passengerid
Survived <- prediction
my.submission <- cbind(PassengerID,Survived)
write.csv(my.submission,"/home/john86/Kaggle/Titanic/Data/johnkane_submission3.csv",row.names=FALSE)
