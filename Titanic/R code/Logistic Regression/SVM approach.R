# Fit a logistic model to ALL the data for training, just to see how it does

data <- read.csv("/home/john86/Kaggle/Titanic/Data/train.csv")

#Create new variables
colnames(data) <- tolower(colnames(data))
data$cabin_level <- substr(data$cabin,1,1)
for(i in 1:length(data$passengerid)){
  if(data$cabin_level[i] != ""){data$cabin_listed[i] <- 1}
  if(data$cabin_level[i] == ""){data$cabin_listed[i] <- 0}
  if(data$parch[i] == 0 & data$sibsp[i] == 0){data$alone[i] <- 1}
  if(data$parch[i] != 0 | data$sibsp[i] != 0){data$alone[i] <- 0}
}
colnames(data)

attach(data)


my.fit <- glm(data=data,as.factor(survived)~as.factor(alone)+sex+as.factor(pclass)+fare+ as.factor(cabin_listed) 
              +as.factor(alone)*sex + as.factor(alone)*as.factor(pclass) + sex*as.factor(pclass)+
                +as.factor(alone)*sex*as.factor(pclass) +embarked ,
              family=binomial,y=TRUE)
#Find the value the maximizes accuracy on the training set

threshold=seq(0.01,0.99,by=0.01)
accuracy=rep(0,99)
for(i in 1:99){
  prediction=rep(-1,length(data$survived))
  for(j in 1:length(data$survived)){
    if(my.fit$fitted.values[j] < threshold[i]){prediction[j]=0}
    if(my.fit$fitted.values[j] >= threshold[i]){prediction[j]=1}
  }
  accuracy[i] = sum(prediction==data$survived)/length(prediction)
}
plot(threshold,accuracy,type='l')
c(max(accuracy),threshold[which.max(accuracy)])
cbind(accuracy,threshold)


# Looks like a threshold of 0.44 does the trick, accuracy = 0.8125
y.predict = predict.glm(my.fit,test.submission,type='response')
y.predict[153] <- 0
prediction=rep(-1,length(test.submission$passengerid))
for(j in 1:length(test.submission$passengerid)){
  if(y.predict[j] < 0.44){prediction[j]=0}
  if(y.predict[j] >= 0.44){prediction[j]=1}
}

PassengerID <- test.submission$passengerid
Survived <- prediction
my.submission <- cbind(PassengerID,Survived)
write.csv(my.submission,"/home/john86/Kaggle/Titanic/Data/johnkane_submission7.csv",row.names=FALSE)






