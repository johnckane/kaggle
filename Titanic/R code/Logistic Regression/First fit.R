# The logistic model

data <- read.csv("/home/john86/Kaggle/Titanic/Data/training_set.csv")

attach(data)


my.fit <- glm(data=data,as.factor(survived)~as.factor(alone)+sex+as.factor(pclass)+fare+ as.factor(cabin_listed) 
           +as.factor(alone)*sex + as.factor(alone)*as.factor(pclass) + sex*as.factor(pclass)+
             +as.factor(alone)*sex*as.factor(pclass) ,
           family=binomial,y=TRUE)
summary(my.fit)

#Measure accuracy on the training set
hist(my.fit$fitted.values)

threshold=seq(0.01,0.99,by=0.01)
accuracy=rep(0,99)
for(i in 1:99){
  prediction=rep(-1,length(survived))
  for(j in 1:length(survived)){
    if(my.fit$fitted.values[j] < threshold[i]){prediction[j]=0}
    if(my.fit$fitted.values[j] >= threshold[i]){prediction[j]=1}
    }
  accuracy[i] = sum(prediction==my.fit$y)/length(prediction)
  }
plot(threshold,accuracy)
threshold[which.max(accuracy)]
max(accuracy)
cbind(threshold,accuracy)


