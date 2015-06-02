# bring in the test data 
data.test <- read.csv("/home/john86/Kaggle/Titanic/Data/test_set.csv")
# Create the new variables
colnames(data.test) <- tolower(colnames(data.test))
data.test$cabin_level <- substr(data.test$cabin,1,1)
data.test$cabin_number <- as.numeric(substr(data.test$cabin,2,4))
for(i in 1:length(data.test$survived)){
  if(data.test$cabin_level[i] != ""){data.test$cabin_listed[i] <- 1}
  if(data.test$cabin_level[i] == ""){data.test$cabin_listed[i] <- 0}
}

for(i in 1:length(data.test$survived)){
  if(data.test$sibsp[i] == 1){data.test$one_sibsp[i] <- 1}
  if(data.test$sibsp[i] != 1){data.test$one_sibsp[i] <- 0}
}

for(i in 1:length(data.test$survived)){
  if(data.test$parch[i] == 0 & data.test$sibsp[i] == 0){data.test$alone[i] <- 1}
  if(data.test$parch[i] != 0 | data.test$sibsp[i] != 0){data.test$alone[i] <- 0}
}

y.predict <- predict.glm(my.fit,data.test,type='response')

# calculate accuracy on test set
threshold=seq(0.01,0.99,by=0.01)
accuracy=rep(0,99)
for(i in 1:99){
  prediction=rep(-1,length(data.test$survived))
  for(j in 1:length(data.test$survived)){
    if(y.predict[j] < threshold[i]){prediction[j]=0}
    if(y.predict[j] >= threshold[i]){prediction[j]=1}
  }
  accuracy[i] = sum(prediction==data.test$survived)/length(prediction)
}
plot(threshold,accuracy)
c(max(accuracy),threshold[which.max(accuracy)])

cbind(threshold,accuracy)
#Threshold of 49% - 62% maxes accuracy at 82.12291%