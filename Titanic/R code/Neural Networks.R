# Let's pull in all the training data, then do k-fold CV

require(neuralnet)
data <- read.csv("/home/john86/Kaggle/Titanic/Data/train.csv")
#need this function later

which.median <- function(x){
  which.min(abs(x-median(x)))
}

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







# Save the size of the dataset
N <- length(data$passengerid)
N10 <- floor(N/10)
# Generate the different test and CV datasets
cvdata <- list()
traindata <- list()
cvindex <- c()
for(i in 1:10){
  if(i == 1){
    cvdata[[i]] <- sample(c(1:N),N10,replace=FALSE)
    traindata[[i]] <- c(1:N)[-cvdata[[i]]]
    cvindex <- c(cvindex,cvdata[[i]])
  }  
  else if(i != 1){
    cvdata[[i]] <- sample(c(1:N)[-cvindex],N10,replace=FALSE)
    traindata[[i]] <- c(1:N)[-cvdata[[i]]]
    cvindex <- c(cvindex,cvdata[[i]])
  }
}

data.df <- data.frame(data)

h = c(5,8,10,12,15)
t = c(0.1,0.05,0.01)



data.df <- as.data.frame(model.matrix( 
  ~ survived + pclass + sex + embarked + alone + cabin_listed,  data = data 
))




nn_parms <- c()
for(j in 1:length(h)){
  for(k in 1:length(t)){
    error <- c()
    this.model.vals <- c()
    for(l in 1:10){
      #Set the lth training and cross-validation training sets
      train.df <- data.df[traindata[[l]],]
      cv.df   <- data.df[cvdata[[l]],]
      # train the nn, then fit to the cross validation set
      train <- neuralnet(survived ~ pclass + sexmale + embarkedC + embarkedQ + embarkedS +alone + cabin_listed,
                         train.df,hidden=h[j],threshold=t[k])
      cv    <- compute(train,cv.df[,3:9])
      #compute the error on the cv set
      error <- c(error,sum((cv$net.result-cv.df$survived)**2))
      # determine the threshold that results in the highest accuracy on the cv set
      threshold=seq(0.01,0.99,by=0.01)
      accuracy=rep(0,99)
      for(m in 1:99){
        prediction=rep(-1,length(cv.df$survived))
        for(n in 1:length(cv.df$survived)){
          if(cv$net.result[n] < threshold[m]){prediction[n]=0}
          if(cv$net.result[n] >= threshold[m]){prediction[n]=1}
        }
        accuracy[m] = sum(prediction==cv.df$survived)/length(prediction)
      }
      this.model.vals <- rbind(this.model.vals,c(threshold[which.max(accuracy)],max(accuracy)))
    }    
    nn_parms <- rbind(nn_parms,c(h[j],t[k],mean(error),median(this.model.vals[,1]),
                                this.model.vals[which.median(median(this.model.vals[,1])),2]))    
    print(nn_parms)
  }
}

nn_parms_df <- data.frame(cv_parms)
colnames(nn_parms_df) <- c("hidden layers","threshold","mean error","prediction threshold","max accuracy")

cv_parms_df[which.min(nn_parms_df[,3]),]