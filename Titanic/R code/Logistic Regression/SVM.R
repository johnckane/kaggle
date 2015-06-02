require(kernlab)

# Let's pull in all the training data, then do k-fold CV

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


# Write code for cross validation using a Gaussian kernal
# Let's consider values of C and sigma in {0.01, 0.05, 0.10, 0.50, 1, 5, 10, 50, 100, 500}
C = c(0.01, 0.05, 0.10, 0.50, 1, 5, 10, 50, 100, 500)
sigma = c(0.01, 0.05, 0.10, 0.50, 1, 5, 10, 50, 100, 500)

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


# Now that the test and CV data has been determined, let's write the loop to fit, test and
# cross validate the data for each value of sigma,C and for each dataset


X_dat = cbind(data$pclass,data$sex,data$embarked,data$alone,data$cabin_listed)
Y_dat = data$survived
cv_parms <- c()
for(j in 1:length(C)){
  for(k in 1:length(sigma)){
    cv_acc = c()
    for(l in 1:10){
      train <- ksvm(X_dat[c(traindata[[l]]),],Y_dat[c(traindata[[l]])],type="C-svc",
                    kernel='rbf',kpar=list(sigma=sigma[k]),C=C[j])
      cv.pred <- predict(train,X_dat[c(cvdata[[l]]),])
      cv_acc <- c(cv_acc,sum(cv.pred==Y_dat[c(cvdata[[l]])])/length(cv.pred))
    }    
    cv_parms <- rbind(cv_parms,c(C[j],sigma[k],mean(cv_acc)))    
    print(cv_parms)
  }
}

cv_parms_df <- data.frame(cv_parms)
colnames(cv_parms_df) <- c("c","sigma","accuracy")

which.max(cv_parms_df$accuracy)
cv_parms_df[72,]

#Let's narrow in on those values
C = c(45,46,47,48,49,50,51,52,53,54,55)
sigma = c(0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.10)

cv_parms <- c()
for(j in 1:length(C)){
  for(k in 1:length(sigma)){
    cv_acc = c()
    for(l in 1:10){
      train <- ksvm(X_dat[c(traindata[[l]]),],Y_dat[c(traindata[[l]])],type="C-svc",
                    kernel='rbf',kpar=list(sigma=sigma[k]),C=C[j])
      cv.pred <- predict(train,X_dat[c(cvdata[[l]]),])
      cv_acc <- c(cv_acc,sum(cv.pred==Y_dat[c(cvdata[[l]])])/length(cv.pred))
    }    
    cv_parms <- rbind(cv_parms,c(C[j],sigma[k],mean(cv_acc)))    
    print(c(j,k))
  }
}

cv_parms_df <- data.frame(cv_parms)
colnames(cv_parms_df) <- c("c","sigma","accuracy")

which.max(cv_parms_df$accuracy)
cv_parms_df[4,]

C = seq(from=43.5,to=44.5,by=0.1)
sigma = seq(from=0.035,to=0.045,by=0.001)
cv_parms <- c()
for(j in 1:length(C)){
  for(k in 1:length(sigma)){
    cv_acc = c()
    for(l in 1:10){
      train <- ksvm(X_dat[c(traindata[[l]]),],Y_dat[c(traindata[[l]])],type="C-svc",
                    kernel='rbf',kpar=list(sigma=sigma[k]),C=C[j])
      cv.pred <- predict(train,X_dat[c(cvdata[[l]]),])
      cv_acc <- c(cv_acc,sum(cv.pred==Y_dat[c(cvdata[[l]])])/length(cv.pred))
    }    
    cv_parms <- rbind(cv_parms,c(C[j],sigma[k],mean(cv_acc),sd(cv_acc)))    
    print(cv_acc)
    print(c(j,k))
  }
}

cv_parms_df <- data.frame(cv_parms)
colnames(cv_parms_df) <- c("c","sigma","accuracy")

which.max(cv_parms_df$accuracy)
cv_parms_df[5,]
 
#Let's fit a SVM with C = 44, sigma = 0.04

model <- ksvm(X_dat,Y_dat,type="C-svc",
              kernel='rbf',kpar=list(sigma=0.04),C=44)

# fit this to the test data
test.submission <- read.csv("/home/john86/Kaggle/Titanic/Data/test_submission.csv")

colnames(test.submission) <- tolower(colnames(test.submission))
test.submission$cabin_level <- substr(test.submission$cabin,1,1)
test.submission$cabin_number <- as.numeric(substr(test.submission$cabin,2,4))
for(i in 1:length(test.submission$passengerid)){
  if(test.submission$cabin_level[i] != ""){test.submission$cabin_listed[i] <- 1}
  if(test.submission$cabin_level[i] == ""){test.submission$cabin_listed[i] <- 0}
  if(test.submission$parch[i] == 0 & test.submission$sibsp[i] == 0){test.submission$alone[i] <- 1}
  if(test.submission$parch[i] != 0 | test.submission$sibsp[i] != 0){test.submission$alone[i] <- 0}
}

submit_X <- cbind(test.submission$pclass,test.submission$sex,test.submission$embarked,test.submission$alone,test.submission$cabin_listed)

submit_pred <- predict(model,submit_X)

PassengerID <- test.submission$passengerid
Survived <- submit_pred
my.submission <- cbind(PassengerID,Survived)
write.csv(my.submission,"/home/john86/Kaggle/Titanic/Data/johnkane_submission6.csv",row.names=FALSE)

  