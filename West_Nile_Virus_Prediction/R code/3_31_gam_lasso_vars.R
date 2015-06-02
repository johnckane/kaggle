setwd('/home/john/Kaggle/West Nile Virus Prediction/Data')
library(dplyr)
library(gam) #for gam models
library(Metrics) #for auc

training_final <- read.csv("training_final.csv", stringsAsFactors = FALSE, header = TRUE)
cv_final <- read.csv("cv_final.csv", stringsAsFactors = FALSE, header = TRUE)

training_final$cut <- as.factor(training_final$cut)
training_final$species2 <- as.factor(training_final$species2)

cv_final$cut <- as.factor(cv_final$cut)
cv_final$species2 <- as.factor(cv_final$species2)

gam_fit = gam(as.factor(wnvpresent)~ cut + species2 + s(longitude,df=5) + 
                  s(avg_may_dew,df=5) + s(avg_may_wet,df=5) + s(dew30, df=5),
              family = binomial , 
              data = training_final)
plot(gam_fit, se = TRUE , col =" green ")

predict_cv <- predict(object = gam_fit,
                      newdata = cv_final,
                      type = 'response')
auc(cv_final$wnvpresent,predict_cv)
#0.83, the highest yet!! 

## Now for the test data
test_final <- read.csv("test_final.csv",stringsAsFactors = FALSE, header = TRUE)
test_final$cut <- as.factor(test_final$cut)
test_final$species2 <- as.factor(test_final$species2)

test_predict <- predict(object = gam_fit,
                        newdata = test_final,
                        type = 'response')
gam_submission <- data.frame(test_final$id, test_predict)
colnames(gam_submission) <- c("Id","WnvPresent")
write.csv(gam_submission, "gam_submission.csv", row.names = FALSE)
