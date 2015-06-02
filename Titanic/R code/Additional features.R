data <- read.csv("/home/john86/Kaggle/Titanic/Data/train.csv")

colnames(data) <- tolower(colnames(data))
data$cabin_level <- substr(data$cabin,1,1)
for(i in 1:length(data$passengerid)){
  data$cabin_listed[i] <- ifelse(data$cabin_level[i] != "",1,0)
  if(data$parch[i] == 0 & data$sibsp[i] == 0){data$alone[i] <- 1}
  if(data$parch[i] != 0 | data$sibsp[i] != 0){data$alone[i] <- 0}
  data$title[i] <- substr(strsplit(as.character(data$name),",")[[i]][2],1,4)
}
data$mr <-  ifelse(data$title == " Mr.",1,0)
data$mas <- ifelse(data$title == " Mas",1,0)
data$mis <- ifelse(data$title == " Mis",1,0)
data$mrs <- ifelse(data$title == " Mrs",1,0)


colnames(data)


missing.age <- data[data$age ==  'NA',]
age.available <- data[is.na(data$age) == FALSE ,]

fit1 <- lm(data=age.available,age~pclass+sex+alone+fare+embarked+cabin_listed+mr+mas+mis+mrs)
summary(fit1)

(pr <- resid(fit1)/(1 - lm.influence(fit1)$hat))


PRESS.statistic <- sum( (resid(fit1)/(1-hatvalues(fit1)))^2 )
print(paste("PRESS statistic= ", PRESS.statistic))
