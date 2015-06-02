setwd('/home/john/Kaggle/West Nile Virus Prediction/Data')
library(dplyr)
library(ggplot2)

training4 <- read.csv('training4.csv', stringsAsFactors = FALSE, header = TRUE)
colnames(training4) <- tolower(colnames(training4))
head(training4)

## Need to bring in other feature engineering data sets. 
may_weather <- read.csv('may_weather.csv', stringsAsFactors = FALSE, header = TRUE)
head(may_weather)
weather3 <- read.csv('weather3.csv', stringsAsFactors = FALSE, header = TRUE)
head(weather3)

?left_join
training5 <- left_join(training4,may_weather, by = 'year')
View(training5)

training6 <- left_join(training5, weather3, by = 'date')
View(training6)

training6$c2 <- as.factor(training6$c2)
training6$c3 <- as.factor(training6$c3)
training6$c4 <- as.factor(training6$c4)
training6$c5 <- as.factor(training6$c5)
training6$c6 <- as.factor(training6$c6)
training6$c7 <- as.factor(training6$c7)
training6$c8 <- as.factor(training6$c8)
training6$c9 <- as.factor(training6$c9)
training6$c10 <- as.factor(training6$c10)


## write the final training dataset
write.csv(training6, "training_final.csv")
