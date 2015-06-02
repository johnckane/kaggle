setwd('/home/john/Kaggle/West Nile Virus Prediction/Data')
library(dplyr)
library(ggplot2)

training2 <- read.csv('training2.csv', stringsAsFactors = FALSE, header = TRUE)
colnames(training2) <- tolower(colnames(training2))
head(training2)

table(training2$species)
prop.table(table(training2$species))

## There are 3 groups that account for almost 96% of all training species. 
## It may makes sense to collapse all other groups.

## Let's see how they differ in terms of testing positve for wnv.

table(training2$wnvpresent,training2$species)
prop.table(table(training2$wnvpresent,training2$species),2)

## "Culex Pipiens" definitely test higher. The "Other groups aren't on record as having
## tested positive, even once. 

## Let's collpase those other groups. 
other <- c("CULEX ERRATICUS", "CULEX SALINARIUS", "CULEX TARSALIS", "CULEX TERRITANS")

training3 <- training2

training3$species2 <- ifelse(training3$species %in% other,
                             "OTHER",
                             training3$species)
table(training3$species, training3$species2)

write.csv(training3,"training3.csv")
