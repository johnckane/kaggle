setwd('/home/john/Kaggle/West_Nile_Virus_Prediction/Data')

library(dplyr)
library(zoo) # for the "rollmean" function

weather2 <- read.csv("weather2.csv",stringsAsFactors = FALSE, header = TRUE)
str(weather2)
weather2$date <- as.Date(weather2$date)
str(weather2)
weather2$year <- format(weather2$date, "%Y")
table(weather2$year)

## Create range variable
weather2$temp_range <- weather2$avg_max - weather2$avg_min


## need to be careful because the end of one year runs right into the next
## will implement the "split-apply-combine" technique with the dplyr package.

## Will also need my own custom function to test if there are enough data points to 
## do a full 7, 14 and 30 day lag. My idea is to take an oberservation and length of 
## rolling average desired, test it's place in the list, then compute the appropriate
## rolling average length. The 10th day of the year won't have a proper 14 and 30 day
## rolling average available so I'll use instead 10 day RA's for each. Not perfect but
## better than not being able to use that data to train the data

## quick test using the which function
which_test <- weather2 %>%
    arrange(date) %>%
    group_by(year) %>%
    mutate(obs = which(date == date))
which_test <- select(which_test,date, year, obs)
View(which_test)
## It works! So let's add that "obs" variable to the dataset

weather3 <- weather2 %>%
    arrange(date) %>%
    group_by(year) %>%
    mutate(obs = which(date == date))

f_avg <- function(obs_num, length, var){
    value <- ifelse(obs_num >= length,
                    rollmean(var[(obs_num-length+1):obs_num],length),
                    rollmean(var[1:obs_num],obs_num)
                    )
    return(value)
}

f_sum <-  function(obs_num, length, var){
    value <- ifelse(obs_num >= length,
                    sum(var[(obs_num-length+1):obs_num]),
                    sum(var[1:obs_num])
    )    
    return(value)
}

f_lag <- function(obs_num,var){
    value <- ifelse(obs_num > 1,
                    var[obs_num-1],
                    var[1]
    )
    return(value)
}

## test the functions
s1 <- seq(from = 1, to = 42, by = 1)
s2 <- sample(c(0,1),57,replace=TRUE,p = c(0.5,0.5))
s3 <- seq(from=100, to = 135, by = 1) 
    
f_avg(35,14,s1)
f_avg(4,7,s1)
f_avg(4,14,s1)
f_avg(4,30,s1)

f_sum(41,7,s2)
f_sum(4,7,s2)
f_sum(4,14,s2)
f_sum(4,30,s2)

f_lag(1,s3)
f_lag(3,s3)
## it check's out!!

## this isn't working
#############################################################
#weather4 <- weather3 %>%
#   arrange(date) %>%
#   group_by(year) %>%
#   mutate(temp7  = f_avg(weather2$obs, 7,  weather2$avg_avg),
#          temp14 = f_avg(weather2$obs, 14, weather2$avg_avg),
#          temp30 = f_avg(weather2$obs, 30, weather2$avg_avg),
#          rain7  = f_sum(weather2$obs, 7,  weather2$wet_con),
#          rain14 = f_sum(weather2$obs, 14, weather2$wet_con),
#          rain30 = f_sum(weather2$obs, 30, weather2$wet_con),
#          dew7   = f_avg(weather2$obs, 7,  weather2$dew_avg),
#          dew14  = f_avg(weather2$obs, 14, weather2$dew_avg),
#          dew30  = f_avg(weather2$obs, 30, weather2$dew_avg),
#          wet7   = f_avg(weather2$obs, 7,  weather2$wet_avg),
#          wet14  = f_avg(weather2$obs, 14, weather2$wet_avg),
#          wet30  = f_avg(weather2$obs, 30, weather2$wet_avg))
##############################################################

## try it in a for-loop, not efficient but it should work
## first create the variables
weather3$temp1 <- rep(0,dim(weather3)[1])
weather3$temp7 <- rep(0,dim(weather3)[1])
weather3$temp14 <- rep(0,dim(weather3)[1])
weather3$temp30 <- rep(0,dim(weather3)[1])
weather3$rain1 <- rep(0,dim(weather3)[1])
weather3$rain7 <- rep(0,dim(weather3)[1])
weather3$rain14 <- rep(0,dim(weather3)[1])
weather3$rain30 <- rep(0,dim(weather3)[1])
weather3$dew1 <- rep(0,dim(weather3)[1])
weather3$dew7 <- rep(0,dim(weather3)[1])
weather3$dew14 <- rep(0,dim(weather3)[1])
weather3$dew30 <- rep(0,dim(weather3)[1])
weather3$wet1 <- rep(0,dim(weather3)[1])
weather3$wet7 <- rep(0,dim(weather3)[1])
weather3$wet14 <- rep(0,dim(weather3)[1])
weather3$wet30 <- rep(0,dim(weather3)[1])
weather3$range1 <- rep(0,dim(weather3)[1])


for(i in 1:dim(weather3)[1]){
    weather3$temp1[i]  = f_avg(weather3$obs[i], 1,  weather3$avg_avg)
    weather3$temp7[i]  = f_avg(weather3$obs[i], 7,  weather3$avg_avg)
    weather3$temp14[i] = f_avg(weather3$obs[i], 14, weather3$avg_avg)
    weather3$temp30[i] = f_avg(weather3$obs[i], 30, weather3$avg_avg)
    weather3$rain1[i]  = f_sum(weather3$obs[i], 1,  weather3$wet_con)
    weather3$rain7[i]  = f_sum(weather3$obs[i], 7,  weather3$wet_con)
    weather3$rain14[i] = f_sum(weather3$obs[i], 14, weather3$wet_con)
    weather3$rain30[i] = f_sum(weather3$obs[i], 30, weather3$wet_con)
    weather3$dew1[i]   = f_avg(weather3$obs[i], 1,  weather3$avg_dew)
    weather3$dew7[i]   = f_avg(weather3$obs[i], 7,  weather3$avg_dew)
    weather3$dew14[i]  = f_avg(weather3$obs[i], 14, weather3$avg_dew)
    weather3$dew30[i]  = f_avg(weather3$obs[i], 30, weather3$avg_dew)
    weather3$wet1[i]   = f_avg(weather3$obs[i], 1,  weather3$avg_wet)
    weather3$wet7[i]   = f_avg(weather3$obs[i], 7,  weather3$avg_wet)
    weather3$wet14[i]  = f_avg(weather3$obs[i], 14, weather3$avg_wet)
    weather3$wet30[i]  = f_avg(weather3$obs[i], 30, weather3$avg_wet)
    weather3$range1[i] = f_lag(weather3$obs[i], weather3$temp_range)
}

View(weather3)


write.csv(weather3,"weather3.csv")

## Examine correlations
library(GGally)
library(ggplot2)
cor(temp)
temp <- data.frame(weather3$temp1,weather3$temp7,weather3$temp14,weather3$temp30)
cor(temp)
temp_plot <- ggpairs(temp, title = "Temperature Plot", 
                     diag = list(continuous = "bar"),
                     params = c(binwidth = 1),
                     columnLabels = c("1 Day","7 Day","14 Day","30 Day"))

rain <- data.frame(weather3$rain1,weather3$rain7,weather3$rain14,weather3$rain30)
cor(rain)
rain_plot <- ggpairs(rain, title = "Days with Rain Plot", 
                     diag = list(continuous = "bar"),
                     params = c(binwidth = 1),
                     columnLabels = c("1 Day","7 Day","14 Day","30 Day"))

dew <- data.frame(weather3$dew1,weather3$dew7,weather3$dew14,weather3$dew30)
cor(dew)
dew_plot <- ggpairs(dew, title = "Dewpoint Plot", 
                    diag = list(continuous = "bar"),
                    params = c(binwidth = 1),
                    columnLabels = c("1 Day","7 Day","14 Day","30 Day"))

wet <- data.frame(weather3$wet1,weather3$wet7,weather3$wet14,weather3$wet30)
cor(wet)
wet_plot <- ggpairs(wet, title = "Wetbulb Plot", 
                    diag = list(continuous = "bar"),
                    params = c(binwidth = 1),
                    columnLabels = c("1 Day","7 Day","14 Day","30 Day"))
wet_plot
library(gridExtra)
grid.arrange(temp_plot,
             rain_plot,
             dew_plot,
             wet_plot,
             ncol = 2)
