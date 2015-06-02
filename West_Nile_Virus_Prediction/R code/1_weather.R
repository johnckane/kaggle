setwd('/home/john/Kaggle/West_Nile_Virus_Prediction/Data')

weather <- read.csv("weather.csv", stringsAsFactors = FALSE, header = TRUE)
colnames(weather) <- tolower(colnames(weather))
weather$date <- as.Date(weather$date)

library(dplyr)
library(stringr)

weather <- tbl_df(weather)
head(weather)
table(weather$codesum)

## Need to make sense of some of these codes. Will create a separate vector where I've 
## split them all up.

codes <- str_split(weather$codesum, " ")
lengths <- sapply(codes,length)
max(lengths)
## Looks like some days have up to 6 different weather events

codes2 <- c()
for(i in 1:6) {
    codes2 <- c(codes2,sapply(codes,'[',i))
}
codes2 <- codes2[-which(is.na(codes2))]
table(codes2)

## Let's make an indicator for wet weather.
## DZ = Drizzle
## RA = Rain
## SQ = Squall
## TS = Thunderstorm
## TSRA = Thunderstorm/Rain

weather$wet_conditions <- ifelse(str_detect(weather$codesum, "DZ") |
                                 str_detect(weather$codesum, "RA") |
                                 str_detect(weather$codesum, "SQ") |
                                 str_detect(weather$codesum, "TS") |
                                 str_detect(weather$codesum, "TSRA"),
                                 1,
                                 0)
weather$wet_conditions <- as.numeric(weather$wet_conditions)

## Let's compare average, high and low temps across airports
library(reshape2)
library(ggplot2)
## need to reformat the dataset
format_temp <- weather %>%
    select(station, date, tmax, tmin, tavg, dewpoint, wetbulb)

format_temp2 <- melt(format_temp,
                     id.vars = c('station','date'),
                     measure.vars = c('tmax','tmin','tavg','dewpoint','wetbulb'),
                     variable.name = 'temp')

format_temp2$value <- as.numeric(format_temp2$value)


plot1 <- ggplot(data=format_temp2, aes(x=date, y=value)) +
    geom_line(aes(colour=factor(station))) +
    facet_wrap(~temp,ncol = 1)
plot1

## Not very telling, let's look at differences between the two airports
format_temp3 <- dcast(format_temp2,
                      date~temp+station,
                      value.var = 'value')

format_temp3 <- format_temp3 %>%
    mutate(max_dif = tmax_1 - tmax_2,
           min_dif = tmin_1 - tmin_2,
           avg_dif = tavg_1 - tavg_2,
           dew_dif = dewpoint_1 - dewpoint_2,
           wet_dif = wetbulb_1 - wetbulb_2)

## now melt this down

format_temp4 <- melt(format_temp3,
                     id.vars = c("date"),
                     measure.vars = c("max_dif","min_dif","avg_dif","dew_dif","wet_dif"))

## now plot these
plot2 <- ggplot(data=format_temp4,
                 aes(x=date,y=value)) +
    geom_line()+
    facet_wrap(~variable, ncol = 1)
plot2

## Looks like average temps are usually very similar, max and mins can differ somewhat
which(format_temp4$variable == "avg_dif" & abs(format_temp4$value) > 5)
summary(format_temp4$value[which(format_temp4$variable == "avg_dif")])
summary(format_temp4$value[which(format_temp4$variable == "dew_dif")])
summary(format_temp4$value[which(format_temp4$variable == "wet_dif")])
sd(format_temp4$value[which(format_temp4$variable == "avg_dif")],na.rm=TRUE)
sd(format_temp4$value[which(format_temp4$variable == "dew_dif")],na.rm=TRUE)
sd(format_temp4$value[which(format_temp4$variable == "wet_dif")],na.rm=TRUE)
#only three times the average difference is > 5 degrees.

## So like I suspected, using the average temperature between the two airports on a given
## day should be a good approximation

weather$tmax <- as.numeric(weather$tmax)
weather$tmin <- as.numeric(weather$tmin)
weather$tavg <- as.numeric(weather$tavg)
weather$dewpoint <- as.numeric(weather$dewpoint)
weather$wetbulb <- as.numeric(weather$wetbulb)

weather2 <- weather %>%
    group_by(date) %>%
    summarise(avg_max = mean(tmax,na.rm=TRUE),
              avg_min = mean(tmin,na.rm=TRUE),
              avg_avg = mean(tavg,na.rm=TRUE),
              avg_dew = mean(dewpoint,na.rm=TRUE),
              avg_wet = mean(wetbulb,na.rm=TRUE),
              wet_con = max(0,min(1,sum(wet_conditions))))

write.csv(weather2,"weather2.csv")
