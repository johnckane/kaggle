setwd('/home/john/Kaggle/West_Nile_Virus_Prediction/Data')

library(dplyr)

weather2 <- read.csv("weather2.csv", stringsAsFactors = FALSE, header = TRUE)

## As per the literature:
##
##  "Drier conditions in the spring followed by wetter conditions just prior to an
##   increase in infection were factors in some but not all years."
##
##  Source: http://parasitesandvectors.com/content/pdf/1756-3305-3-19.pdf
##
## So I'll use May as a proxy for spring weather. For each year we have in the training
## set I'll summarise average temperature in May as well as number of rainy days. This
## will be used as a potential feature in the models

head(weather2)
summary(weather2$date)
weather2$date <- as.Date(weather2$date)
summary(weather2$date)
weather2$year <- format(weather2$date, "%Y")
table(weather2$year)
weather2$month <- format(weather2$date, "%b")
table(weather2$month)

may <- filter(weather2, month == "May")

may <- may %>%
    group_by(year) %>%
    summarise(avg_may_temp = mean(avg_avg),
              num_may_rain = sum(wet_con),
              avg_may_dew  = mean(avg_dew),
              avg_may_wet  = mean(avg_wet))
write.csv(may,"may_weather.csv")
    