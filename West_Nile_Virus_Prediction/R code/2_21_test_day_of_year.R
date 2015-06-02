setwd('/home/john/Kaggle/West_Nile_Virus_Prediction/Data')
library(dplyr)
library(ggplot2)
library(gridExtra)

training <- read.csv('training.csv', stringsAsFactors = FALSE, header = TRUE)
colnames(training) <- tolower(colnames(training))
training$date <- as.Date(training$date)
head(training)
training <- tbl_df(training)
training$year <- as.numeric(format(training$date, "%Y"))
training$month <- format(training$date, "%b")
training$month_day <- format(training$date, "%m %d")
training$day_of_year <- as.numeric(format(training$date, "%j"))

summary(day_summary$day_of_year)


table(training$wnvpresent)
prop.table(table(training$wnvpresent,training$month),2)


qplot(data=training,
      x=day_of_year, 
      y=as.factor(wnvpresent),
      geom="jitter")



## plot cases ##
cases <- filter(training, wnvpresent == 1)
p1 <- ggplot(data=cases,
             aes(x=month_day))
p1 +
    geom_histogram() +
    facet_wrap(~year,ncol = 1)

summary(cases$day_of_year)
summary(cases$day_of_year~cases$year)
summary(cases$day_of_year[cases$year==2007])
summary(cases$day_of_year[cases$year==2009])
summary(cases$day_of_year[cases$year==2011])
summary(cases$day_of_year[cases$year==2013])
hist(cases$day_of_year)
sd(cases$day_of_year)
cases %>%
    group_by(year) %>%
    summarise(mean = mean(day_of_year),
                sd =  sd(day_of_year))

# let's see how many cases fall within 1,2,3 sd of the mean
library(Hmisc)
mu_day <- mean(cases$day_of_year)
sd_day <- sd(cases$day_of_year)
cases$cut <- cut2(cases$day_of_year, c(mu_day-3*sd_day,
                               mu_day-2*sd_day,
                               mu_day-1*sd_day,
                               mu_day+1*sd_day,
                               mu_day+2*sd_day,
                               mu_day+3*sd_day) )
summary(cases$cut)
## Looks like we have 65% within 1 sd, 96% within 2 sd, 99.8% within 3 sd
## Now let's see by year
prop.table(table(cases$year,cases$cut),1)
## So it looks like it differs year to year, but still centered around the overall mean. 




## plot non-cases ##
controls <- filter(training, wnvpresent == 0)
p2 <- ggplot(data=controls,
             aes(x=month_day))
p2 + 
    geom_histogram() +
    facet_wrap(~year,ncol = 1)
summary(controls$day_of_year)
hist(controls$day_of_year)    
sd(controls$day_of_year)

## summarise by day_of_year the most likely time to test postive
day_summary <- training %>%
    group_by(year,day_of_year) %>%
    summarise(pos_pct = sum(wnvpresent)/n())


day_summary$day_of_year[which.max(day_summary$pos_pct)]
day_summary$day_of_year[which.max(day_summary$pos_pct[day_summary$year==2007])]
day_summary$day_of_year[which.max(day_summary$pos_pct[day_summary$year==2009])]
day_summary$day_of_year[which.max(day_summary$pos_pct[day_summary$year==2011])]
day_summary$day_of_year[which.max(day_summary$pos_pct[day_summary$year==2013])]

q <- qplot(data=day_summary, 
      x = day_of_year,
      y = pos_pct,
      geom = 'bar',
      stat = 'identity',
p2b <- ggplot(data=day_summary,
              aes(x=day_of_year, y = pos_pct)) +
              geom_bar(stat = 'identity') +
              ggtitle("Probability of Positive Test \n by Day of Year") +
              scale_x_continuous("Day of Year") +
              scale_y_continuous("Probability of Positive Test")
p3 <- ggplot(data=day_summary,
             aes(x=day_of_year, y = pos_pct)) +
             geom_bar(stat='identity') + 
             facet_wrap(~year,ncol = 1) +
             ggtitle("Probability of Positive Test \n by Year and Day of Year")

grid.arrange(p2b,p3,ncol = 2)
## So it looks like there is definitely a time of year where it is more probable that
## there will be a positive test. Each year is very different as far as general likelihood
## of a postive test but each year around the 230th day of the year it is more likely.
## So I'll create a feature to account for this.

levels(cases$cut)
## use some nested ifelse calls here
training2 <- training %>%
    mutate(cut  = ifelse((day_of_year < 201 | day_of_year >= 265),
                         3,
                         ifelse((day_of_year >= 201 & day_of_year < 217) | (day_of_year >= 249 & day_of_year < 265),
                                2,
                                1)))
check_plot <- ggplot(data=training2,
                     aes(x=as.factor(cut),
                         y = day_of_year))
check_plot + geom_point()

write.csv(training2,"training2.csv")
