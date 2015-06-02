setwd('/home/john/Kaggle/West_Nile_Virus_Prediction/Data')
library(dplyr)
test <- read.csv("test.csv", stringsAsFactors = FALSE, header = TRUE)
colnames(test) <- tolower(colnames(test))
test$date <- as.Date(test$date)
test$year <- as.numeric(format(test$date, "%Y"))
test$month <- format(test$date, "%b")
test$month_day <- format(test$date, "%m %d")
test$day_of_year <- as.numeric(format(test$date, "%j"))

colnames(test)
## First the weather data
## Need to bring in other feature engineering data sets. 
may_weather <- read.csv('may_weather.csv', stringsAsFactors = FALSE, header = TRUE)
weather3 <- read.csv('weather3.csv', stringsAsFactors = FALSE, header = TRUE)
weather3$date <- as.Date(weather3$date)


test2 <- left_join(test,may_weather, by = 'year')
test3 <- left_join(test2, weather3, by = 'date')


##  Now we need to feature engineer the following:
##  1) Determine if the day_of_year is within 1, 2 or 3 sd of the mean day in the training set
##  2) Re-classify infrequently occuring species to "OTHER"
##  3) Assign each trap to each of the different regions as determined by k-means clustering

## Assign day_of_year to 1, 2 or 3 sd. 
test4 <- test3 %>%
    mutate(cut  = ifelse((day_of_year < 201 | day_of_year >= 265),
                         3,
                         ifelse((day_of_year >= 201 & day_of_year < 217) | (day_of_year >= 249 & day_of_year < 265),
                                2,
                                1)))
check_plot <- ggplot(data = cv4,
                     aes(x = as.factor(cut),
                         y = day_of_year))
check_plot + geom_point()

## Re-classify infrequently occuring species to "OTHER"
other <- c("CULEX ERRATICUS", "CULEX SALINARIUS", "CULEX TARSALIS", "CULEX TERRITANS",
           "UNSPECIFIED CULEX")

test4$species2 <- ifelse(test4$species %in% other,
                       "OTHER",
                       test4$species)
table(test4$species, test4$species2)
## a challenge here is definitely very little info in the training data about other
## species other than the big 3. in the test data there is a lot of "OTHERS"

## Assign each trap to one of the 10 predefined clusters
test4 <- test4 %>%
    mutate(unique_id = row_number())
set.seed(14)
    
training3 <- read.csv("training3.csv",stringsAsFactors = FALSE, header = TRUE)

assigned_centers <- matrix(0,nrow=dim(test4)[1],ncol=9)

for(i in 10:10){
    centers <- kmeans(cbind(training3$latitude,training3$longitude),centers = i, nstart = 50)$centers
    centers <- as.data.frame(centers)
    colnames(centers) <- c("center_latitude","center_longitude")
    centers$center <- c(1:i)
    test4$join <- 1
    centers$join <- 1
    testjoin <- left_join(test4,centers,by = 'join')    
    testjoin$center_distance <-
        sqrt((testjoin$center_latitude - testjoin$latitude)**2 + (testjoin$center_longitude - testjoin$longitude)**2)
    
    testjoin2 <- testjoin %>%
        group_by(unique_id) %>%
        arrange(center_distance) %>%
        summarise(nearest_center = first(center))
    
    assigned_centers[,i-1] <- testjoin2$nearest_center
    
}

colnames(assigned_centers) <-
    c("c2",
      "c3",
      "c4",
      "c5",
      "c6",
      "c7",
      "c8",
      "c9",
      "c10")


test_final <- data.frame(test4,assigned_centers)

test_final$c2 <- as.factor(test_final$c2)
test_final$c3 <- as.factor(test_final$c3)
test_final$c4 <- as.factor(test_final$c4)
test_final$c5 <- as.factor(test_final$c5)
test_final$c6 <- as.factor(test_final$c6)
test_final$c7 <- as.factor(test_final$c7)
test_final$c8 <- as.factor(test_final$c8)
test_final$c9 <- as.factor(test_final$c9)
test_final$c10 <- as.factor(test_final$c10)

write.csv(test_final,"test_final.csv")
