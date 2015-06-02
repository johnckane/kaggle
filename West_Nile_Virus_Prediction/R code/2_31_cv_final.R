setwd('/home/john/Kaggle/West_Nile_Virus_Prediction/Data')

cv <- read.csv("cv.csv", stringsAsFactors = FALSE, header = TRUE)

## First the weather data
## Need to bring in other feature engineering data sets. 
may_weather <- read.csv('may_weather.csv', stringsAsFactors = FALSE, header = TRUE)
head(may_weather)
weather3 <- read.csv('weather3.csv', stringsAsFactors = FALSE, header = TRUE)
head(weather3)

cv2 <- left_join(cv,may_weather, by = 'year')
View(cv2)

cv3 <- left_join(cv2, weather3, by = 'date')
View(cv3)

##  Now we need to feature engineer the following:
##  1) Determine if the day_of_year is within 1, 2 or 3 sd of the mean day in the training set
##  2) Re-classify infrequently occuring species to "OTHER"
##  3) Assign each trap to each of the different regions as determined by k-means clustering

## Assign day_of_year to 1, 2 or 3 sd. 
cv4 <- cv3 %>%
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
other <- c("CULEX ERRATICUS", "CULEX SALINARIUS", "CULEX TARSALIS", "CULEX TERRITANS")

cv4$species2 <- ifelse(cv4$species %in% other,
                             "OTHER",
                             cv4$species)
table(cv4$species, cv4$species2)

## Assign each trap to one of the 10 predefined clusters
## First we'll need to regenerate those clusters
training3 <- read.csv("training3.csv",stringsAsFactors = FALSE, header = TRUE)
## each observation needs a unique id
training3 <- training3 %>%
    mutate(unique_id = row_number())
set.seed(14)
clusters <- matrix(0, nrow=7040, ncol = 9)
for(j in 2:2){
    clusters[,j-1] <- kmeans(cbind(training3$latitude,training3$longitude),centers = j, nstart = 50)$cluster
    # the "nstart = 50" comes from a recommendation in ISLR first printing page 405.
}
clusters <- data.frame(clusters)
colnames(clusters) <- c("c2","c3","c4","c5","c6","c7","c8","c9","c10")

centers <- kmeans(cbind(training3$latitude,training3$longitude),centers = 4, nstart = 50)$centers
centers <- as.data.frame(centers)
colnames(centers) <- c("center_latitude","center_longitude")
centers$center <- c(1:4)
training3$join <- 1
centers$join <- 1
testjoin <- left_join(training3,centers,by = 'join')
View(testjoin)

testjoin$center_distance <-
    sqrt((testjoin$center_latitude - testjoin$latitude)**2 + (testjoin$center_longitude - testjoin$longitude)**2)
head(testjoin)

## this appears to work. Now we need for each data point (4 rows per datapoint) select the
## center that is closest. 

testjoin2 <- testjoin %>%
    group_by(unique_id) %>%
    arrange(center_distance) %>%
    summarise(nearest_center = first(center))
head(testjoin2)

## I think this does it!!

## Now we need to implement this across for each of 9 clustering setups 
## generate centers,
## turn into data frame,
## join with training data
## calculate distances
## assign center
## repeat

assigned_centers <- matrix(0,nrow=7040,ncol=9)

for(i in 2:10){
    centers <- kmeans(cbind(training3$latitude,training3$longitude),centers = i, nstart = 50)$centers
    centers <- as.data.frame(centers)
    colnames(centers) <- c("center_latitude","center_longitude")
    centers$center <- c(1:i)
    training3$join <- 1
    centers$join <- 1
    testjoin <- left_join(training3,centers,by = 'join')    
    testjoin$center_distance <-
        sqrt((testjoin$center_latitude - testjoin$latitude)**2 + (testjoin$center_longitude - testjoin$longitude)**2)
    head(testjoin)

    testjoin2 <- testjoin %>%
        group_by(unique_id) %>%
        arrange(center_distance) %>%
        summarise(nearest_center = first(center))
    
    assigned_centers[,i-1] <- testjoin2$nearest_center
    
}

colnames(assigned_centers) <-
    c("c2m",
      "c3m",
      "c4m",
      "c5m",
      "c6m",
      "c7m",
      "c8m",
      "c9m",
      "c10m")

## bring in training4, compaere the results 
training4 <- read.csv("training4.csv", stringsAsFactors = FALSE, header = TRUE)
last_test <- data.frame(training4,assigned_centers)
colnames(last_test)

last_test %>%
    summarise(c2t = sum(c2 - c2m),
              c3t = sum(c3 - c3m),
              c4t = sum(c4 - c4m),
              c5t = sum(c5 - c5m),
              c6t = sum(c6 - c6m),
              c7t = sum(c7 - c7m),
              c8t = sum(c8 - c8m),
              c9t = sum(c9 - c9m),
              c10t = sum(c10 - c10m))

# Nice! I was able to replicate the results. Now I need to apply this to the cv data.



assigned_centers_cv <- matrix(0,nrow=dim(cv4)[1],ncol=9)

cv4 <- cv4 %>%
    mutate(unique_id = row_number())

for(j in 2:10){
    centers <- kmeans(cbind(training3$latitude,training3$longitude),centers = j, nstart = 50)$centers
    centers <- as.data.frame(centers)
    colnames(centers) <- c("center_latitude","center_longitude")
    centers$center <- c(1:j)
    cv4$join <- 1
    centers$join <- 1
    testjoin <- left_join(cv4,centers,by = 'join')    
    testjoin$center_distance <-
        sqrt((testjoin$center_latitude - testjoin$latitude)**2 + (testjoin$center_longitude - testjoin$longitude)**2)
    
    testjoin2 <- testjoin %>%
        group_by(unique_id) %>%
        arrange(center_distance) %>%
        summarise(nearest_center = first(center))
    
    assigned_centers_cv[,j-1] <- testjoin2$nearest_center
    
}

colnames(assigned_centers_cv) <-
    c("c2",
      "c3",
      "c4",
      "c5",
      "c6",
      "c7",
      "c8",
      "c9",
      "c10")
## Now join up with cv data

cv_final <- data.frame(cv4,assigned_centers_cv)

cv_final$c2 <- as.factor(cv_final$c2)
cv_final$c3 <- as.factor(cv_final$c3)
cv_final$c4 <- as.factor(cv_final$c4)
cv_final$c5 <- as.factor(cv_final$c5)
cv_final$c6 <- as.factor(cv_final$c6)
cv_final$c7 <- as.factor(cv_final$c7)
cv_final$c8 <- as.factor(cv_final$c8)
cv_final$c9 <- as.factor(cv_final$c9)
cv_final$c10 <- as.factor(cv_final$c10)

write.csv(cv_final,"cv_final.csv")
