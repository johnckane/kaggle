setwd('/home/john/Kaggle/West_Nile_Virus_Prediction/Data')

spray <- read.csv("spray.csv", stringsAsFactors = FALSE, header = TRUE)
colnames(spray) <- tolower(colnames(spray))
spray$date <- as.Date(spray$date)
spray$year <- format(spray$date, "%Y")
table(spray$year)

library(dplyr)
library(ggmap)

chicago <- get_map(location = c(lon = -87.6847, lat = 41.8369),
                         color = "color",
                         source = "google",
                         maptype = "roadmap",
                         zoom = 10)

ggmap(chicago) +
    geom_point(data=spray,aes(x=longitude,y=latitude),alpha = 0.01)

