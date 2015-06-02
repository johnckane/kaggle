setwd('/home/john/Kaggle/West Nile Virus Prediction/Data')
library(dplyr)
library(ggplot2)

training3 <- read.csv('training3.csv', stringsAsFactors = FALSE, header = TRUE)
colnames(training3) <- tolower(colnames(training3))
head(training3)

## Let's look at how many traps we are dealing with 
dim(table(training3$trap))
## 136, that's a lot. 
table(training3$trap)

prop.table(table(training3$wnvpresent,training3$trap),2)
prop.table(table(training3$wnvpresent,training3$trap),2)[2,]
## Some traps are definitely more likely than others to test postive. 
## There may be confounding with location, so let's take a look at that. 
## First, let's pull unique trap and lat/lon values
training3 <- training3[-c(1,2,3)]
trap_locations <- select(training3, trap, addressnumberandstreet) %>%
    group_by(trap,addressnumberandstreet) %>%
    summarise(count = n())
View(trap_locations)
## So it looks like most, but not all of the traps are only ever at one location.
##

## Feature engineering solution: k-means clustering
## Using latitude and longitude data I will create clusters of data points that 
## divide the region into different areas, based on trap locations. 
## I'll use different number of clusters and end up using that as a tuning parameter
## for the final model. 

##########################################################################
##########                   k-means clustering                 ##########
##########################################################################

## Let's do this in a loop and automate the extracting of cluster assignment. 
## here's a matrix we're going to fill
set.seed(14)
clusters <- matrix(0, nrow=7040, ncol = 9)
for(j in 2:10){
    clusters[,j-1] <- kmeans(cbind(training3$latitude,training3$longitude),centers = j, nstart = 50)$cluster
    # the "nstart = 50" comes from a recommendation in ISLR first printing page 405.
}
clusters <- data.frame(clusters)
colnames(clusters) <- c("c2","c3","c4","c5","c6","c7","c8","c9","c10")
## now add this to the dataframe
training4 <- data.frame(training3,clusters)
View(training4) 

## let's plot all this
library(ggmap)

data_dir <- "/home/john/Kaggle/West Nile Virus Prediction"
mapdata <- readRDS(file.path(data_dir, "mapdata_copyright_openstreetmap_contributors.rds"))

library(gridExtra)
plots <- vector("list",9)
for(k in 1:9){
    k0 <- k+1 #for the plot titles
    plots[[k]] <- ggmap(mapdata) + 
        geom_point(data=training4,aes(x=longitude,y=latitude,color=as.factor(training4[,18+k]))) +
        #geom_point(data=as.data.frame(cl1$centers),aes(x=V2,y=V1,size = 3, shape = "square" )) +
        theme(legend.position = "none") +
        ggtitle(paste("K = ",k0,sep=""))
}
grid.arrange(plots[[1]],
             plots[[2]],
             plots[[3]],
             plots[[4]],
             plots[[5]],
             plots[[6]],
             plots[[7]],
             plots[[8]],
             plots[[9]],
             ncol = 5)


## Now let's see if different "regions" are more likely to test positive
prop.table(table(training4$wnvpresent,training4$c2),2)
## some definitely are, let's see if this holds year-to-year
ftable(training4$c2,training4$wnvpresent,training4$year)
## doesn't really hold year to year it would appear. Will still use thse features. It will
## be interesting to see what kind of breakdown leads to the most informative result in 
## differences between groups.

write.csv(training4,"training4.csv")

###############################################################
#####           Tree methods to split lat/lon       ###########
###############################################################
library(tree)
lat_lon_tree <- tree(data=training3,
                     as.factor(wnvpresent)~latitude+longitude)
plot(lat_lon_tree)
text(lat_lon_tree,pretty=0)
## Looks like latitude cut at -87.7789 makes any difference

## See how this varies by year
table(training3$year)
tree07 <- tree(data=training3[training3$year==2007,],
               as.factor(wnvpresent) ~ latitude + longitude)
tree09 <- tree(data=training3[training3$year==2009,],
               as.factor(wnvpresent) ~ latitude + longitude)
tree11 <- tree(data=training3[training3$year==2011,],
               as.factor(wnvpresent) ~ latitude + longitude)
tree13 <- tree(data=training3[training3$year==2013,],
               as.factor(wnvpresent) ~ latitude + longitude)

plot(tree07);text(tree07,pretty=0)
plot(tree09);text(tree09,pretty=0)
plot(tree11);text(tree11,pretty=0)
plot(tree13);text(tree13,pretty=0)
