# Where are the measurement points?
library(ggmap)

data_dir <- "/home/john/Kaggle/West_Nile_Virus_Prediction"


# Ggmap is used to download and plot map images. Scripts don't have access to the network, so 
# the map data is included as an input file. 
# I downloaded it with the command: mapdata <- get_openstreetmap(bbox = c(-88.0, 41.6, -87.5, 42.1), color="bw", scale = round(606250/4))
mapdata <- readRDS(file.path(data_dir, "mapdata_copyright_openstreetmap_contributors.rds"))

measurement_sites_plot <- ggmap(mapdata) 
    geom_point(aes(x=Longitude, y=Latitude), data=wnv)

ggsave("measurement_sites_plot.png", measurement_sites_plot, width = 12, height = 12)