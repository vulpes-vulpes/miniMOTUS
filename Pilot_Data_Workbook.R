library(XML)
library(dplyr)
library(purrr)
library(data.table)
library(geosphere)
library(ggplot2)
library(ggmap)
library(sf)

### Load and arrange GPS track:

# List .gpx file
filename <- "/Users/toby/Toronto Zoo/Bat Conservation - General/1 - Current Projects/c) miniMOTUS/2020 MOTUS Testing/ViewRanger Tracks/GPX Files/2020-08-31 Motus array test Glen Rouge tag 5.gpx"

# Parse .gpx file into data frame
gpx <- filename %>%
  xmlTreeParse(useInternalNodes = TRUE) %>%
  xmlRoot %>%
  xmlToList %>%
  (function(x) x$trk) %>%
  (function(x) unlist(x[names(x) == "trkseg"], recursive = FALSE)) %>%
  map_df(function(x) as.data.frame(t(unlist(x)), stringsAsFactors=FALSE))

# Fix names and formatting
gpx$lat <- as.numeric(gpx$.attrs.lat)
gpx$lon <- as.numeric(gpx$.attrs.lon)
gpx$.attrs.lat <- NULL
gpx$.attrs.lon <- NULL
gpx$time <- as.POSIXct(gpx$time, tz = "UTC", "%Y-%m-%dT%H:%M:%OS")


### Load MOTUS hits

# Read RDS file
hits <- readRDS("/Users/toby/Toronto Zoo/Bat Conservation - General/1 - Current Projects/c) miniMOTUS/2020 MOTUS Testing/Tag Data/GR_test.rds")
# Drop false hits 
hits <- subset(hits, !recvDeployName == "Test-RPI32C60")


### Combine based on nearest matched time

# Coerce to data.table and append join columns to preserve the original columns 
setDT(gpx)[, join_date := time]
setDT(hits)[, join_date := ts]
# Rolling join
data <- gpx[hits, on = .(join_date), roll = "nearest"]





### Calculate distance between transmitter and reciever

data$Distance <- distHaversine(data[,3:4], data[,12:13])

### Create table of receiever locations

recs <- data[!duplicated(data$recvDeployName), ]
recs <- recs[,12:16]






ggplot(data, aes(x = lon, y = lat, colour = recvDeployName)) +
  coord_quickmap() +
  geom_point()

ggplot(data, aes(x = Distance, y = sig, colour = recvDeployName)) +
  geom_point()



library(rgdal)

shapefile <- readOGR("/Users/toby/Toronto Zoo/Bat Conservation - General/8 - Tools/2 - GIS/8- Completed Projects/USFWS/2019/USFWS 2019/shapefile", "500m_Polygons_2")

# Next the shapefile has to be converted to a dataframe for use in ggplot2
shapefile_df <- fortify(shapefile)

# Now the shapefile can be plotted as either a geom_path or a geom_polygon.
# Paths handle clipping better. Polygons can be filled.
# You need the aesthetics long, lat, and group.
map <- ggplot() +
  geom_path(data = shapefile_df, 
            aes(x = long, y = lat, group = group),
            color = 'gray', fill = 'white', size = .2)

print(map) 






mapImageData <- get_openstreetmap(center = c(lon = mean(gpx$lon), lat = mean(gpx$lat)),
                                  zoom = 10,
                                  color = 'bw',
                                  scale = 1,
                                  maptype = "terrain")
ggmap(mapImageData, extent = "device") + # removes axes, etc.
  geom_point(aes(x = lon,
                 y = lat),
             data = gpx,
             colour = "red3",
             alpha = .1,
             size = .1)


