library(XML)
library(dplyr)
library(purrr)
library(data.table)
library(geosphere)
library(ggplot2)
library(ggmap)
library(sf)

### Load and arrange GPS track:

# Define fnction to load and organise GPX data of tag locations

TagLocs <- function (filename, tagID) {

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
    gpx$Tag_ID <- tagID

    # Set column names
  # gpx <- setNames(gpx, c(paste("Elev_T", tagID, sep = ""),paste("Time_T", tagID, sep = ""),paste("Lat_T", tagID, sep = ""), paste("Lon_T", tagID, sep = "")))
    gpx <- setNames(gpx, c("Tag_Elevation", "Tag_Time", "Tag_Lat", "Tag_Lon", "Tag_ID"))
    
    # Assign to environment
    assign(paste("TagLocs_", tagID, sep=""),gpx, envir = .GlobalEnv)
    
}    

# Call function

TagLocs("/Users/toby/Toronto Zoo/Bat Conservation - General/1 - Current Projects/c) miniMOTUS/2020 MOTUS Testing/ViewRanger Tracks/GPX Files/2020-08-31 Motus array test Glen Rouge tag 5.gpx", 5)
TagLocs("/Users/toby/Toronto Zoo/Bat Conservation - General/1 - Current Projects/c) miniMOTUS/2020 MOTUS Testing/ViewRanger Tracks/GPX Files/2020-08-31 Motus array test Glen Rouge tag 7.gpx", 7)

# Combine results

Tags <- rbind(TagLocs_5, TagLocs_7)
rm(TagLocs_5)
rm(TagLocs_7)

### Load MOTUS hits

# Read RDS file
Rec_Hits <- readRDS("/Users/toby/Toronto Zoo/Bat Conservation - General/1 - Current Projects/c) miniMOTUS/2020 MOTUS Testing/Tag Data/GR_test.rds")

# Drop false hits 
Rec_Hits <- subset(Rec_Hits, !recvDeployName == "Winous Point 2020")
Rec_Hits <- subset(Rec_Hits, !recvDeployName == "Test-RPI32C60")

# Set tagID to numeric
Rec_Hits$mfgID <- as.numeric(Rec_Hits$mfgID)

# Rename columns
Rec_Hits <- setNames(Rec_Hits, c("Rec_Time", "Sig_Stength", "MOTUS_TagID", "Tag_ID", "Tag_Model", "Rec_Deployment", "Rec_Lat", "Rec_Lon", "Rec_Elevation", "Rec_Serial", "Rec_Name"))

### Combine based on nearest matched time

# Coerce to data.table and append join columns to preserve the original columns 
setDT(Tags)[, join_date := Tag_Time]
setDT(Rec_Hits)[, join_date := Rec_Time]
# Rolling join
data <- Tags[Rec_Hits, on = .(Tag_ID, join_date), roll = "nearest"]





### Calculate distance between transmitter and reciever

data$Distance <- distHaversine(data[,3:4], data[,12:13])

### Create table of receiever locations

Rec_Locs <- data[!duplicated(data$Rec_Name), ]
Rec_Locs <- Rec_Locs[,12:16]


ggplot(data, aes(x = Tag_Lon, y = Tag_Lat, colour = Rec_Name)) +
  coord_quickmap() +
  geom_point() +
  geom_point(data = Rec_Locs, mapping = aes(x = Rec_Lon, y = Rec_Lat, colour = Rec_Name, size = 2, shape = 13)) +
  #geom_point(data = Tags, mapping = aes(x = Tag_Lon, y = Tag_Lat)) +
  scale_shape_identity() +
  facet_wrap(~Tag_ID) 

ggplot(data, aes(x = Distance, y = Sig_Strength, colour = recvDeployName)) +
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


