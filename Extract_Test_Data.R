# Set system time:
Sys.setenv(TZ = "UTC")

# Import saved data as RDS file:
df.alltagsGPS <- readRDS("/Users/toby/Toronto Zoo/Bat Conservation - General/1 - Current Projects/c) miniMOTUS/1 - MOTUS Server Downloads/204_alltagsGPS2.rds")

# Subset relevant columns
all.hits <- df.alltagsGPS[, c("ts", "sig", "motusTagID", "mfgID", "tagModel", "recvDeployID", "gpsLat", "gpsLon", "gpsAlt", "recv", "recvDeployName")]

# Set path for saving test data sets
path <- "/Users/toby/Toronto Zoo/Bat Conservation - General/1 - Current Projects/c) miniMOTUS/2020 MOTUS Testing/Tag Data"

# Create test data sets by date
saveRDS(subset(all.hits, lubridate::date(ts) == "2020-08-20"), paste(path, "Linear1_test.rds", sep = "/"))
saveRDS(all.hits[lubridate::date(all.hits$ts) == "2020-08-21" & lubridate::hour(all.hits$ts) < 17,], paste(path, "Linear2_test.rds", sep = "/"))
saveRDS(all.hits[lubridate::date(all.hits$ts) == "2020-08-21" & lubridate::hour(all.hits$ts) > 17,], paste(path, "OH-Woods_test.rds", sep = "/"))
saveRDS(subset(all.hits, lubridate::date(ts) == "2020-08-25"), paste(path, "Circle_test.rds", sep = "/"))
saveRDS(subset(all.hits, lubridate::date(ts) == "2020-08-26"), paste(path, "Array_test.rds", sep = "/"))
saveRDS(subset(all.hits, lubridate::date(ts) == "2020-08-31"), paste(path, "GR_test.rds", sep = "/"))
