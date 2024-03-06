# NOTES
# THE GEO FILES ARE TOO LARGE TO RUN HERE; FILES NEED TO BE BROKEN UP INTO ~300 PARTICIPANTS (ROWS) EACH
# SCRIPT INCLUDES 2017-2018 NHANES FILLES, IF AVAILABLE

#################################################################################
# SETUP
#################################################################################

# Clear workspace of all objects and unload all extra (non-base) packages
rm(list = ls(all = TRUE))
if (!is.null(sessionInfo()$otherPkgs)) {
  res <- suppressWarnings(
    lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=""),
           detach, character.only=TRUE, unload=TRUE, force=TRUE))
}

# --> INCLUDE EVERYTHING BELOW IN NHANES DATA REQUEST

#################################################################################
# SETUP
#################################################################################
# install pacman if not already installed & use it to download/install necessary packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,  
               sf, # spatial files
               units, # drop_units()
               parallel, #if use mclapply()
               terra # raster files
               )

#################################################################################
# LOAD DATA ENVIRONMENT
#################################################################################
# external environmental files for land type (e.g., agriculture)
cdl_path <- file.path("data", "raw", "cdl")
cdl_2013 <- rast(file.path(cdl_path, "2013_30m_cdls","2013_30m_cdls.tif"))
cdl_2015 <- rast(file.path(cdl_path, "2015_30m_cdls","2015_30m_cdls.tif"))
cdl_2017 <- rast(file.path(cdl_path, "2017_30m_cdls","2017_30m_cdls.tif"))

# NHANES FILES
## load NHANES FILES FOR 2013-2018
load(file.path("output", "analysis_datasets.rds"))

## --> NOTE: update lat_long_crs if NHANES uses something different
lat_long_crs <- 4326 #WGS84

## load GEO data and set the CRS to meters like the external files
## --> NOTE: fake_GEO_2010 NEEDS TO BE UPDATED BY RDC WITH THE REAL PARTICIPATE GEOCODE FILE
GEO_2010 <- fake_GEO_2010 %>%  
  # make geocoded file into a spatial file
  st_as_sf(coords = c("LONG", "LAT"), remove=F,
           crs=lat_long_crs) %>%
  # set GEO CRS to be the same as the spatial raster files (in meters, not degrees)
  st_transform(st_crs(cdl_2013)) 
  
## GEO FILES FOR NHANES PARTICIPANTS BY YEAR
geo_2013 <- filter(GEO_2010, SEQN %in% nhanes2013$SSGLYP_H$SEQN)
geo_2015 <- filter(GEO_2010, SEQN %in% nhanes2015$SSGLYP_I$SEQN)
geo_2017 <- filter(GEO_2010, SEQN %in% nhanes2017$SSGLYP_J$SEQN)

#################################################################################
# 1. CALCULATE LAND TYPE WITHIN A BUFFER
#################################################################################
# FN RETURNS DATAFRAME W/ CROP FREQUENCIES WITHIN A BUFFERED LOCATION 
crop_frequency_in_buffer <- function(spatial_raster_data, geo_data, buffer.) {
  #make names the same across different datasets
  names(spatial_raster_data) <- "class_name"
  
  # give participant geolocations the same CRS as the raster
  geo_data <- geo_data %>%
    # temporary subject ID - used to merge to results below
    mutate(ID = seq(1:nrow(.)))
  
  # create a dataframe with land info for each location & buffer (takes a bit to run)
  crop_frequency <- extract(spatial_raster_data, st_buffer(geo_data, buffer.), 
                            exact=T) %>%
    # frequency of crops in each buffered location
    group_by(ID, class_name) %>%
    summarize(pixel_count=sum(fraction)) %>%   
    ungroup() %>%
    mutate(buffer_m = buffer.)
  
  # add NHANES SEQN (vs raster ID) to results
  crop_frequency <- left_join(distinct(geo_data, ID, SEQN),
                              crop_frequency) %>%
    # drop temporary ID
    select(-ID)
  
  return(crop_frequency)
}

#################################################################################
buffers <- c(250, 500, 1e3, 2e3)

## --> NOTE: FULL GEO files (e.g., geo_2013) may be too large and crash; code works when you split these into smaller files of ~300 rows each

## 2013 CROP TYPE AND 2013-14 NHANES SSGLYP
# could use parallel processing to speed this up, e.g., mclapply(...mc.cores = 4...) 
crop_frequency_2013 <- lapply(buffers,
                              function(b) {crop_frequency_in_buffer(spatial_raster_data = cdl_2013, #2013 land types file
                                                                      geo_data = geo_2013, #geocodes for 2013-2014 NHANES (may need to split this file smaller for this to run)
                                                                      buffer.=b)}) %>%
  # combine into 1 dataframe
  bind_rows()

 
## 2015 CROP TYPE AND 2015-16 NHANES SSGLYP
crops_in_buffer_2015 <- lapply(buffers,
                               function(b)  {crop_frequency_in_buffer(spatial_raster_data = cdl_2015, #2015 land types file
                                                                        geo_data = geo_2015, #geocodes for 2015-2016 NHANES
                                                                        buffer.=b)}) %>%
  bind_rows()

## 2017 CROP TYPE AND 2017-18 NHANES SSGLYP
crops_in_buffer_2017 <- lapply(buffers,
                               function(b)  {crop_frequency_in_buffer(spatial_raster_data = cdl_2017, #2017 land types file
                                                                      geo_data = geo_2017, #geocodes for 2017-2018 NHANES
                                                                      buffer.=b)}) %>%
  bind_rows()
  

#################################################################################
# 2. PROXIMITY TO LAND TYPE
#################################################################################
# FUNCTION RETURNS THE DISTANCE BETWEEN A LOCATION AND VARIOUS CROP LAND TYPES
# anything beyond 2,000 m is set to 2,000 m
distance_to_ag <- function(spatial_raster_data, geo_data, max_distance=2e3) {
  
  # only keep raster area near a location (speeds things up a lot; crashes otherwise)
  land_type <- crop(spatial_raster_data, st_buffer(geo_data, max_distance)) %>%
    # create polygons for each land type
    as.polygons() %>% 
    st_as_sf()
  
  #distance between each land type and each location
  pt_distances <- st_distance(land_type, geo_data, name="distance_m") %>%
    drop_units() %>%
    as.data.frame() %>%
    rename(distance_m = V1) %>%
    # link SEQN & land IDs
    mutate(SEQN = geo_data$SEQN,
           class_name = land_type$Class_Names)
  
  return(pt_distances)
}

#################################################################################
# calculate distance to crop types, one location at a time
# could use mclapply() instead of lapply() here as well to speed this up

# --> NOTE: FULL GEO files (e.g., geo_2013) may be too large and crash; code works when you split these into smaller files of ~300 rows each for each year
# 2013
crop_distances_2013 <- lapply(1:nrow(geo_2013[1:2,]), # participants for 2013-2014 NHANES
                              function(x) {
                             distance_to_ag(spatial_raster_data=cdl_2013, # 2013 land types file
                                            geo_data = geo_2013[x,] # geocodes for 2013-2014 NHANES
                                            )}) %>%
  bind_rows()

# 2015
crop_distances_2015 <- lapply(1:nrow(geo_2015),  # participants for 2015-2016 NHANES
                              function(x) {
                                distance_to_ag(spatial_raster_data=cdl_2015, # 2015 land types file
                                               geo_data = geo_2015[x,] # geocodes for 2015-2016 NHANES
                                               )}) %>%
  bind_rows()

# 2017
crop_distances_2017 <- lapply(1:nrow(geo_2017),  # participants for 2017-2018 NHANES
                                function(x) {
                                  distance_to_ag(spatial_raster_data=cdl_2017, # 2017 land types file
                                                 geo_data = geo_2017[x,] # geocodes for 2017-2018 NHANES
                                                 )}) %>%
  bind_rows()

#################################################################################
# SAVE THE DATA FILES WE NEED TO ACCESS IN THE LAB 
#################################################################################
save(crop_frequency_2013, 
     crop_frequency_2015,
     crop_frequency_2017,  
     crop_distances_2013, 
     crop_distances_2015, 
     crop_distances_2017,
     file= file.path("output", "crop_proximity_datasets.rds"))
