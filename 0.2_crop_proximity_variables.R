

########################################################################################################################
# SETUP
########################################################################################################################
# Clear workspace of all objects and unload all extra (non-base) packages
rm(list = ls(all = TRUE))
if (!is.null(sessionInfo()$otherPkgs)) {
  res <- suppressWarnings(
    lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=""),
           detach, character.only=TRUE, unload=TRUE, force=TRUE))
}

# install pacman if not already installed & use it to download/install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,  
               sf,
               #rgdal, raster, #deprecated/old - use terra instead  
               units, # drop_units()
               parallel, #mclapply()
               terra # replaces raster (& gdal?)
               )

# --> this can be increased for parallel processing later with mclapply()
mc_cores <- 2

########################################################################################################################
# LOAD DATA ENVIRONMENT
########################################################################################################################
# --> update - OK for NHANES?
lat_long_crs <- 4326 #WGS84

# NHANES FILES
# --> RDC NEEDS TO DOWNLOAD 3 NHANES DEMO_<X> FILES FOR 2013-2018
load(file.path("output", "analysis_datasets.rds"))

# --> fake_GEO_2010 NEEDS TO BE UPDATED BY RDC WITH THE REAL PARTICIPATE GEOCODE FILE
GEO_2010 <- fake_GEO_2010 %>%  
  # make geocoded file into a spatial file
  st_as_sf(coords = c("LONG", "LAT"), remove=F,
           # --> what CRS to use?
           crs=lat_long_crs)  

# --> nhanes2013$DEMO_H$SEQN TO BE UPDATED BY RDC WITH DEMO_H$SEQN OR HOWEVER THIS FILE IS LABELED IN THEIR SYSTEM
geo_2013 <- filter(GEO_2010, SEQN %in% nhanes2013$DEMO_H$SEQN)
geo_2015 <- filter(GEO_2010, SEQN %in% nhanes2015$DEMO_I$SEQN)
#geo_2017 <- filter(GEO_2010, SEQN %in% nhanes2017$DEMO_J$SEQN)

# external environmental files
cdl_path <- file.path("data", "raw", "cdl")
cdl_2013 <- rast(file.path(cdl_path, "2013_30m_cdls","2013_30m_cdls.tif"))
cdl_2015 <- rast(file.path(cdl_path, "2015_30m_cdls","2015_30m_cdls.tif"))
cdl_2017 <- rast(file.path(cdl_path, "2017_30m_cdls","2017_30m_cdls.tif"))

# # --> delete? 
# cultivated_crops <- read.csv(file.path("data", "modified", "cultivated_crops.csv"))
 
########################################################################################################################
# CDL: AG LAND WITHIN A BUFFER
########################################################################################################################
# FN RETURNS DATAFRAME W/ CROP FREQUENCIES WITHIN A BUFFERED LOCATION 

# raster_dt=cdl_2013
# location_dt = GEO_2010[1:10,]
# buffer. = 500
crop_frequency_in_buffer <- function(raster_dt, location_dt, buffer.) {
  #make names the same across different datasets
  names(raster_dt) <- "class_name"
  
  # give participant geolocations the same CRS as the raster
  location_dt <- location_dt %>%
    # same CRS as raster 
    st_transform(st_crs(raster_dt)) %>%
    # use this to merge to results later
    mutate(ID = seq(1:nrow(.)))
  
  # create a dataframe with land info for each location & buffer
  crop_frequency <- extract(raster_dt, st_buffer(location_dt, buffer.), 
                            #fun=tabulate, #tabulate raster values for each polygon - output format is awkward
                            exact=T, # exact proportion of a cell in buffer; could drop
                            #weights=T # approximate proportion of a cell in buffer; this is faster than exact=T
                            #raw=T return numeric (vs character) crop labels
                            ) %>%
    # frequency of crops in each buffered location
    group_by(ID, class_name) %>%
    summarize(pixel_count=sum(fraction)) %>%
    ungroup() %>%
    mutate(buffer_m = buffer.)
  
  # add NHANES SEQN (vs raster ID) to results
  crop_frequency <- left_join(distinct(location_dt, ID, SEQN),
                              crop_frequency)
  
  return(crop_frequency)
}

########################################################################################################################
# quantify land type of crops within buffers of each location, in meters

## NOTE: if memory becomes exhausted, run 1 buffer at a time, e.g.: buffers <- c(250)
buffers <- c(250, 500, 1e3, 2e3)

## 2013 LAND USE WITH 2013-14 NHANES GLY SAMPLES
crop_frequency_2013 <- mclapply(buffers, 
                                mc.cores = mc_cores,
                                function(b)  {
                                  crop_frequency_in_buffer(raster_dt = cdl_2013, #2013 CDL file
                                                           location_dt = geo_2013, # to be replaced by RDC with participant geocodes for 2013-2014 NHANES 
                                                           buffer.=b)}) %>%
  bind_rows()

 
## 2015 LAND USE WITH 2015-16 NHANES GLY SAMPLES
crops_in_buffer_2015 <- mclapply(buffers, 
                                 mc.cores = mc_cores, 
                                 function(b)  {
                                   crop_frequency_in_buffer(raster_dt = cdl_2015, #2015 CDL file
                                                            location_dt = geo_2015, #geocodes for 2015-2016 NHANES 
                                                            buffer.=b)}) %>%
  bind_rows()

# ## TO DO
## 2017 LAND USE WITH 2017-18 NHANES GLY SAMPLES
# crops_in_buffer_2017 <- mclapply(buffers, mc.cores = mc_cores, function(b)  {
#   
#   crop_frequency_in_buffer(raster_dt = cdl_2017, #2017 CDL file
#                            location_dt = geo_2017, #geocode for 2017-18 NHANES GLY sample
#                            buffer.=b)}) %>%
#   bind_rows()



########################################################################################################################
# DISTANCE TO LAND TYPE
########################################################################################################################
# # --> delete crop types here? 
# cultivated <- cultivated_crops$id
# # list is from crop frequency metadata
# corn <- c(1,225,226,228,237,241)
# cotton <- c(2,232,238,239)
# soybeans <- c(5,26,239,240,241,254)
# wheat <- c(22,23,24,26,225,230,234,236,238)
# 
# # --> gmo vs gly desicated?
# gmo <- c(corn, cotton, soybeans,
#          31, #canola
#          36, #alfalfa
#          41) #sugarbeets
# 
# 
# # --> CHECK THAT LIST IS OK
# # wheat, oats
# desication <- c(wheat,
#                 25, 226, 240, #oats
#                 24, #dry beans
#                 51:53, # chick peas, lentisl, peas
#                 10, 76, # peanuts, walnuts
#                 
#                 # --> ??
#                 31:34 # canola, flaxseed, safflower, rape seed
# )
########################################################################################################################
# raster_dt = cdl_2013
# location_dt = GEO_2010[2:2,]
# max_distance=2e3
# crop_type = "cultivated"

# --> keep max distance at 2e3? 

# function calculates the distance for a single location to various crop land types
distance_to_ag <- function(raster_dt, location_dt, max_distance=2e3) {
  
  location_dt <- location_dt %>%
    # same CRS as raster 
    st_transform(st_crs(raster_dt))
  
  # only keep raster area near a location (speeds things up a lot; crashes otherwise)
  nearby_land <- crop(raster_dt, st_buffer(location_dt, max_distance)) %>%
    #polygons for each land type
    as.polygons() %>% #round=F
    st_as_sf()
  
  # QC - visualize location & land types nearby. #looks good
  # ggplot() + geom_sf(data=nearby_land, aes(fill=Class_Names)) + geom_sf(data=location_dt)# + scale_fill_viridis_c() 
  
  #distance to land types
  pt_distances <- st_distance(nearby_land, location_dt, name="distance_m") %>%
    drop_units() %>%
    as.data.frame() %>%
    rename(distance_m = V1) %>%
    # link SEQN & land IDs
    mutate(SEQN = location_dt$SEQN,
           class_name = nearby_land$Class_Names)
  
  return(pt_distances)
}

########################################################################################################################
# x=2
# calculate distance to crop types, one location at a time

crop_distances_2013 <- mclapply(1:nrow(geo_2013), #to be updated 
                           mc.cores = mc_cores,
                           function(x) {
                             distance_to_ag(raster_dt=cdl_2013,
                                                       location_dt = geo_2013[x,] #to be updated 
                                            )}) %>%
  # combine into 1 dataframe
  bind_rows()

crop_distances_2015 <- mclapply(1:nrow(geo_2015),  #to be updated 
                                mc.cores = mc_cores,
                                function(x) {
                                  distance_to_ag(raster_dt=cdl_2015,
                                                 location_dt = geo_2015[x,] #to be updated 
                                                 )}) %>%
  bind_rows()

# crop_distances_2017 <- mclapply(1:nrow(geo_2017),  #to be updated 
#                                 mc.cores = mc_cores,
#                                 function(x) {
#                                   distance_to_ag(raster_dt=cdl_2017,
#                                                  location_dt = geo_2017[x,] #to be updated 
#                                   )}) %>%
#   bind_rows()


########################################################################################################################
# SAVE THE DATA FILES WE NEED TO ACCESS IN THE LAB 
########################################################################################################################
save(crops_in_buffer_2013, crops_in_buffer_2015, #crops_in_buffer_2017,
     crop_distances_2013, crop_distances_2015, crop_distances_2017,
     file= file.path("output", "crop_proximity_datasets.rds"))


