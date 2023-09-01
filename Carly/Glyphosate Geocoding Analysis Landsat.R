# Clear workspace
rm(list=ls())

library(tidyverse)
library(sf)
library(raster)
library(sp)
library(nngeo)


# Import Landsat Scenes
setwd("/Users/carlyhyland/Dropbox/Publication drafts/Pesticide geocoding/Data Analysis")
#landsat_nampa <- raster("LandSat/Landsat Scenes/NDVI_Nampa_Threshold_GEE.tif")
#landsat_twin <- raster("LandSat/Landsat Scenes/NDVI_Twin_Threshold_GEE.tif")
landsat_nampa <- raster("LandSat/Landsat Scenes/NDVI_Nampa_Threshold_90.tif")
landsat_twin <- raster("LandSat/Landsat Scenes/NDVI_Twin_Threshold_90.tif")

plot(landsat_nampa)
plot(landsat_twin)


###Plot Points
#addresses<-read_csv("/Users/carlyhyland/Dropbox/Postdoc/Organic Diet Study/Groundtruthing/Addresses and Fields/lat_long.csv")
nampa<-filter(addresses, Identifier=="Nampa")
twin<-filter(addresses, Identifier=="Twin")

addresses<-read_csv("/Users/carlyhyland/Dropbox/Postdoc/Organic Diet Study/Groundtruthing/Addresses and Fields/lat_long_2_2.csv")


points <- addresses %>% st_as_sf(coords = c("Long", "Lat"), 
                                 crs = 4269, 
                                 na.fail = FALSE) %>%
  tibble::rowid_to_column("ID") # add ID column to join pixels back to

plot(points)



nampa_points <- nampa %>% st_as_sf(coords = c("Long", "Lat"), 
                                   crs = 4269, 
                                   na.fail = FALSE) %>%
  tibble::rowid_to_column("ID") # add ID column to join pixels back to

plot(nampa_points)


twin_points <- twin %>% st_as_sf(coords = c("Long", "Lat"), 
                                 crs = 4269, 
                                 na.fail = FALSE) %>%
  tibble::rowid_to_column("ID") # add ID column to join pixels back to

plot(twin_points)


# Buffer each point 
nampa_points_buffer <- nampa_points %>% 
  st_transform(crs = st_crs(landsat_nampa)) %>% 
  st_buffer(500) 


twin_points_buffer <- twin_points %>% 
  st_transform(crs = st_crs(landsat_twin)) %>% 
  st_buffer(500) 


points_buffer <- points %>% 
  st_transform(crs = st_crs(landsat_twin)) %>% 
  st_buffer(500) 

# Plot to take a look at buffered points (they are now stored as polygons, not points)
plot(nampa_points_buffer)
plot(twin_points_buffer)
plot(points_buffer)

### Extract Landsat cell values within each of the buffered points
nampa_points_crop <- crop(landsat_nampa, nampa_points_buffer, df = FALSE)
plot(nampa_points_crop)
plot(nampa_points_buffer, add = TRUE)

twin_points_crop <- crop(landsat_twin, twin_points_buffer, df = FALSE)
plot(twin_points_crop)
plot(twin_points_buffer, add = TRUE)

points_crop <- crop(landsat_twin, points_buffer, df = FALSE)
plot(points_crop)
plot(points_buffer, add = TRUE)

# Only extract cell values (produces dataframe rather than a raster grid)
# The ID column indicates which polygon, the long name column tells you the cell value
nampa_points_extracted_df <- extract(nampa_points_crop, nampa_points_buffer, df=TRUE) %>%
  rename(class = NDVI_Nampa_Threshold_90)

twin_points_extracted_df <- extract(twin_points_crop, twin_points_buffer, df = TRUE) %>%
  rename(class = NDVI_Twin_Threshold_90)

points_extracted_df <- extract(points_crop, points_buffer, df = TRUE) %>%
  rename(class = NDVI_Twin_Threshold_90)

# Make a new raster grid with only cell values within each buffer
nampa_points_extracted_rast <- mask(nampa_points_crop, nampa_points_buffer)
twin_points_extracted_rast <- mask(twin_points_crop, twin_points_buffer)
points_extracted_rast <- mask(points_crop, points_buffer)


# Visualize the rasters as well as the areas they have been bounded in
plot(nampa_points_extracted_rast)
writeRaster(nampa_points_extracted_rast, "Nampa Points Landsat_90.tif")
plot(nampa_points_buffer, add=TRUE)

plot(twin_points_extracted_rast)
writeRaster(twin_points_extracted_rast, "Twin Points Landsat_90.tif")
plot(twin_points_buffer, add=TRUE)

plot(points_extracted_rast)
writeRaster(points_extracted_rast, "Twin Points Landsat_90.tif")
plot(points_buffer, add=TRUE)

# Calculate proportion of buffer that is cultivated crops -----------------

nampa_crop_pixels <- nampa_points_extracted_df %>%
  group_by(ID, class) %>% 
  summarise(n_crop = n()) %>% filter(class == 1)


nampa_all_pixels <- nampa_points_extracted_df %>%
  group_by(ID) %>%
  summarise(n = n())

nampa_crop_prop <- left_join(nampa_crop_pixels, nampa_all_pixels) %>%
  mutate(nampa_crop_prop = n_crop / n) %>% #Total crop pixels out of all pixels; can figure out total acreage by multiplying proportion with size of buffer 
  left_join(nampa_points) 



twin_crop_pixels <- twin_points_extracted_df %>%
  group_by(ID, class) %>% #here I  might want to sumnmarize by Particpant ID instead 
  summarise(n_crop = n()) %>% filter(class == 1)

twin_all_pixels <- twin_points_extracted_df %>%
  group_by(ID) %>%
  summarise(n = n())

twin_crop_prop <- left_join(twin_crop_pixels, nampa_all_pixels) %>%
  mutate(twin_crop_prop = n_crop / n) %>% #Total crop pixels out of all pixels; can figure out total acreage by multiplying proportion with size of buffer 
  left_join(twin_points) 


# Calculate distance to nearest crop --------------------------------------

# Turn only raster cells with a value of 1 (crops) into polygons (cropped buffers only)
nampa_crop_polygons <- rasterToPolygons(nampa_points_extracted_rast, fun=function(x){x==1}) %>%
  st_as_sf()

twin_crop_polygons <- rasterToPolygons(twin_points_extracted_rast, fun=function(x){x==1}) %>%
  st_as_sf()

# Convert crs of Nampa Points
nampa_points2 <- nampa_points %>%
  st_transform(crs = st_crs(nampa_crop_polygons))

twin_points2 <- twin_points %>%
  st_transform(crs = st_crs(twin_crop_polygons))


# Plot
plot(nampa_points_extracted_rast)
plot(nampa_points2, add=TRUE, col = "red")

plot(twin_points_extracted_rast)
plot(twin_points2, add=TRUE, col = "red")

# Calculate distance from each building to the nearest state road
nampa_crop_dist <- st_nn(nampa_points2, nampa_crop_polygons, 
                         k = 1, 
                         returnDist = T)

nampa_crop_dist_list <- lapply(nampa_crop_dist$dist, `[[`, 1) 

nampa_crop_dist_list2 <- do.call(rbind.data.frame, nampa_crop_dist_list) %>%
  tibble::rownames_to_column("ID") %>%
  mutate(ID = as.numeric(ID)) %>%
  as.data.frame() %>%
  left_join(nampa_points) 

colnames(nampa_crop_dist_list2)[2] <- "meters_to_crop" # change column name


twin_crop_dist <- st_nn(twin_points2, twin_crop_polygons, 
                        k = 1, 
                        returnDist = T)

twin_crop_dist_list <- lapply(twin_crop_dist$dist, `[[`, 1) 

twin_crop_dist_list2 <- do.call(rbind.data.frame, twin_crop_dist_list) %>%
  tibble::rownames_to_column("ID") %>%
  mutate(ID = as.numeric(ID)) %>%
  as.data.frame() %>%
  left_join(twin_points) 

colnames(twin_crop_dist_list2)[2] <- "meters_to_crop" # change column name




#write.csv(nampa_crop_dist_list2,"LandSat/Nampa Distance.csv", row.names = TRUE)
#write.csv(nampa_crop_prop,"LandSat/Nampa Proportion.csv", row.names = TRUE)

#write.csv(twin_crop_dist_list2,"LandSat/Twin Distance.csv", row.names = TRUE)
#write.csv(twin_crop_prop,"LandSat/Twin Proportion.csv", row.names = TRUE)

write.csv(nampa_crop_dist_list2,"LandSat/Nampa Distance_90.csv", row.names = TRUE)
write.csv(nampa_crop_prop,"LandSat/Nampa Proportion_90.csv", row.names = TRUE)

write.csv(twin_crop_dist_list2,"LandSat/Twin Distance_90.csv", row.names = TRUE)
write.csv(twin_crop_prop,"LandSat/Twin Proportion_90.csv", row.names = TRUE)















#######Sensitivity Analysis


# Clear workspace
rm(list=ls())

library(tidyverse)
library(sf)
library(raster)
library(sp)
library(nngeo)


# Import Landsat Scenes
setwd("/Users/carlyhyland/Dropbox/Publication drafts/Pesticide geocoding/Data Analysis")
landsat_nampa <- raster("LandSat/Landsat Scenes/NDVI_Nampa_Threshold_GEE.tif")
landsat_twin <- raster("LandSat/Landsat Scenes/NDVI_Twin_Threshold_GEE.tif")
#landsat_nampa <- raster("LandSat/Landsat Scenes/NDVI_Nampa_Threshold_90.tif")
#landsat_twin <- raster("LandSat/Landsat Scenes/NDVI_Twin_Threshold_90.tif")

plot(landsat_nampa)
plot(landsat_twin)


###Plot Points
addresses<-read_csv("/Users/carlyhyland/Dropbox/Postdoc/Organic Diet Study/Groundtruthing/Addresses and Fields/lat_long_sen2.csv")
nampa<-filter(addresses, Identifier=="Nampa")
twin<-filter(addresses, Identifier=="Twin")

nampa_points <- nampa %>% st_as_sf(coords = c("Long", "Lat"), 
                                   crs = 4269, 
                                   na.fail = FALSE) %>%
  tibble::rowid_to_column("ID") # add ID column to join pixels back to

plot(nampa_points)


twin_points <- twin %>% st_as_sf(coords = c("Long", "Lat"), 
                                 crs = 4269, 
                                 na.fail = FALSE) %>%
  tibble::rowid_to_column("ID") # add ID column to join pixels back to

plot(twin_points)


# Buffer each point 
nampa_points_buffer <- nampa_points %>% 
  st_transform(crs = st_crs(landsat_nampa)) %>% 
  st_buffer(500) 


twin_points_buffer <- twin_points %>% 
  st_transform(crs = st_crs(landsat_twin)) %>% 
  st_buffer(500) 



# Plot to take a look at buffered points (they are now stored as polygons, not points)
plot(nampa_points_buffer)
plot(twin_points_buffer)

### Extract Landsat cell values within each of the buffered points
nampa_points_crop <- crop(landsat_nampa, nampa_points_buffer, df = FALSE)
plot(nampa_points_crop)
plot(nampa_points_buffer, add = TRUE)

twin_points_crop <- crop(landsat_twin, twin_points_buffer, df = FALSE)
plot(twin_points_crop)
plot(twin_points_buffer, add = TRUE)

# Only extract cell values (produces dataframe rather than a raster grid)
# The ID column indicates which polygon, the long name column tells you the cell value
nampa_points_extracted_df <- extract(nampa_points_crop, nampa_points_buffer, df=TRUE) %>%
  rename(class = NDVI_Nampa_Threshold_90)

twin_points_extracted_df <- extract(twin_points_crop, twin_points_buffer, df = TRUE) %>%
  rename(class = NDVI_Twin_Threshold_90)

# Make a new raster grid with only cell values within each buffer
nampa_points_extracted_rast <- mask(nampa_points_crop, nampa_points_buffer)
twin_points_extracted_rast <- mask(twin_points_crop, twin_points_buffer)



# Visualize the rasters as well as the areas they have been bounded in
plot(nampa_points_extracted_rast)
writeRaster(nampa_points_extracted_rast, "Nampa Points Landsat_sen2.tif")
plot(nampa_points_buffer, add=TRUE)

plot(twin_points_extracted_rast)
writeRaster(twin_points_extracted_rast, "Twin Points Landsat_sen2.tif")
plot(twin_points_buffer, add=TRUE)
