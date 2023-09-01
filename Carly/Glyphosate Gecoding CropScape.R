# Clear workspace
rm(list=ls())
library(tidyverse)
library(sf)
library(raster)
library(sp)
library(nngeo)
library(CropScapeR)
setwd("/Users/carlyhyland/Dropbox/Publication drafts/Pesticide geocoding/Data Analysis")


# Extract CropScape buffers --------------------------------------------------------

# Import CropScape data 
CS_Idaho <- GetCDLData(aoi = '16', year = 2020, type = 'f') #Idaho 

#CS_buffer_pts <- sf::st_read("/Users/carlyhyland/Dropbox/Publication drafts/Pesticide geocoding/Data Analysis/Shapefile/points_buffer.shp")
#CS_buffer_pts_sp <- GetCDLData(aoi = sf::st_bbox(CS_buffer_pts), year = '2020', type = 'b')
#CS_buffer_shape <- raster::mask(CS_buffer_pts_sp, CS_buffer_pts)





addresses<-read_csv("/Users/carlyhyland/Dropbox/Postdoc/Organic Diet Study/Groundtruthing/Addresses and Fields/lat_long.csv")
nampa<-filter(addresses, Identifier=="Nampa")
twin<-filter(addresses, Identifier=="Twin")


points <- addresses %>% st_as_sf(coords = c("Long", "Lat"), 
                                 crs = 4269, 
                                 na.fail = FALSE) %>%
  tibble::rowid_to_column("ID") # add ID column to join pixels back to

plot(points)




# Buffer each point 
points_buffer <- points %>% 
  st_transform(crs = st_crs(CS_Idaho)) %>% # convert crs to match NLCD data
  st_buffer(500) 

#st_write(points_buffer, "Shapefile/points_buffer.shp") #creates a shapefile for me to then open in ArcGIS that I can look at 

# Plot to take a look at buffered points (they are now stored as polygons, not points)
plot(points_buffer)

### Extract cell values within each of the buffered points
# First crop to the general area to speed up processing
points_crop <- crop(CS_Idaho, points_buffer, df = FALSE)
plot(points_crop)
plot(points_buffer, add = TRUE)

# Only extract cell values (produces dataframe rather than a raster grid)
# The ID column indicates which polygon, the long name column tells you the cell value
points_extracted_df <- raster::extract(points_crop, points_buffer, df = TRUE) %>%
  rename(class = CDL_2020_16)

# Make a new raster grid with only cell values within each buffer
points_extracted_rast <- mask(points_crop, points_buffer)


# Visualize the rasters as well as the areas they have been bounded in
plot(points_extracted_rast)
#writeRaster(twin_points_extracted_rast, "Twin Points CropScape.tif")
plot(points_buffer, add=TRUE)

# Calculate proportion of buffer that is cultivated crops -----------------

#Check if any double-cropped land and particular crops comes up in Idaho; if not, we can exclude it
# crop_pixels_double <- points_extracted_df %>%
#   group_by(ID, class) %>% 
#   summarise(n_crop = n()) %>% filter(class == 12 #Sweet corn
#                                      | class==13 #Popcorn and ornamental corn
#                                      | class==24 #winter wheat
#                                      | class==26 #Dbl Crop WinWht/Soybeans 
#                                      | class==38 #Camelina 
#                                      | class==39 #buckwheat
#                                      | class==224 #vetch
#                                      | class==226
#                                      | class==227
#                                      | class==228
#                                      | class==229
#                                      | class==230
#                                      | class==231
#                                      | class==232
#                                      | class==233
#                                      | class==234
#                                      | class==235
#                                      | class==236
#                                      | class==237
#                                      | class==238
#                                      | class==239
#                                      | class==240
#                                      | class==241
#                                      | class==254)  

crop_pixels_gmo <- points_extracted_df %>%
  group_by(ID, class) %>% 
  summarise(n_crop = n()) %>% filter(class == 1 #corn 
                                     | class==2 #Cotton 
                                     | class==5 #soybeans
                                     | class==31 #canola 
                                     | class==36 #alfalfa
                                     | class==41) #sugarbeets

crop_pixels_total <- points_extracted_df %>%
  group_by(ID, class) %>% 
  summarise(n_crop = n()) %>% filter(class == 3 #rice 
                                     | class==4 #sorghum
                                     | class==6 #unflower
                                     | class==10 #peanuts
                                     | class==11 #tobacco
                                     | class==14 #mint
                                     | class==32 #flaxseed
                                     | class==33 #safflower
                                     | class==34 #rape seed  
                                     | class==35 #mustard
                                     | class==42 #dry beans
                                     | class==43 #potatoes
                                     | class==44 #other crops
                                     | class==45 #sugarcane
                                     | class==46 #sweet potatoes
                                     | class==47 #misc veggies
                                     | class==48 #watermelon
                                     | class==49 #onions
                                     | class==50 #cucumbers
                                     | class==51 #chickpeas
                                     | class==52 #lentils
                                     | class==53 #peas
                                     | class==54 #tomateos
                                     | class==55 #caneberries
                                     | class==56 #hops
                                     | class==57 #herbs
                                     | class==58 #clover/wildflowers
                                     | class==66 #cherries
                                     | class==67 #peaches
                                     | class==68 #apples
                                     | class==69 #grapes
                                     | class==72 #citrus
                                     | class==74 #pecans
                                     | class==75 #almonds
                                     | class==76 #walnuts
                                     | class==77 #pears
                                     | class==204 #pistachios
                                     | class==206 #carrots
                                     | class==207 #asparagus
                                     | class==208 #garlic 
                                     | class==209 #cantaloupe
                                     | class==210 #prunes 
                                     | class==211 #olives
                                     | class==212 #oranges
                                     | class==213 #honeydew 
                                     | class==214 #broccoli 
                                     | class==215 #avocado 
                                     | class==216 #peppers
                                     | class==217 #pomegranate
                                     | class==218 #nectarines 
                                     | class==219 #greens
                                     | class==220 #plums
                                     | class==221 #strawberries 
                                     | class==222 #squash
                                     | class==223 #apricots
                                     | class==242 #blueberries
                                     | class==243 #cabbage
                                     | class==244 #cauliflower
                                     | class==245 #celery 
                                     | class==246 #radishes
                                     | class==247 #turnips
                                     | class==248 #eggplants
                                     | class==249 #gourds
                                     | class==250 #cranberries
                                     | class==21 #barley 
                                     | class==22 #Durum wheat 
                                     | class==23 #spring wheat
                                     | class==25 #other small grains 
                                     | class==27 #rye
                                     | class==28 #oats
                                     | class==29 #millet
                                     | class==30 #speltz
                                     | class==205 #Triticale
                                     | class==1 #corn 
                                     | class==2 #Cotton 
                                     | class==5 #soybeans
                                     | class==31 #canola 
                                     | class==36 #alfalfa
                                     | class==41 #sugarbeets
                                     | class==12 #sweetcorn
                                     | class==13 #Popcorn and ornamental corn
                                     | class==24) #winter wheat
                                     

                                     
crop_pixels_idle <- points_extracted_df %>%
  group_by(ID, class) %>% 
  summarise(n_crop = n()) %>% filter(class == 61) #Fallow/idle land    

crop_pixels_hay <- points_extracted_df %>%
  group_by(ID, class) %>% 
  summarise(n_crop = n()) %>% filter(class == 37) #Other hay/non alfalfa 



all_pixels <- points_extracted_df %>%
  group_by(ID) %>%
  summarise(n = n())

crop_prop_gmo <- left_join(crop_pixels_gmo, all_pixels) %>%
  mutate(crop_prop = n_crop / n) %>% #Total crop pixels out of all pixels; can figure out total acreage by multiplying proportion with size of buffer 
  left_join(points) 


crop_prop_total <- left_join(crop_pixels_total, all_pixels) %>%
  mutate(crop_prop = n_crop / n) %>% #Total crop pixels out of all pixels; can figure out total acreage by multiplying proportion with size of buffer 
  left_join(points) 

crop_prop_idle <- left_join(crop_pixels_idle, all_pixels) %>%
  mutate(crop_prop = n_crop / n) %>% #Total crop pixels out of all pixels; can figure out total acreage by multiplying proportion with size of buffer 
  left_join(points) 

crop_prop_hay <- left_join(crop_pixels_hay, all_pixels) %>%
  mutate(crop_prop = n_crop / n) %>% #Total crop pixels out of all pixels; can figure out total acreage by multiplying proportion with size of buffer 
  left_join(points) 
# Calculate distance to nearest crop --------------------------------------

# Turn only raster cells with a value of any crops we want to include into polygons (cropped buffers only)
crop_polygons_gmo <- rasterToPolygons(points_extracted_rast, fun=function(x){x==1 | x==2 | x==5 | x==31 | x==36 | x==41}) %>%
  st_as_sf()

crs(crop_polygons_gmo)
st_crs(crop_polygons_gmo)

crop_polygons_total <- rasterToPolygons(points_extracted_rast, fun=function(x){x==1 | x==2 | x==5 | x==31 | x==36 | x==41 | x==22 | x==23 | x==25 | x==27 | x==28 | x==29 | x==30 | x==206 |
                                                                                  x==4 | x==6  | x==10  | x==11 | x==14 | x==32 | x==33 | x==35 | x==42 | x==43 | x==44 | x==45 | x==46 | x==47 |
                                                                                  x==48 | x==49 | x==50 | x==51 | x==52 | x==53 | x==54 | x==55 | x==56 | x==57 | x==58 | x==66 | x==67 | x==68 |
                                                                                  x==69 | x==72 | x==74 | x==75 | x==76 | x==77 | x==204 | x==206 | x==207 | x==208 | x==209 | x==210 | x==211 | 
                                                                                   x==212| x==213 | x==214 | x==216 | x==217 | x==218 | x==219 | x==220 | x==221 | x==223 | x==242 | x==243 | x==244 |
                                                                                  x==245 | x==246 | x==248 | x==249 | x==250 | x==21 | x==12 | x==13 | x==24}) %>%
   st_as_sf()

# Convert crs of Points
points_gmo <- points %>%
  st_transform(crs = st_crs(crop_polygons_gmo))

points_total <- points %>%
  st_transform(crs = st_crs(crop_polygons_total))



# Plot
plot(points_extracted_rast)
plot(points_gmo, add=TRUE, col = "red")
plot(points_total, add=TRUE, col = "red")



# Calculate distance from each house to nearest field 
#GMO
crop_dist_gmo <- st_nn(points_gmo, crop_polygons_gmo, 
                         k = 1, 
                         returnDist = T)

crop_dist_list_gmo <- lapply(crop_dist_gmo$dist, `[[`, 1) 

crop_dist_list_gmo2 <- do.call(rbind.data.frame, crop_dist_list_gmo) %>%
  tibble::rownames_to_column("ID") %>%
  mutate(ID = as.numeric(ID)) %>%
  as.data.frame() %>%
  left_join(points) 

colnames(crop_dist_list_gmo2)[2] <- "meters_to_crop" # change column name


#Total
crop_dist_total <- st_nn(points_total, crop_polygons_total, 
                            k = 1, 
                            returnDist = T)

crop_dist_list_total <- lapply(crop_dist_total$dist, `[[`, 1) 

crop_dist_list_total2 <- do.call(rbind.data.frame, crop_dist_list_total) %>%
  tibble::rownames_to_column("ID") %>%
  mutate(ID = as.numeric(ID)) %>%
  as.data.frame() %>%
  left_join(points) 

colnames(crop_dist_list_total2)[2] <- "meters_to_crop" # change column name








######Sensitivity Analysis 


# Clear workspace
rm(list=ls())
library(tidyverse)
library(sf)
library(raster)
library(sp)
library(nngeo)
library(CropScapeR)
setwd("/Users/carlyhyland/Dropbox/Publication drafts/Pesticide geocoding/Data Analysis")


# Extract CropScape buffers --------------------------------------------------------

# Import CropScape data 
CS_Idaho <- GetCDLData(aoi = '16', year = 2020, type = 'f') #Idaho 

#CS_buffer_pts <- sf::st_read("/Users/carlyhyland/Dropbox/Publication drafts/Pesticide geocoding/Data Analysis/Shapefile/points_buffer.shp")
#CS_buffer_pts_sp <- GetCDLData(aoi = sf::st_bbox(CS_buffer_pts), year = '2020', type = 'b')
#CS_buffer_shape <- raster::mask(CS_buffer_pts_sp, CS_buffer_pts)





addresses<-read_csv("/Users/carlyhyland/Dropbox/Postdoc/Organic Diet Study/Groundtruthing/Addresses and Fields/lat_long_sen2.csv")
nampa<-filter(addresses, Identifier=="Nampa")
twin<-filter(addresses, Identifier=="Twin")


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
points_buffer <- points %>% 
  st_transform(crs = st_crs(CS_Idaho)) %>% # convert crs to match NLCD data
  st_buffer(500) 

nampa_points_buffer <- nampa_points %>% 
  st_transform(crs = st_crs(CS_Idaho)) %>% # convert crs to match NLCD data
  st_buffer(500) 

twin_points_buffer <- twin_points %>% 
  st_transform(crs = st_crs(CS_Idaho)) %>% # convert crs to match NLCD data
  st_buffer(500) 

#st_write(points_buffer, "Shapefile/points_buffer.shp") #creates a shapefile for me to then open in ArcGIS that I can look at 

# Plot to take a look at buffered points (they are now stored as polygons, not points)
plot(points_buffer)

### Extract cell values within each of the buffered points
# First crop to the general area to speed up processing
points_crop <- crop(CS_Idaho, points_buffer, df = FALSE)
plot(points_crop)
plot(points_buffer, add = TRUE)



nampa_points_crop <- crop(CS_Idaho, nampa_points_buffer, df = FALSE)
plot(nampa_points_crop)
plot(nampa_points_buffer, add = TRUE)

twin_points_crop <- crop(CS_Idaho, twin_points_buffer, df = FALSE)
plot(twin_points_crop)
plot(twin_points_buffer, add = TRUE)

# Only extract cell values (produces dataframe rather than a raster grid)
# The ID column indicates which polygon, the long name column tells you the cell value
points_extracted_df <- raster::extract(points_crop, points_buffer, df = TRUE) %>%
  rename(class = CDL_2020_16)


nampa_points_extracted_df <- raster::extract(nampa_points_crop, nampa_points_buffer, df = TRUE) %>%
  rename(class = CDL_2020_16)

twin_points_extracted_df <- raster::extract(twin_points_crop, twin_points_buffer, df = TRUE) %>%
  rename(class = CDL_2020_16)

# Make a new raster grid with only cell values within each buffer
points_extracted_rast <- mask(points_crop, points_buffer)

nampa_points_extracted_rast <- mask(nampa_points_crop, nampa_points_buffer)

twin_points_extracted_rast <- mask(twin_points_crop, twin_points_buffer)


# Visualize the rasters as well as the areas they have been bounded in
plot(nampa_points_extracted_rast)
writeRaster(nampa_points_extracted_rast, "Nampa Points CropScape_sen2.tif")
plot(nampa_points_buffer, add=TRUE)


plot(twin_points_extracted_rast)
writeRaster(twin_points_extracted_rast, "Twin Points CropScape_sen2.tif")
plot(twin_points_buffer, add=TRUE)
