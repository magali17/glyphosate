#DISTANCE TO AG

distance_to_ag <- function(crop_type, locations_dt, raster_dt, max_distance=2e3) {
  # give locations the same CRS as the raster & make spatial objects for raster package functions
  locations_dt <- locations_dt %>%
    # same CRS as raster 
    st_transform(st_crs(raster_dt))  
  
  # --> NEED TO DO??
  # # raster::crop() requires spatial (not sf) objects
  # as_Spatial()
  
  # raster_df <- as.data.frame(raster_dt, xy=TRUE)
  # 
  # distanceFromPoints(raster_dt, locations_dt)
  
  
  # --> START HERE - TAKING TOO LONG; convert to polygon before sending over? 
  
  # ?? use test <- raster::extract(raster_dt, locations_dt, buffer=2e3, df=TRUE)
  
  nearby_land <- raster::crop(raster_dt, st_buffer(locations_dt, max_distance)) %>%
    #polygons for each land type
    rasterToPolygons(dissolve = T) %>% 
    st_as_sf()
  
  # plot(nearby_land)
  
  
  #distance to land types
  ## distanceFromPoints
  pt_distances <- st_distance(nearby_land, locations_dt)
  
  result <- pt_distances %>%
    as.data.frame()
  
  return(dist_to_ag)
}


# raster_dt[1:1e3,1:1e3]
# mask(raster_dt)
# raster_dt[!raster_dt %in% get("cultivated")] <- NA
# values(raster_dt)

raster_df <- as.data.frame(raster_dt, xy=TRUE)

ggplot() +
  geom_raster(data = raster_dt) + 
  #geom_raster(data = , aes(x = x, y = y, fill = ID)) + 
  geom_sf(data = locations_dt, fill = NA) +
  coord_sf()
