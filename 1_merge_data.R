

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

knitr::opts_chunk$set(echo = F, 
                      cache=F, cache.comments = F, 
                      message = F, warning = F, 
                      tidy.opts=list(width.cutoff=60), tidy=TRUE,
                      fig.height = 8, fig.width = 8)  

pacman::p_load(tidyverse,  
               sf,
               raster #, nngeo::st_nn #e.g. distance to nearest ag field?
               )

########################################################################################################################
# LOAD DATA ENVIRONMENT
########################################################################################################################
load(file.path("output", "analysis_datasets.rds"))

#????
# test <- cdl_2013
# raster::crs(test) <- 9822 #EPSG code for Albers Equal Area Projection
# raster::plot(test)


########################################################################################################################
# COMMON VARIABLES
########################################################################################################################
# NHANES CRS
# --> update
lat_long_crs <- 4326 #WGS84
m_crs <- 32148

cdl_pixel_resolution_km2 <-(30/1e3)^2 #km

# --> update to be for GLY data? or entire NHANES sample for later comparison of urinary representativeness vs that year's sample?
# --> why does DEMO_H have ~10k rows??
gly_seqn <- c(
  unique(nhanes2013$SSGLYP_I$SEQN),
  unique(nhanes2015$SSGLYP_I$SEQN)#,
  
  #select(nhanes2017$SSGLYP_J, SEQN)
)

# --> OK that project crs becomes "unnamed"??

fake_GEO_2010 <- fake_GEO_2010 %>%
  
  # --> need to do this??
  
  st_as_sf(coords = c("LONG", "LAT"), remove=F, crs=lat_long_crs) %>%
  # same CRS as CDL, which is in meters
  st_transform(st_crs(cdl_2013)) #%>%
  
  # --> can RDC Do this??
  #filter(SEQN %in% gly_seqn)


# --> TEST?? ERROR IN PROEJCTION OTHERWISE??
# crs(cdl_2013) <- m_crs
# projection(cdl_2013) <- m_crs


  
  
  
########################################################################################################################
# CDL AG PROXIMITY ANALYSES
########################################################################################################################
cultivated <- cultivated_crops$id
# list is from crop frequency metadata
corn <- c(1,225,226,228,237,241)
cotton <- c(2,232,238,239)
soybeans <- c(5,26,239,240,241,254)
wheat <- c(22,23,24,26,225,230,234,236,238)

# --> gmo vs gly desicated?
gmo <- c(corn, cotton, soybeans,
               31, #canola
               36, #alfalfa
               41) #sugarbeets


# --> 
# wheat, oats
desication <- c(wheat,
                    25, 226, 240, #oats
                    24, #dry beans
                    51:53, # chick peas, lentisl, peas
                    10, 76, # peanuts, walnuts
                    
                    # --> ??
                    31:34 # canola, flaxseed, safflower, rape seed
                    )

############################################################################################
# fn returns dataframe with crop frequencies within a buffered location

# raster_dt=cdl_2013
# locations_dt = fake_GEO_2010[1:10,]
# buffer. = 500

crop_frequency_in_buffer <- function(raster_dt, locations_dt, buffer.) {
  #make names the same across different datasets
  names(raster_dt) <- "land_id"
  
  # create a df with land info for a given location & buffer
  crop_frequency <- raster::extract(raster_dt, locations_dt, buffer=buffer., df=TRUE) %>%
    as_tibble() %>% 
    # frequency of crops in each buffered location
    group_by_all() %>% 
    summarize(pixel_count=n()) %>%
    ungroup() %>%
    mutate(buffer_m = buffer.)
  
  return(crop_frequency)
}

############################################################################################
# function returns the total number of a specific crop for a given buffered area

# df = crop_frequency
# keep_crops = cultivated_crops$id
# crop_label = "cultivated"
# pixel_resolution = cdl_pixel_resolution_km2 # area each pixel represents

crop_total_in_buffer <- function(df, keep_crops, crop_label, pixel_resolution) {
  # total crop area
  crop_total <- df %>%
    filter(land_id %in% keep_crops) %>%
    group_by(ID, buffer_m) %>%
    summarize(tot_pixels = sum(pixel_count)) %>%
    #mutate(land_area = tot_pixels*pixel_resolution) %>%
    #select(ID, buffer_m, land_area) %>%
    mutate(land_label = crop_label)
  
  # #note: only locations with the indicated land type are returned. If missing, values are 0.
  # crop_total <- left_join(ids, crop_total) %>%
  #   mutate(land_area = ifelse(is.na(land_area), 0, land_area),
           
   #        ) #%>%
    # cbind(SEQN=nhanes_seqn, .) %>%
    # select(SEQN, land_area)
     
  return(crop_total)          
  
}

############################################################################################
# calculate total number of agricultral crops near a location
buffers <- c(500, 1e3#,5e3,10e3
             )
crops <- c("cultivated", "corn", "cotton", "soybeans", "wheat", "gmo", "desication")

#lapply(crops, get)

# b=buffers[1]
# calculate the frequency of cropped land at buffered locations
crop_frequency <- lapply(buffers, function(b)  {
  
  crop_frequency_in_buffer(raster_dt = cdl_2013,
                           locations_dt = fake_GEO_2010[1:10,], 
                           buffer.=b)
  }) %>%
  bind_rows()


# --> TOO COMPLICATED FOR RDC TO RUN???

buffer_char <- max(nchar(buffers))

crop_frequency <- crop_frequency %>%
  mutate(buffer_m = str_pad(string=buffer_m, width=buffer_char, side="left", pad="0"))

# crop_frequency %>% filter(ID == first(ID)) %>% View()

# c=crops[2]
crop_totals <- lapply(crops, function(c) {
  
  crop_total_in_buffer(df=crop_frequency, 
                       keep_crops = get(c), crop_label = c, 
                       pixel_resolution = cdl_pixel_resolution_km2
                       )
  }) %>%
  bind_rows()


# # drop to simply RDC code
# crop_totals2 <- crop_totals %>%
#   mutate(land_type = paste0(land_type, "_", buffer_m)) %>%
#   select(-buffer_m) %>%
#   pivot_wider(names_from = land_type, values_from = tot_pixels)
# 
# crop_totals2



# --> START HERE...OR SEE NOTES ABOVE

# --> add NHANES SEQN (vs ID) to results
# --> upate IDs to be just for GLY? vs 2013-2018?









########################################################################################################################
# MERGE DATA
########################################################################################################################



