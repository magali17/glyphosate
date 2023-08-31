

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

pacman::p_load(tidyverse, raster, 
               sf
               )

########################################################################################################################
# LOAD DATA ENVIRONMENT
########################################################################################################################
load(file.path("output", "analysis_datasets.rds"))

########################################################################################################################
# COMMON VARIABLES
########################################################################################################################
# NHANES CRS
# --> update
lat_long_crs <- 4326 #WGS84
m_crs <- 32148

cdl_resolution_m2 <-30^2 #m


fake_GEO_2010 <- fake_GEO_2010 %>%
  
  # --> need to do this??
  
  st_as_sf(coords = c("LONG", "LAT"), remove=F, crs=lat_long_crs) %>%
  # same CRS as CDL
  st_transform(st_crs(cdl_2013))


# --> TEST?? ERROR IN PROEJCTION OTHERWISE??
# crs(cdl_2013) <- m_crs
# projection(cdl_2013) <- m_crs


########################################################################################################################
# CDL AG PROXIMITY ANALYSES
########################################################################################################################
# list is from crop frequency metadata
corn <- c(1,225,226,228,237,241)
cotton <- c(2,232,238,239)
soybeans <- c(5,26,239,240,241,254)
wheat <- c(22,23,24,26,225,230,234,236,238)

# --> gmo vs gly desicated?
gmo_crops <- c(corn, cotton, soybeans,
               31, #canola
               36, #alfalfa
               41) #sugarbeets


# --> 
# wheat, oats
gly_desication <- c(wheat,
                    25, 226, 240, #oats
                    24, #dry beans
                    51:53, # chick peas, lentisl, peas
                    10, 76, # peanuts, walnuts
                    
                    # --> ??
                    31:34 # canola, flaxseed, safflower, rape seed
                    )

##############################################
# fn returns prop of a buffer with specifc crops
calculate_ag_in_buffer <- function(df, buffer.) {

  # x=buffers.[1]
  #lapply(buffers., function(x) {
         
         # --> ERROR: Error in .availableRAM(maxmem) : object '_raster_availableRAM' not found
         # --> WARNING: In sp::CRS(from$proj4string) : invalid PROJ4 string
         
         buffer_area_m2 <- pi*buffer.^2
         
         # only keep raster needed to speed things up
         temp <- raster::crop(cdl_2013, st_buffer(fake_GEO_2010, buffer.)) %>%
           #pull out cell values; return df
           raster::extract(., st_buffer(fake_GEO_2010, buffer.), df=TRUE) %>%
         
           # --> ??
           group_by(SEQN,
                    ID#, class
                    ) %>%
           
           summarize(n = n(),
                     #tot_area = area(.),
                     # --> ???
                     #n_cultivated =  = area(.@data@attributes[[1]] %>% as.data.frame() %>% filter(ID %in% cultivated_crops$id))
                     ) %>% 
           
           # --> ??
           group_by(SEQN) %>%
           
           summarize(cultivated = filter(., ID %in% cultivated_crops$id) %>% sum(.$n),
                     corn = filter(., ID %in% corn) %>% sum(.$n),
                     cotton = ,
                     soybeans = ,
                     wheat = ,
                     gmo_crops = ,
                     gly_desication =
                       ) %>%
           
           
           # --> 
           mutate_at(vars(-SEQN), ~.*cdl_resolution_m/buffer_area_m2) %>%
           mutate(buffer = buffer.) 
            
          
  
}

##############################################
buffers <- c(1e3,5e3,10e3)

lapply(buffers, function(x) calculate_ag_in_buffer(cdl_2013, buffer. = x) )


# ggplot(fake_GEO_2010) + geom_sf()














########################################################################################################################
# MERGE DATA
########################################################################################################################



