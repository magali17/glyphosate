

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

# install.packages("rgdal")
pacman::p_load(raster, #rgdal,
               tidyverse,
               nhanesA, # retrieve NHANES data
               sf
               )

########################################################################################################################
# NHANES DATA
########################################################################################################################
# nhanesCodebook('DEMO_D', 'RIAGENDR')
# nhanesTables('LAB', 2013)

# # get file names 
# dem_t_name <- nhanesTables("DEMO", 2013) %>% pull(Data.File.Name)
# diet_t_names <- nhanesTables("DIET", 2013) %>%
#   filter(grepl("Dietary Interview", Data.File.Description),
#          grepl("First Day", Data.File.Description)) #%>% pull(Data.File.Name)
# exam_t_names <- nhanesTables("EXAM", 2013) %>%
#   filter(grepl("Body Measures", Data.File.Description)) #%>% pull(Data.File.Name)
# lab_t_names <- nhanesTables("LAB", 2013) %>% arrange(Data.File.Name) %>%
#   filter(grepl("Albumin & Creatinine - Urine|Glyphosate", Data.File.Description)) #%>% pull(Data.File.Name)
# q_t_names <- nhanesTables("Q", 2013) %>% arrange(Data.File.Name) %>%
#   filter(grepl("Alcohol Use|Current Health Status|Occupation|Pesticide Use|Smoking - Cigarette Use", Data.File.Description)) #%>% pull(Data.File.Name)

survey_year_codes <- c("H", "I") #2013-2014, 2015-2016

table_prefixes <- c("SSGLYP", #glyphosate
                    "DEMO", #demographics
                   "BMX", #bmi
                   "ALB_CR", #creatinine 
                   "ALQ", #alcohol
                   "HSQ", #health
                   "OCQ", #occupation
                   "PUQMEC", #pesticides
                   "SMQ" #smoke
                   )

table_names <- lapply(survey_year_codes, function(x) paste(table_prefixes, x, sep = "_"))

nhanes2013 <- lapply(table_names[[1]], function(x) {nhanes(x)})
names(nhanes2013) <- table_names[[1]]

nhanes2015 <- lapply(table_names[[2]], function(x) {nhanes(x)})
names(nhanes2015) <- table_names[[2]]


diet_table_prefixes <- c("DRXFCD",#food codes
                         "DR1IFF", #foods consumed
                         "DR1TOT" #total nutrients
                         )
diet_table_names <- lapply(survey_year_codes, function(x) paste(diet_table_prefixes, x, sep = "_"))

diet2013 <- lapply(diet_table_names[[1]], function(x) {nhanes(x)})
names(diet2013) <- diet_table_names[[1]]

diet2015 <- lapply(diet_table_names[[2]], function(x) {nhanes(x)})
names(diet2015) <- diet_table_names[[2]]


########################################################################################################################
# ADI
########################################################################################################################
# ADI is for 2015
adi0 <- read.csv(file.path("data", "raw", "adi", "US_2015_ADI_Census Block Group_v3.1.txt"), 
                 #don't drop leading 0s
                 colClasses = c(rep("character", 5))) 

adi <- adi0 %>%
  # mutate(
  #   # drop indicators of missingness # don't do now until later?
  #   ADI_NATRANK = ifelse(ADI_NATRANK %in% as.character(1:100), as.numeric(ADI_NATRANK), NA),
  #   ADI_STATERNK = ifelse(ADI_STATERNK %in% as.character(1:10), as.numeric(ADI_STATERNK), NA)) %>%
  #rename to match NHANES
  select(BG2KX = FIPS, adi_natrank=ADI_NATRANK, adi_staternk = ADI_STATERNK)
 
rm(adi0) 
write.csv(adi, file.path("data", "modified", "adi.csv"), row.names = F)

# adi <- read.csv(file.path("data", "modified", "adi.csv"))
########################################################################################################################
# COUNTY GLYPHOSATE
########################################################################################################################
keep_county_yrs <- c(2013:2016)

county_gly0 <- read.delim(file.path("data", "raw", "county_pesticides", "EPest_county_estimates_2013_2017_v2.txt")) 

county_gly <- county_gly0 %>%
  filter(COMPOUND == "GLYPHOSATE",
         YEAR %in% keep_county_yrs) %>% 
  #take avg of year pairs
  mutate(
    YEAR = ifelse(YEAR %in% 2013:2014, 2013, 
                  ifelse(YEAR %in% 2015:2016, 2015, NA))
  ) %>%
  group_by(YEAR, STATE_FIPS_CODE, COUNTY_FIPS_CODE) %>%
  summarize(EPEST_LOW_KG = mean(EPEST_LOW_KG),
            EPEST_HIGH_KG = mean(EPEST_HIGH_KG)) %>%
  rowwise() %>%
  mutate(
    # avg pesticide estimate - low/high estimates are typically very similar anyways
    county_gly_mean_kg = mean(c(EPEST_LOW_KG, EPEST_HIGH_KG), na.rm=T)) %>%
  ungroup() %>%
  select(-c(EPEST_LOW_KG, EPEST_HIGH_KG)) %>%  
  pivot_wider(names_from = YEAR, values_from = county_gly_mean_kg, names_prefix = "county_gly_kg_") %>%
  
  # rename to match GEO_2010 merging variables
  rename(
    STATE2KX = STATE_FIPS_CODE,
    CNTY2KX = COUNTY_FIPS_CODE)
  
rm(county_gly0)

# lapply(keep_county_yrs, function(x) {
#   county_gly %>%
#     filter(YEAR==x) %>%
#     select(-YEAR)
#   
# })

# --> save in wide format??
write.csv(county_gly, file.path("data", "modified", "county_gly.csv"), row.names = F)

# county_gly <- read.csv(file.path("data", "modified", "county_gly.csv"))
########################################################################################################################
# CDL
########################################################################################################################
# goals: (1) distance to the nearest agricultural field [cultivated crops], and 
#        (2) the total acreage of agricultural fields within a 0.5, 5.0 km buffer.

#pacman::p_load(raster)

cdl_path <- file.path("data", "raw", "cdl")
 

# --> WARNING: In sp::CRS(from$proj4string) : invalid PROJ4 string

cdl_2013 <- raster(file.path(cdl_path, "2013_30m_cdls","2013_30m_cdls.tif"))
cdl_2015 <- raster(file.path(cdl_path, "2015_30m_cdls","2015_30m_cdls.tif"))
cultivated_crops <- read.delim(file.path(cdl_path, "cultivated_crops.txt"), skip = 1, col.names = c("class")) %>%
  rownames_to_column(var="id") 

# --> keep?
cdl_corn_frequency <-raster(file.path(cdl_path, "Crop_Frequency_2008-2022","crop_frequency_corn_2008-2022.tif"))
cdl_cotton_frequency <-raster(file.path(cdl_path, "Crop_Frequency_2008-2022","crop_frequency_cotton_2008-2022.tif"))
cdl_soybeans_frequency <-raster(file.path(cdl_path, "Crop_Frequency_2008-2022","crop_frequency_soybeans_2008-2022.tif"))
cdl_wheat_frequency <-raster(file.path(cdl_path, "Crop_Frequency_2008-2022","crop_frequency_wheat_2008-2022.tif"))


# # plot(cdl_2013)
# 
# # cdl_2013@data@attributes
# # cdl_2013@crs
# # cdl_2013@legend #256 ?colors
# # cdl_2013@data

########################################################################################################################
# NLCD
########################################################################################################################
# #install.packages("FedData") # ERROR -  keep getting restart messages; 
# # devtools::install_github("ropensci/FedData") # EROR - still have installation issues
# pacman::p_load(FedData)
# nlcd_2016 <- get_nlcd(year=2016, dataset = "landcover")


# # --> ERROR LOADING
# nlcd_path <- file.path("data", "raw", "nlcd")
# nlcd_2016 <- raster(file.path(nlcd_path, "nlcd_2016_land_cover_l48_20210604.ige"))

###### still having issues downloading FedData
# pacman::p_load(FedData,tigris)

#####in Brain
#pacman::p_load("DevTools") # terra, ‘units’ ‘terra’ ‘raster’ ‘sf’

# in plasmid:
# ERROR: Error in curl::curl_fetch_disk(url, x$path, handle = handle) : transfer closed with outstanding read data remaining

# 8/31/23: works in plasmid!
nlcd <- get_nlcd(
  template = tigris::nation(year = 2021), # "Retrieving data for the year 2021"
  label = '2016 USA', 
  year = 2016, 
  dataset = 'landcover',
  extraction.dir = file.path("output", "nlcd_us_2916")
)


########################################################################################################################
# GENERATE FAKE NHANES LOCATIONS & RURALITY FOR CODE PREP
########################################################################################################################
# ALTERNATIVE: https://cran.r-project.org/web/packages/tigris/tigris.pdf
# bg <- tigris::block_groups(year=2010, state=NULL, cb=TRUE)

#lat_long_crs <- 4326 #WGS84

pacman::p_load(maps)
data(us.cities)

us.cities <- us.cities %>%
  mutate(rurality = ifelse(pop<quantile(pop, 0.20), "R", "U"))

gly_ids <- c(nhanes2013$SSGLYP_H$SEQN, 
                   nhanes2015$SSGLYP_I$SEQN)

lat_long_sample_indeces <- sample(x = 1:nrow(us.cities), size = length(gly_ids), replace = T)
state_county_sample_indeces <- sample(x = 1:nrow(county_gly), size = length(gly_ids), replace = T)
block_group_sample_indeces <- sample(x = 1:nrow(adi), size = length(gly_ids), replace = T)
  
fake_GEO_2010 <- data.frame(SEQN = gly_ids,
                            LONG = us.cities$long[lat_long_sample_indeces], #x
                            LAT = us.cities$lat[lat_long_sample_indeces],#y
                            STATE2KX = county_gly$STATE2KX[state_county_sample_indeces],
                            CNTY2KX = county_gly$CNTY2KX[state_county_sample_indeces],
                            UR2KX = us.cities$rurality[lat_long_sample_indeces] %>% as.character(),
                            BG2KX = adi$BG2KX[block_group_sample_indeces] %>% as.character()
                            ) #%>% st_as_sf(coords = c("LONG", "LAT"), remove=F, crs=lat_long_crs)

########################################################################################################################
# SAVE DATA FILES
########################################################################################################################
save(nhanes2013, nhanes2015,
     diet2013, diet2015,
                
    fake_GEO_2010,
    
    adi,
    county_gly,
    cdl_2013, cdl_2015, cultivated_crops, 
    # --> keep??
    cdl_corn_frequency, cdl_cotton_frequency, cdl_soybeans_frequency, cdl_wheat_frequency,
    
    # --> add nlcd dataset
    
    file= file.path("output", "analysis_datasets.rds"))

