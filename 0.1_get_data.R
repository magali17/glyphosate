

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

pacman::p_load(raster, rgdal,
               tidyverse,
               nhanesA, haven, # retrieve NHANES data
               sf,
               tigris, FedData #NLCD
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

survey_year_codes <- c("H", "I", "J") #2013-2014, 2015-2016, 2017-2018

table_prefixes <- c("SSGLYP", #glyphosate
                    "UPHOPM", # pyrethroids, herbicides & OP's in urine
                    "DEMO", #demographics
                   "BMX", #bmi
                   "ALB_CR", #creatinine 
                   "ALQ", #alcohol
                   "HSQ", #health
                   "OCQ", #occupation
                   "PUQMEC", #pesticides
                   "SMQ",#smoke
                   "FSQ" #food security
                   )

table_names <- lapply(survey_year_codes, function(x) paste(table_prefixes, x, sep = "_"))

nhanes2013 <- lapply(table_names[[1]], function(x) {nhanes(x)})
names(nhanes2013) <- table_names[[1]]

nhanes2015 <- lapply(table_names[[2]], function(x) {nhanes(x)})
names(nhanes2015) <- table_names[[2]]


# TEMP - don't include GLY data, which isn't yet available
table_names_temp <- setdiff(table_names[[3]], c("SSGLYP_J", "UPHOPM_J"))
# --> replace table_names_temp later with table_names[[3]]
nhanes2017 <- lapply(table_names_temp, function(x) {nhanes(x)})
names(nhanes2017) <- table_names_temp

# # --> ERROR - these don't download using nhanes()??
# diet_table_prefixes <- c("DRXFCD",#food codes
#                          "DR1IFF", #foods consumed
#                          "DR1TOT" #total nutrients
#                          )
# diet_table_names <- lapply(survey_year_codes, function(x) paste(diet_table_prefixes, x, sep = "_"))
# 
# diet2013 <- lapply(diet_table_names[[1]], function(x) {nhanes(x)})
# names(diet2013) <- diet_table_names[[1]]
# 
# diet2015 <- lapply(diet_table_names[[2]], function(x) {nhanes(x)})
# names(diet2015) <- diet_table_names[[2]]

# upload diet data from XPT files
diet_files_names <- list.files(file.path("data", "raw", "nhanes"))
diet <- lapply(diet_files_names, function(x) read_xpt(file.path("data", "raw", "nhanes", x)))
# remove file extensions
names(diet) <- tools::file_path_sans_ext(diet_files_names)   

########################################################################################################################
# ADI
########################################################################################################################
# ADI is for 2015
adi0 <- read.csv(file.path("data", "raw", "adi", "US_2015_ADI_Census Block Group_v3.1.txt"), 
                 #don't drop leading 0s
                 colClasses = c(rep("character", 5))) 

adi <- adi0 %>%
  #rename to match NHANES
  select(BG2KX = FIPS, adi_natrank=ADI_NATRANK#, adi_staternk = ADI_STATERNK
         )
 
rm(adi0) 
write.csv(adi, file.path("data", "modified", "adi.csv"), row.names = F)

# adi <- read.csv(file.path("data", "modified", "adi.csv"))
########################################################################################################################
# COUNTY GLYPHOSATE
########################################################################################################################
# Using the overall average for each survey year duo (e.g., 2013-2014) and min vs max gly application rates (these are typically very similar anyways)

keep_county_yrs <- c(2013:2018)

county_gly0 <- read.delim(file.path("data", "raw", "county_pesticides", "EPest_county_estimates_2013_2017_v2.txt")) %>%
  # include preliminary 2018 values
  bind_rows(read.delim(file.path("data", "raw", "county_pesticides", "EPest_county_estimates_2018.txt")))

county_gly <- county_gly0 %>%
  filter(COMPOUND == "GLYPHOSATE",
         YEAR %in% keep_county_yrs) %>% 
  #take avg of year pairs
  mutate(
    YEAR = ifelse(YEAR %in% 2013:2014, 2013, 
                  ifelse(YEAR %in% 2015:2016, 2015, 
                         ifelse(YEAR %in% 2017:2018, 2017, NA)))) %>%
  group_by(YEAR, STATE_FIPS_CODE, COUNTY_FIPS_CODE) %>%
  summarize(EPEST_LOW_KG = mean(EPEST_LOW_KG),
            EPEST_HIGH_KG = mean(EPEST_HIGH_KG)) %>%
  rowwise() %>%
  mutate(
    # avg pesticide estimate - low/high estimates are typically very similar anyways
    EPEST_MEAN_KG = mean(c(EPEST_LOW_KG, EPEST_HIGH_KG), na.rm=T)) %>%
  ungroup() %>% #View()
  # select(-c(EPEST_LOW_KG, EPEST_HIGH_KG)) %>%  
  # pivot_wider(names_from = YEAR, values_from = county_gly_mean_kg, names_prefix = "county_gly_kg_") %>%
  
  # rename to match GEO_2010 merging variables
  rename(
    STATE2KX = STATE_FIPS_CODE,
    CNTY2KX = COUNTY_FIPS_CODE)
  
rm(county_gly0)

# --> save in wide format??
write.csv(county_gly, file.path("data", "modified", "county_gly.csv"), row.names = F)

# county_gly <- read.csv(file.path("data", "modified", "county_gly.csv"))
########################################################################################################################
# CDL
########################################################################################################################

cultivated_crops <- read.delim(file.path(cdl_path, "cultivated_crops.txt"), skip = 1, col.names = c("class")) %>%
  rownames_to_column(var="id") %>%
  mutate(id = as.numeric(id))

write.csv(cultivated_crops, file.path("data", "modified", "cultivated_crops.csv"), row.names = F)

# cultivated_crops <- read.csv(file.path("data", "modified", "cultivated_crops.csv"))
########################################################################################################################
# NLCD
########################################################################################################################
# not using for now; having issues w/ the downloaded file still - there are no values in the raster

# us_template <- tigris::nation(year = 2021) 
# # us_template <- tigris::counties('WA') #%>% filter(NAME == 'King')
# 
# yr <- c(2016)
# 
# nlcd_path <- file.path("data", "raw", "nlcd", paste0("nlcd_us_", yr))
# 
# !if(file.exists(nlcd_path)) { 
#   nlcd <- get_nlcd(
#     template = us_template,
#     label = paste(yr, ' USA'),
#     year = yr,
#     dataset = 'landcover', 
#     extraction.dir = nlcd_path)
#   } else {
#   # --> add yr to nlcd object name?
#   nlcd <- raster(file.path(nlcd_path, paste0(yr, "  USA_NLCD_Land_Cover_", yr, ".tif")))
#     }
# 
# # raster::plot(nlcd)


########################################################################################################################
# CENSUS STATE REIGIONS AND DIVISIONS
########################################################################################################################
# Remove first few lines for easier merging
states <- readxl::read_xlsx(file.path("data", "raw", "census", "state-geocodes-v2021.xlsx"), skip = 5, 
                            col_types = c("numeric", "numeric", "numeric", "text")) %>%
  rename("STATE2KX" = "State (FIPS)")
# states

write.csv(states, file.path("data", "modified", "state_divisions.csv"), row.names = F)


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

#gly_ids <- c(nhanes2013$DEMO_H$SEQN, nhanes2015$DEMO_I$SEQN)
gly_ids <- c(nhanes2013$SSGLYP_H$SEQN, nhanes2015$SSGLYP_I$SEQN)

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
                            ) 

########################################################################################################################
# SAVE DATA FILES
########################################################################################################################
save(nhanes2013, nhanes2015, nhanes2017,
     diet,
     fake_GEO_2010,
     file= file.path("output", "analysis_datasets.rds"))
