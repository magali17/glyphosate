
# about terra library: https://cran.r-project.org/web/packages/terra/terra.pdf 

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

pacman::p_load(#EpiNOAA, #"error epinoa not avaialble for this version of R"
  terra, # for raster files. updated/maintained version of 'raster' library 
  tidyverse,
  lubridate
  )

# where temp files are stored in this project
dt_path <- file.path("data", "raw", "temp", "raster")
dt_path2 <- file.path("data", "raw", "temp", "csv")
if(!dir.exists(dt_path)) {dir.create(dt_path)}
if(!dir.exists(dt_path2)) {dir.create(dt_path2)}


########################################################################################################################
# COMMON VARIABLES
########################################################################################################################
yrs <- c(2013:2018)
months <- c(1:12) %>% str_pad(2, pad="0")

########################################################################################################################
# DOWNLOAD DATA
########################################################################################################################
# go here: https://www.ncei.noaa.gov/products/land-based-station/nclimgrid-daily 
# select FTP grids/area averages
# **year-specific files are here: https://www.ncei.noaa.gov/data/nclimgrid-daily/access/grids/ 
# documentation 1: https://www.ncei.noaa.gov/pub/data/daily-grids/v1-0-0/nclimgrid-daily_v1-0-0_readme-ftp.txt # 
# documentaiton 2: https://www.ncei.noaa.gov/pub/data/daily-grids/v1-0-0/nclimgrid-daily_v1-0-0_user-guide.pdf 

# raster files

# one year at a time
lapply(yrs, function(yr) {
  # create year-month file names (how these are labeled online)
  file_names <- paste0("ncdd-", yr, months, "-grd-scaled.nc")
  
  # download 1 file at a time, if it doesn't already exist
  lapply(file_names, function(f){
    file_path <- file.path(dt_path, f)
    
    if(!file.exists(file_path)) {
      root_dir <- "https://www.ncei.noaa.gov/pub/data/daily-grids/v1-0-0/grids"
      this_file <- paste0(root_dir, "/", yr, "/", f)
      download.file(this_file, file_path)
      }
    })
  })

# # works!
# test0 <- rast(file_path)
# plot(test0)
########################################################################################################################
# census tract CSVs
lapply(yrs, function(yr) {
  # create year-month file names (how these are labeled online)
  file_names <- c(paste0("tavg-", yr, months, "-cen-scaled.csv"),
                  paste0("tmax-", yr, months, "-cen-scaled.csv"))
  
  # download 1 file at a time, if it doesn't already exist
  ## f=file_names[1]
  lapply(file_names, function(f){
    file_path <- file.path(dt_path2, f)
    
    if(!file.exists(file_path)) {
      root_dir <- "https://www.ncei.noaa.gov/pub/data/daily-grids/v1-0-0/averages/"
      this_file <- paste0(root_dir, "/", yr, "/", f)
      download.file(this_file, file_path)
    }
  })
})

########################################################################################################################
# MODIFY CSV FILES
########################################################################################################################
# modify files
temp_files <- list.files(dt_path2)

column_names <- c("Unit_size", "TRACTX2", "Tract", "Year", "Month", "Temp_measure",
                  paste0("day_", 1:31))

# yr=yrs[1]
# one year at a time
modify_temp_file <- function(yr) {
  
  these_files <- str_subset(temp_files, as.character(yr))
    
  # f <- "tavg-201309-cen-scaled.csv"
  result <- lapply(these_files, function(f) {
    
    print(f)

    temp <- read.csv(file.path(dt_path2, f), header = F, col.names = column_names) %>%
      mutate(Month = str_pad(Month, width=2, side="left", pad="0"),
             TRACTX2 = str_pad(TRACTX2, width=11, side="left", pad="0")) %>%
        pivot_longer(cols = contains("day"), names_to = "Day") %>%  
        mutate(Day = gsub("day_", "", Day),
               Day = str_pad(Day, width=2, side="left", pad="0"),
               Date = ymd(paste0(as.character(Year), Month, Day))) %>% #filter((is.na(Date))) %>% distinct(Year, Month)
      # e.g., drop 9/31/13 #day does not exist & has -999 values
      drop_na(Date) %>%
      select(TRACTX2, Date, Temp_measure, value)
    }) %>%
    bind_rows()
  
  return(result)
}

# modify & save annual files
lapply(#yrs, 
  c(2014:2018),
       function(x) {
  message(x)
  modified_files <- modify_temp_file(x) 
  
  message("saving file")
  write.csv(modified_files, file.path(dt_path2, paste0("cen_temp_", x, ".csv")))
})

 






# OLD
########################################################################################################################
# UPLOAD RASTER FILES
########################################################################################################################

# upload raster files to R
# temp1312 <- rast(file.path(dt_path, "ncdd-201312-grd-scaled.nc"))


# # summarize the data
# temp
# plot(temp$tmax_1)

#temp1312


########################################################################################################################
# CALCULATE TEMPERATURE AT ??EXACT LOCATIONS
########################################################################################################################
# SEE 0.2_crop_proximity_variables.R for example spatial code






