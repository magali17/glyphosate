# create directories

directories <- c(file.path("data", "raw", c("adi", "cdl", "county_pesticides", "nhanes", "nlcd")),
                 file.path("data", "modified"),
                 file.path("data", "z. old")
                 )

lapply(directories, function(x) {
  if(!dir.exists(x)) {dir.create(x, recursive = T)}
})
