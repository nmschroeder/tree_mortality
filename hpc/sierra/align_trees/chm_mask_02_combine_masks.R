#!/usr/bin/env Rscript

## Combine masks for each year into one big data mask

# Nicole Hemming-Schroeder, April 6, 2022

# Read in all the Soaproot saddle masks for 2017-2021
# and multiply them together to create a combined data mask

# Repeat for Lower Teakettle

# Entries with 1 indicate valid data and 0 indicates no data

library(sf)
library(raster)
library(dplyr)

data_dir <- "/path/to/your/data/directory/"
wd <- paste0(data_dir, "landsat_analysis")
setwd(wd)
sites <- c("soap", "teak")

# There are large gaps in 2013 that we do not want to consider
years <- c("2013", "2017", "2018", "2019", "2021")

for (site in sites){
  mlist <- list()
  for (i in 1:length(years)){
    site_fname <- paste0(site,"_mask_30_",years[i],".tif") 
    mlist[[i]] <- raster(site_fname)
  }
  combined_mask <- mlist[[1]]
  for (i in 2:length(years)){
    combined_mask <- combined_mask*mlist[[i]]
  }
  fname <- paste0(site,"_mask.tif")
  check <- list.files(path = ".", fname)
  if (length(check)>0){
    writeRaster(combined_mask, fname, overwrite = TRUE)
  } else{
    writeRaster(combined_mask, fname)
  }
}
