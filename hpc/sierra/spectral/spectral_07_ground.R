#!/usr/bin/env Rscript
library(dplyr)
library(raster)
library(stringr)

data_dir <- "/path/to/your/data/directory/"
wd <- paste0(data_dir, "sierra/spectral/merged_rasters")
setwd(wd)

sites <- c("SOAP", "TEAK")
year <- "2017"

for (site in sites){
  chm <- raster(paste0("chm_", site, "_", year, ".tif"))
  ground <- chm<0.5
  fname <- paste0("ground_",site,".tif")
  check <- list.files(wd, pattern=fname)
  if (length(check)>0){
    writeRaster(ground, fname, overwrite=TRUE)
  }else{
    writeRaster(ground, fname)
  }
}

print("Completed")
