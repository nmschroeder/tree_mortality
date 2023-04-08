#!/usr/bin/env Rscript
siteyear <- commandArgs(trailingOnly = TRUE)
siteyear

library(dplyr)
library(raster)
library(stringr)

data_dir <- "/path/to/your/data/directory/"
wd <- paste0(data_dir, "sierra/spectral/merged_rasters")
setwd(wd)

siteyear <- str_split(siteyear, ",")[[1]]
site <- siteyear[1]
year <- siteyear[2]

print("Site and year check:")
print("Site:")
print(site)
print("Year:")
print(year)

chm_dir <- paste0(data_dir, "NEON/chm/", year, "/", site)

fnames <- list.files(chm_dir, pattern = glob2rx(paste0("NEON*",site,"*_CHM.tif")), full.names = TRUE, recursive = TRUE)
fnames

print("Applying raster to each filename")
chm <- lapply(fnames, raster)
chm <- do.call(merge, chm)

fname <- paste0("chm_",site,"_",year,".tif")
check <- list.files(wd, pattern=fname)
if (length(check)>0){
  writeRaster(chm, fname, overwrite=TRUE)
}else{
  writeRaster(chm,fname)
}


print("Completed")
