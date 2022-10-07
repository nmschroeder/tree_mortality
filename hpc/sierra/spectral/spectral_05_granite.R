#!/usr/bin/env Rscript

library(dplyr)
library(raster)
library(stringr)

wd <- "/dfs5/bio/hemmingn/sierra/spectral/merged_rasters"
setwd(wd)

granite_dir <- "/dfs4/jranders_lab/users/hemmingn/sierra"

fnames <- list.files(granite_dir, pattern = glob2rx("granite*.tif"), full.names = TRUE)
xcoord <- str_extract(fnames, pattern =	"_[0-9]{6}_")

for (i in 1:length(xcoord)){
 xcoord[i] <- gsub("_", "", xcoord[i])
}
xcoord <- as.numeric(xcoord)
print(xcoord)

sites <- c("SOAP", "TEAK")

for (site in sites){
  if (site == "SOAP"){
    idx <- xcoord < 305000
  } else{
    idx <- xcoord > 305000
  }

  fs <- fnames[idx]
  print(fs)
  granite <- lapply(fs, raster)
  granite <- do.call(merge, granite)

  fname <- paste0("granite_",site,".tif")
  check <- list.files(wd, pattern=fname)
  if (length(check)>0){
    writeRaster(granite, fname, overwrite=TRUE)
  }else{
    writeRaster(granite,fname)
  }
  rm(granite)
}


print("Completed")
