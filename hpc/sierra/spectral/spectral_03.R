#!/usr/bin/env Rscript
siteyear <- commandArgs(trailingOnly = TRUE)
siteyear

library(dplyr)
library(raster)
library(stringr)

data_dir <- "/path/to/your/data/directory/"
data_dir <- paste0(data_dir, "sierra/spectral")
wd <- paste0(data_dir, "/merged_rasters")
setwd(wd)

siteyear <- str_split(siteyear, ",")[[1]]
site <- siteyear[1]
year <- siteyear[2]

print("Site and year check:")
print("Site:")
print(site)
print("Year:")
print(year)
fnames <- list.files(data_dir, pattern = glob2rx(paste0("spectral_",site,"_",year,"_*.tif")), full.names = TRUE)
fnames

print("Applying raster to each filename")
blue <- lapply(fnames, raster, band = 1)
blue <- do.call(merge, blue)

print("Applying raster to each filename")
green <- lapply(fnames, raster, band = 2)
green <- do.call(merge, green)

print("Applying raster to each filename")
red <- lapply(fnames, raster, band = 3)
red <- do.call(merge, red)

luminosity <- (blue + red + green)/3
fname <- paste0("luminosity_",site,"_",year,".tif")
check <- list.files(wd, pattern=fname)
if (length(check)>0){
  writeRaster(luminosity, fname, overwrite=TRUE)
}else{
  writeRaster(luminosity,fname)
}
rm(luminosity)

rgreen <- green/(blue+red+green)
fname <- paste0("rgreen_",site,"_",year,".tif")
check <- list.files(wd, pattern=fname)
if (length(check)>0){
  writeRaster(rgreen, fname, overwrite=TRUE)
}else{
  writeRaster(rgreen,fname)
}
rm(rgreen)

rred <- red/(blue+red+green)
fname <- paste0("rred_",site,"_",year,".tif")
check <- list.files(wd, pattern=fname)
if (length(check)>0){
  writeRaster(rred, fname, overwrite=TRUE)
}else{
  writeRaster(rred,fname)
}
rm(rred)

rblue <- blue/(blue+red+green)
fname <- paste0("rblue_",site,"_",year,".tif")
check <- list.files(wd, pattern=fname)
if (length(check)>0){
  writeRaster(rblue, fname, overwrite=TRUE)
}else{
  writeRaster(rblue,fname)
}
rm(rblue)

nir <- lapply(fnames, raster, band = 4)
nir <- do.call(merge, nir)

ndvi <- (nir - red)/(nir + red)
rm(red)
fname <- paste0("ndvi_",site,"_",year,".tif")
check <- list.files(wd, pattern=fname)
if (length(check)>0){
  writeRaster(ndvi, fname, overwrite=TRUE)
}else{
  writeRaster(ndvi,fname)
}
rm(ndvi)

w1 <- lapply(fnames, raster, band = 5)
w1 <- do.call(merge, w1)
fname <- paste0("w1_", site, "_", year, ".tif")
check <- list.files(wd, pattern=fname)
if (length(check)>0){
  writeRaster(w1, fname, overwrite=TRUE)
}else{
  writeRaster(w1, fname)
}

ndmi1 <- (nir - w1)/(nir + w1)
rm(w1)
fname <- paste0("ndmi1_",site,"_",year,".tif")
check <- list.files(wd, pattern=fname)
if (length(check)>0){
  writeRaster(ndmi1, fname, overwrite=TRUE)
}else{
  writeRaster(ndmi1,fname)
}

rm(ndmi1)

swir1 <- lapply(fnames, raster, band = 6)
swir1 <- do.call(merge, swir1)
ndmi <- (nir - swir1)/(nir + swir1)
fname <- paste0("ndmi_",site,"_",year,".tif")
check <- list.files(wd, pattern=fname)
if (length(check)>0){
  writeRaster(ndmi, fname, overwrite=TRUE)
}else{
  writeRaster(ndmi,fname)
}

rm(swir1, ndmi)

w2 <- lapply(fnames, raster, band = 7)
w2 <- do.call(merge, w2)
fname <- paste0("w2_", site, "_", year,	".tif")
check <- list.files(wd,	pattern=fname)
if (length(check)>0){
  writeRaster(w2, fname, overwrite=TRUE)
}else{
  writeRaster(w2, fname)
}

ndmi2 <- (nir - w2)/(nir + w2)
rm(w2)
fname <- paste0("ndmi2_",site,"_",year,".tif")
check <- list.files(wd, pattern=fname)
if (length(check)>0){
  writeRaster(ndmi2, fname, overwrite=TRUE)
}else{
  writeRaster(ndmi2,fname)
}

rm(ndmi2)
 
print("Completed")
