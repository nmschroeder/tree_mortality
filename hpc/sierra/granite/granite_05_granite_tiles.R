#!/usr/bin/env Rscript
fname <- commandArgs(trailingOnly = TRUE)
library(dplyr)
library(sf)
library(raster)
library(stringr)

## Make a raster file for all the waterbodies in the NEON CHM tif extent for each CHM tile

# HPC
data_dir <- "/path/to/your/data/directory/"
home_dir <- paste0(data_dir, "sierra")

setwd(home_dir)

# Extract the tag for later
pat <- "[0-9]{6}_[0-9]{7}"

xy_tag <- str_extract(fname, pattern = pat)
xy_tag

site_pattern <- "[S,T][O,E][A,E][P,K]"
site <- str_extract(fname, pattern = site_pattern)
site

chm <- raster(fname)
chm <- chm<0.5

# Check if each file we need is available
skip <- FALSE

print("Finding the luminosity file")
lum <- list.files(home_dir, pattern = glob2rx(paste0("lum_",xy_tag,".tif")), full.names = TRUE)
lum
if (length(lum)>0){
  lum <- raster(lum)
  lum <- lum>0.11
} else{
  skip <- TRUE
}


print("Waterbody file")
water <- list.files(home_dir, pattern = glob2rx(paste0("waterbody_",xy_tag,".tif")), recursive = TRUE, full.names = TRUE) 
water
if (length(water)>0 & !skip){
  water <- raster(water)
  water <- water == 0
} else{
  skip <- TRUE
}
print("Roads file")
roads <- list.files(home_dir, pattern = glob2rx(paste0("roads_",xy_tag,".tif")), recursive = TRUE, full.names = TRUE)
roads
if (length(roads)>0 & !skip){
  roads <- raster(roads)
  roads <- roads == 0
} else{
  skip <- TRUE
}

if (!skip){
  print("Stacking the files")
  tif_obj <- stack(chm, lum, water, roads)
  tif_obj <- calc(tif_obj, fun = prod)
  
  tif_fname <- paste0("granite_", xy_tag, ".tif")
  
  check <- list.files(path = ".", pattern = tif_fname)
  if (length(check)>0){
    writeRaster(tif_obj, tif_fname, overwrite = TRUE)
  } else{
    writeRaster(tif_obj, tif_fname)
  }

}
