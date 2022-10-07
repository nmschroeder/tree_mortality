#!/usr/bin/env Rscript
fname <- commandArgs(trailingOnly = TRUE)
library(dplyr)
library(sf)
library(raster)
library(stringr)

## Make a raster file for all the waterbodies in the NEON CHM tif extent for each CHM tile

# HPC3
home_dir <- "/dfs4/jranders_lab/users/hemmingn/sierra"

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

print("Finding the ndvi file")
ndvi <- list.files(home_dir, pattern = glob2rx(paste0("ndvi_",xy_tag,".tif")), full.names = TRUE)
ndvi
if (length(ndvi)>0){
  ndvi <- raster(ndvi)
  ndvi_lb <- ndvi>=-0.20
  ndvi_ub <- ndvi<=0.50
} else{
  skip <- TRUE
}

rgreen <- list.files(home_dir, pattern = glob2rx(paste0("rgreen_", xy_tag, ".tif")), full.names = TRUE)

if (length(rgreen)>0 & !skip){
  rgreen <- raster(rgreen)
  rgreen_lb <- rgreen>=0.25
  rgreen_ub <- rgreen<=0.35
} else{
  skip <- TRUE
}

rred <- list.files(home_dir, pattern = glob2rx(paste0("rred_", xy_tag, ".tif")), full.names = TRUE)

if (length(rred)>0 & !skip){
  rred <- raster(rred)
  rred_lb <- rred>=0.25
  rred_ub <- rred<=0.35
} else{
  skip <- TRUE
}

rblue <- list.files(home_dir, pattern = glob2rx(paste0("rblue_", xy_tag, ".tif")), full.names = TRUE)

if (length(rblue)>0 & !skip){
  rblue <- raster(rblue)
  rblue_lb <- rblue>=0.18
  rblue_ub <- rblue<=0.35
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
  tif_obj <- stack(chm, ndvi_ub, ndvi_lb, rgreen_ub, rgreen_lb, rred_ub, rred_lb, rblue_lb, rblue_ub, water, roads)
  tif_obj <- calc(tif_obj, fun = prod)
  
  tif_fname <- paste0("granite_", xy_tag, ".tif")
  
  check <- list.files(path = ".", pattern = tif_fname)
  if (length(check)>0){
    writeRaster(tif_obj, tif_fname, overwrite = TRUE)
  } else{
    writeRaster(tif_obj, tif_fname)
  }

}
