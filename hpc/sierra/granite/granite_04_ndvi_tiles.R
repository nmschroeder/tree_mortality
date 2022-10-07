#!/usr/bin/env Rscript
fname <- commandArgs(trailingOnly = TRUE)
library(dplyr)
library(sf)
library(raster)
library(stringr)

## Make a raster file for all the waterbodies in the NEON CHM tif extent for each CHM tile

# Test
#fname <- "/dfs4/jranders_lab/users/hemmingn/NEON/lidar/chm/DP3.30015.001/2017/FullSite/D17/2017_SOAP_2/L3/DiscreteLidar/CanopyHeightModelGtif/NEON_D17_SOAP_DP3_292000_4098000_CHM.tif"
#data_dir <- "/Volumes/LaCie/NEON/spectral_tifs"
#home_dir <- "/Volumes/LaCie/sierra"

# HPC3
home_dir <- "/dfs4/jranders_lab/users/hemmingn/sierra"
data_dir <- "/dfs5/bio/hemmingn/sierra/spectral/merged_rasters"

setwd(home_dir)

# Extract the tag for later
pat <- "[0-9]{6}_[0-9]{7}"

xy_tag <- str_extract(fname, pattern = pat)

print("The xy tag is:")
print(xy_tag)
site_pattern <- "[S,T][O,E][A,E][P,K]"
site <- str_extract(fname, pattern = site_pattern)

check_intersection <- function(x,y){
  test <- tryCatch(intersect(x,y), error = function(e) test <- NA)
  return(test)
}

chm <- raster(fname)
neon_extent <- extent(chm)
# NDVI
ndvi_file <- list.files(data_dir, pattern = glob2rx(paste0("ndvi_",site,"_2017.tif")), recursive = TRUE, full.names = TRUE)
print(ndvi_file)
ndvi <- raster(ndvi_file)
#test <- check_intersection(ndvi, y = chm)
#ndvi <- ndvi[!is.na(test)]
ndvi <- crop(ndvi, chm)

print("First tif filename:")
tif_fname <- paste0("ndvi_", xy_tag, ".tif")
print(tif_fname)

check <- list.files(path = ".", pattern = tif_fname)
if (length(check)>0){
  writeRaster(ndvi, tif_fname, overwrite = TRUE)
} else{
  writeRaster(ndvi, tif_fname)
}
# Relative Green
rgreen_file <- list.files(data_dir, pattern = glob2rx(paste0("rgreen_",site,"_2017.tif")), recursive = TRUE, full.names = TRUE)
rgreen <- raster(rgreen_file)
#test <- check_intersection(rgreen, y = chm)
#rgreen <- rgreen[!is.na(test)]
rgreen <- crop(rgreen, chm)

tif_fname <- paste0("rgreen_", xy_tag, ".tif")

check <- list.files(path = ".", pattern = tif_fname)
if (length(check)>0){
  writeRaster(rgreen, tif_fname, overwrite = TRUE)
} else{
  writeRaster(rgreen, tif_fname)
}

# Relative red
rred_file <- list.files(data_dir, pattern = glob2rx(paste0("rred_",site,"_2017.tif")), recursive = TRUE, full.names = TRUE)
rred <- raster(rred_file)
#test <- check_intersection(rred, y = chm)
#rred <- rred[!is.na(test)]
rred <- crop(rred, chm)

tif_fname <- paste0("rred_", xy_tag, ".tif")

check <- list.files(path = ".", pattern = tif_fname)
if (length(check)>0){
  writeRaster(rred, tif_fname, overwrite = TRUE)
} else{
  writeRaster(rred, tif_fname)
}

# Relative Blue
rblue_file <- list.files(data_dir, pattern = glob2rx(paste0("rblue_",site,"_2017.tif")), recursive = TRUE, full.names = TRUE)
rblue <- raster(rblue_file)
#test <- check_intersection(rblue,  y = chm)
#rblue <- rblue[!is.na(test)]
rblue <- crop(rblue, chm)

tif_fname <- paste0("rblue_", xy_tag, ".tif")

check <- list.files(path = ".", pattern = tif_fname)
if (length(check)>0){
  writeRaster(rblue, tif_fname, overwrite = TRUE)
} else{
  writeRaster(rblue, tif_fname)
}

