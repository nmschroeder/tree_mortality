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

# Luminosity
lum_files <- list.files(data_dir, pattern = glob2rx(paste0("luminosity_",site,"_*.tif")), recursive = TRUE, full.names = TRUE)
lum_files <- lum_files[2:length(lum_files)] # cut 2013 due to missing tiles
lum <- lapply(lum_files, raster)
lum <- lapply(lum, crop, y = chm)
lum <- do.call(stack, lum)
lum <- calc(lum, fun = median, na.rm = TRUE)

print("First tif filename:")
tif_fname <- paste0("lum_", xy_tag, ".tif")
print(tif_fname)

check <- list.files(path = ".", pattern = tif_fname)
if (length(check)>0){
  writeRaster(lum, tif_fname, overwrite = TRUE)
} else{
  writeRaster(lum, tif_fname)
}

