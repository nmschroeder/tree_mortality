#!/usr/bin/env Rscript
fname <- commandArgs(trailingOnly = TRUE)
library(dplyr)
library(sf)
library(raster)
library(stringr)

## Make a raster file for all the waterbodies in the NEON CHM tif extent for each CHM tile

# Test
#fname <- "/dfs4/jranders_lab/users/hemmingn/NEON/lidar/chm/DP3.30015.001/2017/FullSite/D17/2017_SOAP_2/L3/DiscreteLidar/CanopyHeightModelGtif/NEON_D17_SOAP_DP3_292000_4098000_CHM.tif"

# Local
data_dir <- "/Volumes/LaCie/sierra"
nhd_dir <- "/Volumes/LaCie/NHD_HR"

# HPC3
data_dir <- "/dfs4/jranders_lab/users/hemmingn/sierra"
nhd_dir <- "/dfs4/jranders_lab/users/hemmingn/sierra/landscape/NHD_HR"

setwd(data_dir)

# Extract the tag for later
pat <- "[0-9]{6}_[0-9]{7}"
xy_tag <- str_extract(fname, pattern = pat)

chm <- raster(fname)
neon_extent <- extent(chm)

p1 <- st_point(x = c(neon_extent@xmin, neon_extent@ymin), dim = "XY")
p2 <- st_point(x = c(neon_extent@xmin, neon_extent@ymax), dim = "XY")
p3 <- st_point(x = c(neon_extent@xmax, neon_extent@ymax), dim = "XY")
p4 <- st_point(x = c(neon_extent@xmax, neon_extent@ymin), dim = "XY")
neon_polygon <- st_polygon(x = list(rbind(p1, p2, p3, p4, p1)), dim = "XY")
neon_polygon <- st_sfc(neon_polygon, crs = 32611)

waterbodies <- list.files(nhd_dir, pattern = glob2rx("NHD*Water*.shp"), recursive = TRUE, full.names = TRUE)
waterbodies <- lapply(waterbodies, read_sf)
waterbodies <- do.call(rbind.data.frame, waterbodies)
str(waterbodies)

waterbodies <- st_transform(waterbodies, crs = 32611)

idx <- st_intersects(neon_polygon, waterbodies, sparse = FALSE)[1,]
if (sum(idx)>0){
  neon_water <- st_intersection(neon_polygon, waterbodies[idx,])
  neon_water_tile <- rasterize(as(neon_water,'Spatial'), chm)
  neon_water_tile[is.na(neon_water_tile)] <- 0
}else{
  neon_water_tile <- chm
  neon_water_tile[,] <- 0
}

tif_fname <- paste0("waterbody_", xy_tag, ".tif")

check <- list.files(path = ".", pattern = tif_fname)
if (length(check)>0){
  writeRaster(neon_water_tile, tif_fname, overwrite = TRUE)
} else{
  writeRaster(neon_water_tile, tif_fname)
}

