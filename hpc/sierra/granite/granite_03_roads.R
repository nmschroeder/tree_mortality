#!/usr/bin/env Rscript
fname <- commandArgs(trailingOnly = TRUE)
library(dplyr)
library(sf)
library(raster)
library(stringr)

## Make a raster file for all the waterbodies in the NEON CHM tif extent for each CHM tile

# HPC3
data_dir <- "/path/to/your/data/directory/"
data_dir <- paste0(data_dir, "sierra")
road_dir <- paste0(data_dir, "/landscape/Summary")

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

roads <- list.files(path = road_dir, pattern = glob2rx("Trans_RoadSegment_USFS.shp"), recursive = TRUE, full.names = TRUE)

roads <- lapply(roads, read_sf)
roads <- do.call(rbind.data.frame, roads)
str(roads)

roads <- st_transform(roads, crs = 32611)
roads <- st_buffer(roads, dist = 5)

idx <- st_intersects(neon_polygon, roads, sparse = FALSE)[1,]
if (sum(idx)>0){
  neon_roads <- st_intersection(neon_polygon, roads[idx,])
  neon_roads_tile <- rasterize(as(neon_roads,'Spatial'), chm)
  neon_roads_tile[is.na(neon_roads_tile)] <- 0
}else{
  neon_roads_tile <- chm
  neon_roads_tile[,] <- 0
}

tif_fname <- paste0("roads_", xy_tag, ".tif")

check <- list.files(path = ".", pattern = tif_fname)
if (length(check)>0){
  writeRaster(neon_roads_tile, tif_fname, overwrite = TRUE)
} else{
  writeRaster(neon_roads_tile, tif_fname)
}

