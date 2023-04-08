#!/usr/bin/env Rscript
start_end <- commandArgs(trailingOnly = TRUE)

library(dplyr)
library(sf)
library(stringr)
library(raster)
library(exactextractr)
start_end <- str_split(start_end, pattern = ",")[[1]]

tag <- paste(start_end, collapse = "_")
start_end <- as.numeric(start_end)
print(start_end)
print(tag)

data_dir <- "/path/to/your/data/directory/"
data_dir <- paste0(data_dir, "sierra/")
setwd(data_dir)

print("Read in tree locations")
shps <- read_sf("tree_locations_las_intersection.shp")
shps <- shps[start_end[1]:start_end[2],]
shps_buffer <- st_buffer(shps, dist = 10)

print("Read in SRTM tifs")
aspect <- raster("neon_aspect.tif")
elev <- raster("neon_elev.tif")
slope <- raster("neon_slope.tif")

# Extract values from rasters
print("Exact extract aspect")
aspect <- exact_extract(aspect, shps_buffer, 'median')

print("Exact extract slope")
slope <- exact_extract(slope, shps_buffer, 'median')

print("Exact extract elevation")
elev <- exact_extract(elev, shps_buffer, 'median')

# Save the data
env_data <- data.frame(treeID = shps$treeID,
                          aspect = aspect, slope = slope, elev = elev)

fname <- paste0("env_data_part2_", tag, ".csv")
write.csv(env_data, fname)
