#!/usr/bin/env Rscript
start_end <- commandArgs(trailingOnly = TRUE)

library(dplyr)
library(sf)
library(stringr)
start_end <- str_split(start_end, pattern = ",")
start_end <- do.call(c, start_end)
start_end <- as.numeric(start_end)
print(start_end)

# HPC
data_dir <- "/path/to/your/data/directory/"
data_dir <- paste0(data_dir, "sierra")
out_dir <- data_dir
data_dir <- paste0(data_dir, "/granite")

# Read in the tree polygons
shps <- read_sf(paste0(out_dir, "/tree_locations_las_intersection.shp"))
st_crs(shps) <- 32611

# Read in the granite polygons
granite_polys <- read_sf(paste0(data_dir, "/granite_polygons_complete.shp"))
granite_polys <- dplyr::filter(granite_polys, poly_area>10000)
# Initialize a list for the dataframes for each tree
d <- list()

ptm <- proc.time()

# For each tree top, find the distance between the tree and the granite
for (i in start_end[1]:start_end[2]){
  tree <- shps[i,]
  idx <- which.min(sf::st_distance(tree, granite_polys))
  d_min <- sf::st_distance(tree, granite_polys[idx,])
  d[[i]] <- data.frame(treeID = tree$treeID, d_granite = d_min, area_granite = as.numeric(st_area(granite_polys[idx,])))
}

# Collect process time
proc.time() - ptm

# Bind all the dataframes in the list together
min_dist <- do.call(rbind.data.frame, d)

# Save output
write.csv(min_dist, paste0(out_dir,"/granite_distance_", as.character(start_end[1]),"_",as.character(start_end[2]),".csv"))
