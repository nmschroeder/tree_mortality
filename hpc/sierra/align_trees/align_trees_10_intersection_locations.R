#!/usr/bin/env Rscript
.libPaths("/data/homezvol0/hemmingn/R/x86_64-pc-linux-gnu-library/3.6")
library(sf)
library(dplyr)

# Local
data_dir <- "/Volumes/LaCie/sierra"

# HPC3
data_dir <- "/dfs5/bio/hemmingn/sierra/align_trees"

setwd(data_dir)

xy <- read_sf("tree_locations.shp")
shps <- read_sf("tree_objects_2017_las_intersection.shp")


xy_data <- dplyr::filter(xy, treeID %in% shps$treeID)


ca2013 <- st_area(shps$geometry) %>% as.numeric()
shps <- mutate(shps, ca2013 = ca2013) %>% st_drop_geometry()
shp_chars <- dplyr::select(shps, treeID, ca2013, zmax2013)
xy_data_match <- right_join(xy_data, shp_chars, by = "treeID")
xy_data_match <- dplyr::filter(xy_data_match, zmax2013 >= 5, ca2013>= 1)
st_write(xy_data_match, "tree_locations_las_intersection.shp", append = FALSE, delete_dsn = TRUE)
