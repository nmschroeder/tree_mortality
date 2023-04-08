#!/usr/bin/env Rscript

library(sf)
library(dplyr)

# HPC
data_dir <- "/path/to/your/data/directory/"
data_dir <- paste0(data_dir, "sierra")

setwd(data_dir)

xy <- read_sf("tree_locations.shp")
shps <- read_sf("tree_objects_2013_las_intersection.shp")


xy_data <- dplyr::filter(xy, treeID %in% shps$treeID)


ca2013 <- st_area(shps$geometry) %>% as.numeric()
shps <- mutate(shps, ca2013 = ca2013) %>% st_drop_geometry()
shp_chars <- dplyr::select(shps, treeID, ca2013, zmax2013)
xy_data_match <- right_join(xy_data, shp_chars, by = "treeID")
xy_data_match <- dplyr::filter(xy_data_match, zmax2013 >= 5, ca2013>= 1)
st_write(xy_data_match, "tree_locations_las_intersection.shp", append = FALSE, delete_dsn = TRUE)
