#!/usr/bin/env Rscript

library(tidyr)
library(dplyr)
library(sf)

data_dir <- "/path/to/your/data/directory/"
home_dir <- paste0(data_dir, "sierra")
setwd(home_dir)

print("Reading in the table")
# Read in tree table
trees_csv <- read.csv(paste0(home_dir, "tree_objects.csv"))

print("Finding indices for overlap")
# Find indices which are not NA
idx <- which(!is.na(trees_csv$zmax2013) & !is.na(trees_csv$zmax2017) & !is.na(trees_csv$zmax2018) & !is.na(trees_csv$zmax2019) & !is.na(trees_csv$zmax2021))
treeIDs <- trees_csv$treeID[idx]

print("The number of overlapping trees")
length(idx)

print("The first 10 tree IDs")
treeIDs[1:10]


trees_2013 <- read_sf(paste0(home_dir, "tree_objects_2013.shp"))
trees_2017 <- read_sf(paste0(home_dir, "tree_objects_2017.shp"))
trees_2018 <- read_sf(paste0(home_dir, "tree_objects_2018.shp"))
trees_2019 <- read_sf(paste0(home_dir, "tree_objects_2019.shp"))
trees_2021 <- read_sf(paste0(home_dir, "tree_objects_2021.shp"))

trees_2013 <- dplyr::filter(trees_2013, treeID %in% treeIDs)
trees_2017 <- dplyr::filter(trees_2017, treeID %in% treeIDs)
trees_2018 <- dplyr::filter(trees_2018, treeID %in% treeIDs)
trees_2019 <- dplyr::filter(trees_2019, treeID %in% treeIDs)
trees_2021 <- dplyr::filter(trees_2021, treeID %in% treeIDs)
trees_csv <- dplyr::filter(trees_csv, treeID %in% treeIDs)

trees_2013 <- left_join(trees_2013, trees_csv, by = "treeID")
trees_2017 <- left_join(trees_2017, trees_csv, by = "treeID")
#trees_2018 <- left_join(trees_2018, trees_csv, by = "treeID")
#trees_2019 <- left_join(trees_2019, trees_csv, by = "treeID")
#trees_2021 <- left_join(trees_2021, trees_csv, by = "treeID")

st_write(trees_2013, "tree_objects_2013_las_intersection.shp", update = FALSE, delete_dsn = TRUE)
st_write(trees_2017, "tree_objects_2017_las_intersection.shp", update = FALSE, delete_dsn = TRUE)
st_write(trees_2018, "tree_objects_2018_las_intersection.shp", update = FALSE, delete_dsn = TRUE)
st_write(trees_2019, "tree_objects_2019_las_intersection.shp", update = FALSE, delete_dsn = TRUE)
st_write(trees_2021, "tree_objects_2021_las_intersection.shp", update = FALSE, delete_dsn = TRUE)


