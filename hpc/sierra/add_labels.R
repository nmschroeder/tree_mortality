#!/usr/bin/env Rscript

library(dplyr)
library(sf)

data_dir <- "/path/to/your/data/directory/"

data_dir <- paste0(data_dir, "sierra")
setwd(data_dir)

feature_vars <- read.csv("feature_vars.csv")
str(feature_vars)
feature_shps <- read_sf("feature_vars.shp")

labels <- read_sf(paste0(data_dir, "/trees_2017_rgreen.shp")) 
labels <- dplyr::select(labels, geometry, treeID, mean_green, live) %>% dplyr::filter(treeID %in% feature_vars$treeID)
labels <- arrange(labels, treeID)
labels <- st_drop_geometry(labels)

feature_shps <- dplyr::filter(feature_shps, treeID %in% labels$treeID)
feature_shps <- arrange(feature_shps, treeID)

feature_vars <- dplyr::filter(feature_vars, treeID %in% labels$treeID)
feature_vars <- arrange(feature_vars, treeID)

if (identical(labels$treeID, feature_shps$treeID)){
  feature_shps_labels <- cbind.data.frame(labels, dplyr::select(feature_shps, -treeID))
  st_write(feature_shps_labels, "feature_vars_labels.shp", append = FALSE)
} else{
  print("treeID arrays were not identical")
}

if (identical(labels$treeID, feature_vars$treeID)){
  feature_vars <- cbind.data.frame(labels, dplyr::select(feature_vars, -treeID))
  write.csv(feature_vars, "feature_vars_labels.csv")
} else{
  print("treeID arrays were not identical")
}
