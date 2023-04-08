#!/usr/bin/env Rscript

library(dplyr)
library(sf)

data_dir <- "/path/to/your/data/directory/"
data_dir <- paste0(data_dir, "sierra/")

setwd(data_dir)

granite <- read.table("granite_distances.csv") %>% dplyr::select(-X) %>%
    rename(dgran = d_granite, agran = area_granite)
print("Read in granite distances")
str(granite)

rgranite <- read.csv("rgranite.csv")
str(rgranite)

landscape <- read.table("landscape/landscape_distances.csv")
print("Read in landscape_distances.csv")
str(landscape)

# Has treeID
clustering_metric <- read.table("clustering_metrics.csv") %>% dplyr::select(treeID, meank, medk)
print("Read in clustering metrics")
str(clustering_metric)

# Has treeID but the format of the data frame looks incorrect with read table (read.csv looks good, though)
env_vars <- read.csv("env_data_las_intersection.csv") %>% dplyr::select(-X)
print("Read in environmental variables")
str(env_vars)

tree_data <- read.csv("tree_objects.csv") %>% dplyr::select(treeID, starts_with("z"))
print("Read in tree data")
str(tree_data)

print("Join tables")
tree_data <- dplyr::filter(tree_data, treeID %in% granite$treeID) %>% 
  right_join(granite, by = "treeID") %>% 
  right_join(rgranite, by = "treeID") %>%
  right_join(landscape, by = "treeID") %>%
  right_join(clustering_metric, by = "treeID") %>%
  right_join(env_vars, by = "treeID") %>% 
  dplyr::select(-starts_with("X"))

rm(granite, landscape, env_vars)
print("Write csv")
write.csv(tree_data, "feature_vars.csv")

print("Next: reading in tree shapefile")
tree_shps <- read_sf("tree_objects_2017_las_intersection.shp") %>% 
  dplyr::select(-starts_with("X|z"))

tree_shps_data <- right_join(tree_shps, tree_data, by = "treeID")

check <- list.files(".", pattern = "feature_vars.shp")
st_write(tree_shps_data, "feature_vars.shp", append = FALSE)


