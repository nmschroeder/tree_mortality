#!/usr/bin/env Rscript
start_end <- commandArgs(trailingOnly = TRUE)

library(dplyr)
library(sf)
library(stringr)
start_end <- str_split(start_end, pattern = ",")
start_end <- do.call(c, start_end)
start_end <- as.numeric(start_end)
print(start_end)
idx <- start_end[1]:start_end[2]

# HPC3
data_dir <- "/path/to/your/data/directory/"
stovall_dir <- paste0(data_dir, "Stovall")
data_dir <- paste0(data_dir, "sierra")

setwd(data_dir)

# Stovall trees
stovall_trees <- read.csv(paste0(stovall_dir, "/ALLtrees_v2.csv"))
stovall_trees <- stovall_trees[idx,]
n <- length(idx)
# Tree locations
trees <- read_sf(paste0(data_dir, "/trees_2017_rgreen.shp")) %>% st_drop_geometry()
trees2013 <- read_sf(paste0(data_dir, "/trees_2013_rgreen.shp")) %>% dplyr::select(treeID)

trees <- right_join(trees, trees2013, by = "treeID")

treeID <- vector(length = n)
mean_green <- vector(length = n)
zmax2013 <- vector(length = n)
d <- vector(length = n)
label <- vector(length = n)
contained <- vector(length = n)

for (i in 1:n){
  # Assign the coordinates of tree i to x and y
  x <- stovall_trees$x[i]
  y <- stovall_trees$y[i]
  
  # Compute all the distances between the Stovall tree location and our trees
  distances <- sqrt((x - trees$xlas2017)^2 + (y - trees$ylas2017)^2)
  
  # Find the minimum distance
  idx_min <- which.min(distances)
  d[i] <- distances[idx_min]
  
  # Save all the ancillary information to go with it
  treeID[i] <- trees$treeID[idx_min]
  mean_green[i] <- trees$mean_green[idx_min]
  label[i] <- trees$live[idx_min]
  zmax2013[i] <- trees$zmax2013[idx_min]
  
  # Check to see if this location is contained within our crown polygon
  pt <- st_point(x = c(x, y), dim = "XY")
  tf <- st_contains(trees$geometry[idx_min], pt, sparse = FALSE)
  contained[i] <- tf*1.0
}

stovall_trees_v2 <- mutate(stovall_trees, d = d, treeID = treeID, mean_green = mean_green, zmax2013 = zmax2013, label = label, contain = contained)
tag <- as.character(start_end) %>% paste(collapse = "_")
fname <- paste0("stovall_matches_", tag, ".csv")
write.csv(stovall_trees_v2, fname)
