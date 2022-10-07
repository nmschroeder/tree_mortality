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

# Local
stovall_dir <- "~/Documents/R/tree_mortality_stovall/figshare"
data_dir <- "/Volumes/LaCie/sierra/figures/data"

# HPC3
stovall_dir <- "/dfs4/jranders_lab/users/hemmingn/sierra/stovall"
data_dir <- "/dfs5/bio/hemmingn/sierra/align_trees"

setwd(data_dir)

# Stovall trees
stovall_trees <- read.csv(paste0(stovall_dir, "/ALLtrees_v2.csv"))
stovall_trees <- stovall_trees[idx,]
n <- length(idx)
# Tree locations
trees <- read_sf(paste0(data_dir, "/trees_2017_rgreen.shp"))

treeID <- vector(length = n)
rgreen <- vector(length = n)
zmax2013 <- vector(length = n)
d <- vector(length = n)
label <- vector(length = n)
contained <- vector(length = n)

for (i in 1:n){
  x <- stovall_trees$x[i]
  y <- stovall_trees$y[i]
  
  # Compute all the distances between the Stovall tree location and our trees
  distances <- sqrt((x - trees$xlas2017)^2 + (y - trees$ylas2017)^2)
  
  # Find the minimum distance
  idx <- which.min(distances)
  d[i] <- distances[idx]
  
  # Save all the ancillary information to go with it
  treeID[i] <- trees$treeID[idx]
  rgreen[i] <- trees$mean_green[idx]
  label[i] <- trees$live[idx]
  zmax2013[i] <- trees$zmax2013[idx]
  
  # Check to see if this location is contained within our crown polygon
  pt <- st_point(x = c(x, y), dim = "XY")
  tf <- st_contains(trees$geometry[idx], pt, sparse = FALSE)
  contained[i] <- tf*1.0
}

stovall_trees_v2 <- mutate(stovall_trees, d = d, treeID = treeID, rgreen = rgreen, zmax2013 = zmax2013, label = label, contain = contained)
tag <- as.character(start_end) %>% paste(collapse = "_")
fname <- paste0("stovall_matches_", tag, ".csv")
write.csv(stovall_trees_v2, fname)
