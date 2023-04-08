#!/usr/bin/env Rscript
start_end <- commandArgs(trailingOnly = TRUE)

library(dplyr)
library(sf)
library(stringr)
start_end <- str_split(start_end, pattern = ",")
start_end <- do.call(c, start_end)
start_end <- as.numeric(start_end)
print(start_end)

# How many neighbors?
k <- 10

# Search radius for trees within r meters
r <- 20

# HPC
data_dir <- "/path/to/your/data/directory/"
data_dir <- paste0(data_dir, "sierra/")

# Read in 2016 data
shps <- read_sf(paste0(data_dir, "tree_locations_las_intersection.shp"))
st_crs(shps) <- 32611

crds <- st_coordinates(shps)
x <- crds[,1]
y <- crds[,2]
shps <- mutate(shps, x = x, y = y)

clustering_metrics <- list()
ptm <- proc.time()
for (i in start_end[1]:start_end[2]){
  shp <- shps[i,]
  treeID <- shps$treeID[i]    
  # Find the distances between this tree and all the others using the Pythagorean Theorem in 3D
  d <- sqrt((shp$x - shps$x)^2 + (shp$y - shps$y)^2 + (shp$z - shps$z)^2)
  idx <- order(d)
    
  d_ordered <- d[idx]

  # From here, grab the trees within x distance of our tree of interest
  id <- d_ordered < r
  idx_r <- idx[id]
    
  # Store these values
  meanr <- mean(d[idx_r])
  medr <- median(d[idx_r])
  idxr <- str_c(as.character(idx_r), collapse = ",")

  # Pick out the k nearest neighbors
  # (Note the closest one will be the tree itself)  
  idx_k <- idx[2:(k+1)]
    
  # Store these values
  meank <- mean(d[idx_k])
  medk <- median(d[idx_k])
  idxk <- str_c(as.character(idx_k), collapse = ",")
  clustering_metrics[[i]] <- data.frame(treeID = treeID, meank = meank, 
                                          medk = medk,
                                          idxk = idxk,
                                          meanr = meanr, medr = medr, idxr = idxr)
}

proc.time() - ptm
clustering_metrics <- do.call(rbind.data.frame, clustering_metrics)

write.csv(clustering_metrics, paste0(data_dir,"clustering_metrics_", as.character(start_end[1]),"_",as.character(start_end[2]),".csv"))
