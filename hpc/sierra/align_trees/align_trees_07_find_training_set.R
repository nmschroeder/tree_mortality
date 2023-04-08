#!/usr/bin/env Rscript
task_idx <- commandArgs(trailingOnly = TRUE)
library(dplyr)
library(sf)
library(stringr)

data_dir <- "/path/to/your/data/directory/"
home_dir <- paste0(data_dir, "sierra")
setwd(home_dir)

task_idx <- str_split(task_idx,	pattern	= ",")
task_idx <- do.call(c, task_idx)
tag <- paste(task_idx, collapse	= "_")
print(tag)
task_idx <- as.numeric(task_idx)
task_seq <- task_idx[1]:task_idx[2]

# Read in trees
print("reading in the trees")
trees_2013 <- read_sf(paste0(home_dir, "tree_objects_2013_las_intersection.shp")) 
trees_2013 <- trees_2013[task_seq,]

tree_locs <- read_sf(paste0(home_dir, "tree_locations.shp"))
str(tree_locs)
sample_shps <- read_sf(paste0(home_dir, "training_sample_sites.shp"))
str(sample_shps)
print("pull the treeIDs for trees in the lidar intersection")
# Pull the treeIDs for the trees in the lidar file intersection 
las_treeIDs <- trees_2013$treeID

# Filter the tree locations
print("filter for the tree locations in the intersection")

tree_locs_sample <- dplyr::filter(tree_locs, treeID %in% las_treeIDs)
print(str(tree_locs_sample))

print("read in custom intersection function")
# Need to check if it intersects any of the sample shapes
st_intersects_sample <- function(x, y){
  test <- st_intersects(x, y, sparse = FALSE)
  if (any(test)){
    sample_id <- which(test)
  } else{
    sample_id <- NA
  }
  return(sample_id)
}

print("apply the function to the trees in the intersection")
sample_id <- sapply(tree_locs_sample$geometry, st_intersects_sample, y = sample_shps$geometry)
str(sample_id)
print("find out which of the trees are in the sample squares")
idx <- !is.na(sample_id)

print("reduce the sample ids down to only the ones in the sample squares")
sampleid <- sample_id[idx]

# These should be much smaller and easier to load into QGIS
if (length(sample_id)>0){
  print("select only the trees that are in the sample squares")
  tree_locs_sample <- tree_locs_sample[idx,]
  
  print("add the sample id to these trees")
  tree_locs_sample <- cbind.data.frame(tree_locs_sample, sampleid)
  print("check if the file is already in the directory")
  fname <- paste0("tree_locations_sample_", tag, ".shp")
  check <- list.files(pattern = glob2rx(fname))
  if (length(check) >= 1){
    st_write(tree_locs_sample, fname, update = FALSE, delete_dsn = TRUE)
  } else{
    st_write(tree_locs_sample, fname)
  }
  
  sample_id_df <- dplyr::select(tree_locs_sample, treeID, sampleid)
  print("Creating the 2013 data frame")
  trees_2013_sample <- dplyr::filter(trees_2013, treeID %in% sample_id_df$treeID)
  
  # Arrange both data frames by treeID
  trees_2013_sample <- arrange(trees_2013_sample, by = "treeID")
  sample_id_df <- arrange(sample_id_df, by = "treeID")
  
  # See if the data frames are identical
  if (identical(trees_2013_sample$treeID, sample_id_df$treeID)){
    sampleid <- sample_id_df$sampleid
    trees_2013_sample <- cbind.data.frame(trees_2013_sample, sampleid)
    fname <- paste0("trees_2013_sample_", tag, ".shp")
    check <- list.files(pattern = glob2rx(fname))
    if (length(check) >= 1){
      st_write(trees_2013_sample, fname, update = FALSE, delete_dsn = TRUE)
    } else{
      st_write(trees_2013_sample, fname)
    }
  } else{
    stop("Indices did not match up")
  }
  
  trees_2017 <- read_sf(paste0(home_dir, "tree_objects_2017_las_intersection.shp")) 
  trees_2017 <- trees_2017[task_seq,]
  trees_2018 <- read_sf(paste0(home_dir, "tree_objects_2018_las_intersection.shp")) 
  trees_2018 <- trees_2018[task_seq,]
  trees_2019 <- read_sf(paste0(home_dir, "tree_objects_2019_las_intersection.shp")) 
  trees_2019 <- trees_2019[task_seq,]
  trees_2021 <- read_sf(paste0(home_dir, "tree_objects_2021_las_intersection.shp")) 
  trees_2021 <- trees_2021[task_seq,]
  
  trees_2017_sample <- dplyr::filter(trees_2017, treeID %in% sample_id_df$treeID)
  
  # Arrange both data frames by treeID
  trees_2017_sample <- arrange(trees_2017_sample, by = "treeID")
  sample_id_df <- arrange(sample_id_df, by = "treeID")
  
  # See if the data frames are identical
  if (identical(trees_2017_sample$treeID, sample_id_df$treeID)){
    trees_2017_sample <- mutate(trees_2017_sample, sampleid = sample_id_df$sampleid)
    fname <- paste0("trees_2017_sample_", tag, ".shp")
    check <- list.files(pattern = glob2rx(fname))
    if (length(check) >= 1){
      st_write(trees_2017_sample, fname, update = FALSE, delete_dsn = TRUE)
    } else{
      st_write(trees_2017_sample, fname)
    }
  } else{
    stop("Indices did not match up")
  }
  
  trees_2018_sample <- dplyr::filter(trees_2018, treeID %in% sample_id_df$treeID)
  
  # Arrange both data frames by treeID
  trees_2018_sample <- arrange(trees_2018_sample, by = "treeID")
  sample_id_df <- arrange(sample_id_df, by = "treeID")
  
  # See if the data frames are identical
  if (identical(trees_2018_sample$treeID, sample_id_df$treeID)){
    trees_2018_sample <- mutate(trees_2018_sample, sampleid = sample_id_df$sampleid)
    fname <- paste0("trees_2018_sample_", tag, ".shp")
    check <- list.files(pattern = glob2rx(fname))
    if (length(check) >= 1){
      st_write(trees_2018_sample, fname, update = FALSE, delete_dsn = TRUE)
    } else{
      st_write(trees_2018_sample, fname)
    }
  } else{
    stop("Indices did not match up")
  }
  
  trees_2019_sample <- dplyr::filter(trees_2019, treeID %in% sample_id_df$treeID)
  
  # Arrange both data frames by treeID
  trees_2019_sample <- arrange(trees_2019_sample, by = "treeID")
  sample_id_df <- arrange(sample_id_df, by = "treeID")
  
  # See if the data frames are identical
  if (identical(trees_2019_sample$treeID, sample_id_df$treeID)){
    trees_2019_sample <- mutate(trees_2019_sample, sampleid = sample_id_df$sampleid)
    fname <- paste0("trees_2019_sample_", tag, ".shp")
    check <- list.files(pattern = glob2rx(fname))
    if (length(check) >= 1){
      st_write(trees_2019_sample, fname, update = FALSE, delete_dsn = TRUE)
    } else{
      st_write(trees_2019_sample, fname)
    }
  } else{
    stop("Indices did not match up")
  }
  
  trees_2021_sample <- dplyr::filter(trees_2021, treeID %in% sample_id_df$treeID)
  
  # Arrange both data frames by treeID
  trees_2021_sample <- arrange(trees_2021_sample, by = "treeID")
  sample_id_df <- arrange(sample_id_df, by = "treeID")
  
  # See if the data frames are identical
  if (identical(trees_2021_sample$treeID, sample_id_df$treeID)){
    trees_2021_sample <- mutate(trees_2021_sample, sampleid = sample_id_df$sampleid)
    fname <- paste0("trees_2021_sample_", tag, ".shp")
    check <- list.files(pattern = glob2rx(fname))
    if (length(check) >= 1){
      st_write(trees_2021_sample, fname, update = FALSE, delete_dsn = TRUE)
    } else{
      st_write(trees_2021_sample, fname)
    }
  } else{
    stop("Indices did not match up")
  }
  
  
  
} else{
  print("no trees found")
}
