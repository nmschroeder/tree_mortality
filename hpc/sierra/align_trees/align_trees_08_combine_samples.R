#!/usr/bin/env Rscript

library(dplyr)
library(sf)

data_dir <- "/path/to/your/data/directory/"
home_dir <- paste0(data_dir, "sierra")
save_dir <- home_dir

setwd(home_dir)

tags <- c("trees_2013_sample", "trees_2017_sample", "trees_2018_sample", "trees_2019_sample", "trees_2021_sample", "tree_locations_sample")

for (tag in tags){
  print(tag)
  fnames <- list.files(path = home_dir, pattern = glob2rx(paste0(tag, "_*.shp")), full.names = TRUE)
  print(fnames)
  print("reading in the shapefiles")
  tree_data <- lapply(fnames, read_sf)
  print(tree_data[[1]])
  print("binding together the data")
  trees_df <- do.call(rbind.data.frame, tree_data)
  print(str(trees_df))
  print("checking if there is a file already saved")
  fname <- paste0(tag, ".shp")
  check <- list.files(path = save_dir, pattern = fname)
  
  print(check)
  if (length(check)>0){
    st_write(trees_df, paste0(save_dir, fname), append = FALSE, delete_dsn=TRUE)
  } else{
    st_write(trees_df, paste0(save_dir, fname))
  }
}
