#!/usr/bin/env Rscript

# HPC
data_dir <- "/path/to/your/data/directory/"
home_dir <- paste0(data_dir, "sierra")

library(rgdal)
library(rgeos)
library(sf)
library(dplyr)
library(stringr)

# Read in the task indices
fname <- "align_trees_task_array_indices.txt"
idx_tags <- read.csv(fname, header = FALSE)
print(idx_tags)

# Collapse the indices into a character array of index tags to match the filenames
idx_tags <- sapply(idx_tags, as.character) %>% apply(1, paste0, collapse = "_")
idx_tags
# Let's put the lidar years in here for now
years <- c("2013", "2017", "2018", "2019", "2021")

# Here's the starting string
st <- "tree_objects"

treeID_start <- 1
csv_list <- list()
shps_2013 <- list()
shps_2017 <- list()
shps_2018 <- list()
shps_2019 <- list()
shps_2021 <- list()

count <- 0
# Let's go through each set of task indices and look for entries
# (pairs of task indices where there were no trees would have ended
# without saving shapefiles or csvs)
for (i in 1:length(idx_tags)){
  tag <- idx_tags[i]
  print(tag)
  file_list <- list.files(path = home_dir,
                          pattern = glob2rx(paste0("tree_objects*_",tag,".*")),
                          recursive = FALSE,
                          full.names = TRUE)
  print(file_list[1])
  if (length(file_list)>0){
    print(tag)
    count <- count + 1
    idx <- which(str_detect(file_list, pattern = ".csv"))
    csv_file <- file_list[idx]
    print("csv file check")
    print(csv_file)
    csv_df <- read.csv(csv_file)
    n_entries <- dim(csv_df)[1]
    treeID_end <- treeID_start + n_entries - 1
    new_treeID <- treeID_start:treeID_end
    treeID_start <- treeID_start + n_entries
    csv_df <- dplyr::select(csv_df, -treeID)
    csv_df <- mutate(csv_df, treeID = new_treeID)
    csv_list[[count]] <- csv_df
    
    idx_2013 <- which(str_detect(file_list, pattern = paste0("2013_", tag, ".shp")))
    if (length(idx_2013)>0){
      fname <- file_list[idx_2013]
      shps <- read_sf(fname)
      shps <- dplyr::select(shps, -treeID)
      shps <- mutate(shps, treeID = new_treeID)
      shps_2013[[count]] <- shps
    }
    
    idx_2017 <- which(str_detect(file_list, pattern = paste0("2017_", tag, ".shp")))
    if (length(idx_2017)>0){
      fname <- file_list[idx_2017]
      shps <- read_sf(fname)
      shps <- dplyr::select(shps, -treeID)
      shps <- mutate(shps, treeID = new_treeID)
      shps_2017[[count]] <- shps
    }
    
    idx_2018 <- which(str_detect(file_list, pattern = paste0("2018_", tag, ".shp")))
    if (length(idx_2018)>0){
      fname <- file_list[idx_2018]
      shps <- read_sf(fname)
      shps <- dplyr::select(shps, -treeID)
      shps <- mutate(shps, treeID = new_treeID)
      shps_2018[[count]] <- shps
    }
    
    idx_2019 <- which(str_detect(file_list, pattern = paste0("2019_", tag, ".shp")))
    if (length(idx_2019)>0){
      fname <- file_list[idx_2019]
      shps <- read_sf(fname)
      shps <- dplyr::select(shps, -treeID)
      shps <- mutate(shps, treeID = new_treeID)
      shps_2019[[count]] <- shps
    }
    
    idx_2021 <- which(str_detect(file_list, pattern = paste0("2021_", tag, ".shp")))
    if (length(idx_2021)>0){
      fname <- file_list[idx_2021]
      shps <- read_sf(fname)
      shps <- dplyr::select(shps, -treeID)
      shps <- mutate(shps, treeID = new_treeID)
      shps_2021[[count]] <- shps
    }

  }
    
}

# Save the files
for (y in years){
  # Pull the filename
  fname <- paste0(st, "_", y, ".shp")
  
  # Which year's dataframe do we want to save?
  if (y == "2013"){shps <- shps_2013}
  if (y == "2017"){shps <- shps_2017}
  if (y == "2018"){shps <- shps_2018}
  if (y == "2019"){shps <- shps_2019}
  if (y == "2021"){shps <- shps_2021}
  
  # Combine the list (okay if some elements are NULL)
  shps <- do.call(rbind.data.frame, shps)
  
  # Check to see if the shapefile exists
  file_check <- list.files(path = home_dir, pattern = fname)
  
  if (length(file_check) == 0){
    # If not, just make the shapefile
    st_write(shps, fname)
  } else{
    # If it does exist, then delete the previous one before making a new one
    # (or it will throw an error)
    st_write(shps, fname, update = FALSE, delete_dsn = TRUE)
  }
}

# CSV files
csvs <- do.call(rbind.data.frame, csv_list)
fname <- paste0(home_dir, "/", st,".csv")
write.csv(csvs, fname)
