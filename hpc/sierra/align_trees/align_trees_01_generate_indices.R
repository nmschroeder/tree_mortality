#!/usr/bin/env Rscript

sites <- c("SOAP", "TEAK")
N_per_array <- 100

# Script to read in all NEON lidar files and create aligned tree polygon shapefiles
# The inputs are the NEON .las files 
# The outputs are 4 shapefiles, one for each year of lidar

# HPC
data_dir <- "/path/to/your/data/directory/"
lidar_dir <- paste0(data_dir, "NEON/lidar")

## File to create a canopy height model and tree polygons using NEON lidar data
## and NAIP imagery for SOAP, TEAK, years = 2016, 2018, 2020 (NAIP)

library(tidyverse)
library(raster)
library(pracma)
library(rgdal)
library(rgeos)
library(sf)
library(lidR)
library(RStoolbox)
library(exactextractr)
library(dplyr)
library(concaveman)
library(stringr)

# Making a canopy height model uses a lot of memory, so we need to cut the
# las files into smaller pieces

# Define the size here
dx <- 96   # meters
dy <- 96  # meters

# Tree returns in the buffer zone will be included for tree tops in the
# region of interest but not for tree tops in the buffer. This allows us to 
# naturally draw trees outside the lines without drawing polygons around partial
# trees (most of the time)
bff <- 24 # meters
flag <- 0

w_bnd <- 280000
L_buffer <- 10

remove_outlying_returns <- function(ctg, west_bnd, L_buffer){
  # Pull the extent of the input catalog
  L_ext <- extent(ctg)
  
  if (L_ext@xmin < west_bnd){
    L_ext@xmin <- west_bnd
    # Add a small buffer to the new extent to avoid clipping small sections
    # of the LAS catalog
    L_ext@xmax <- L_ext@xmax + L_buffer
    L_ext@ymin <- L_ext@ymin - L_buffer
    L_ext@ymax <- L_ext@ymax + L_buffer
    
    # Make a spatial points polygon to clip
    x_coord <- c(L_ext@xmin, L_ext@xmin, L_ext@xmax, L_ext@xmax)
    y_coord <- c(L_ext@ymin, L_ext@ymax, L_ext@ymax, L_ext@ymin)
    xy <- cbind(x_coord, y_coord)
    polyclip <- Polygon(xy)
    polyclip <- Polygons(list(polyclip), 1)
    polyclip <- SpatialPolygons(list(polyclip))
    proj4string(polyclip) <- "+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
    ctg_new <- catalog_intersect(ctg, polyclip)
  } else{
    ctg_new <- ctg
  }
  return(ctg_new)
}

for (i in 1:length(sites)){
  # Collect all neon lidar files in the data directory with their full filenames
  fnames_laz <- list.files(lidar_dir, pattern = glob2rx(paste0("*",sites[i],"*_classified_point_cloud*.laz")), 
                           recursive = TRUE, full.names = TRUE)
  
  
  lidar_year_extract <- function(fname){
    yr <- str_extract(fname, pattern = "/[0-9]{4}/")
    return(as.numeric(gsub("/", "", yr)))
  }
  
  test <- lidar_year_extract(fnames_laz[1])
  
  lidar_years <- lapply(fnames_laz, lidar_year_extract)
  lidar_years <- do.call(c, lidar_years)
  
  df_laz <- data.frame(fnames_laz = fnames_laz, lidar_years = lidar_years, stringsAsFactors = FALSE)
  lidar_years_unique <- unique(lidar_years)
  
  # Record the number of laz files
  N_las <- length(fnames_laz)
  
  # Reading the catalog allows you to save a catalog
  # object that is quite small memory-wise. You can then clip it to 
  # a region of interest as a full las object (takes up a lot of memory)
  L_catalog_2013 <- readLAScatalog(dplyr::filter(df_laz, lidar_years == 2013)$fnames_laz)
  L_catalog_2017 <- readLAScatalog(dplyr::filter(df_laz, lidar_years == 2017)$fnames_laz)
  L_catalog_2018 <- readLAScatalog(dplyr::filter(df_laz, lidar_years == 2018)$fnames_laz)
  L_catalog_2019 <- readLAScatalog(dplyr::filter(df_laz, lidar_years == 2019)$fnames_laz)
  L_catalog_2021 <- readLAScatalog(dplyr::filter(df_laz, lidar_years == 2021)$fnames_laz)
  
  L_catalog_2013 <- remove_outlying_returns(L_catalog_2013, w_bnd, L_buffer)
  L_catalog_2017 <- remove_outlying_returns(L_catalog_2017, w_bnd, L_buffer)
  L_catalog_2018 <- remove_outlying_returns(L_catalog_2018, w_bnd, L_buffer)
  L_catalog_2019 <- remove_outlying_returns(L_catalog_2019, w_bnd, L_buffer)
  L_catalog_2021 <- remove_outlying_returns(L_catalog_2021, w_bnd, L_buffer)
  
  L_extent_2013 <- extent(L_catalog_2013)
  L_extent_2017 <- extent(L_catalog_2017)
  L_extent_2018 <- extent(L_catalog_2018)
  L_extent_2019 <- extent(L_catalog_2019)
  L_extent_2021 <- extent(L_catalog_2021)
  
  # Find the extent which represents the union of the 2013, 2017, 2018, 2019, and 2021 lidar data
  L_extent <- c(xmin = min(c(L_extent_2013@xmin, L_extent_2017@xmin, L_extent_2018@xmin, L_extent_2019@xmin, L_extent_2021@xmin)),
                xmax = max(c(L_extent_2013@xmax, L_extent_2017@xmax, L_extent_2018@xmax, L_extent_2019@xmax, L_extent_2021@xmax)),
                ymin = min(c(L_extent_2013@ymin, L_extent_2017@ymin, L_extent_2018@ymin, L_extent_2019@ymin, L_extent_2021@ymin)),
                ymax = max(c(L_extent_2017@ymax, L_extent_2017@ymax, L_extent_2018@ymax, L_extent_2019@ymax, L_extent_2021@ymax)))
  
  L_extent <- extent(L_extent)
  
  # Create a grid to loop through for each extent
  xseq <- seq(L_extent@xmin + bff, L_extent@xmax - bff, dx)
  xseq_lb <- head(xseq, -1) - bff
  xseq_lb[1] <- L_extent@xmin
  xseq_ub <- tail(xseq, -1) + bff
  
  yseq <- seq(L_extent@ymin + bff, L_extent@ymax - bff, dy)
  yseq_lb <- head(yseq, -1) - bff
  yseq_lb[1] <- L_extent@ymin
  yseq_ub <- tail(yseq, -1) + bff
  
  XYseq <- meshgrid(xseq, yseq)
  XYseq_lb <- meshgrid(xseq_lb, yseq_lb)
  XYseq_ub <- meshgrid(xseq_ub, yseq_ub)
  
  xseq_lb_long <- c(XYseq_lb$X)
  yseq_lb_long <- c(XYseq_lb$Y)
  xseq_ub_long <- c(XYseq_ub$X)
  yseq_ub_long <- c(XYseq_ub$Y)
  
  if (flag == 0){
    # Initialize data frame
    index_df <- data.frame(site = sites[i],
                           xseq_lb = xseq_lb_long,
                           xseq_ub = xseq_ub_long,
                           yseq_lb = yseq_lb_long,
                           yseq_ub = yseq_ub_long,
                           stringsAsFactors = FALSE)
    flag <- 1
  } else{
    # Otherwise, add to the data frame
    index_next_site <- data.frame(site = sites[i],
                           xseq_lb = xseq_lb_long,
                           xseq_ub = xseq_ub_long,
                           yseq_lb = yseq_lb_long,
                           yseq_ub = yseq_ub_long,
                           stringsAsFactors = FALSE)
    index_df <- rbind.data.frame(index_df, index_next_site)
  }
}

# Create a data frame
idx <- data.frame(idx = 1:dim(index_df)[1])
index_df_complete <- cbind.data.frame(idx, index_df)
str(index_df_complete)

# Save the data frame without quotes or row names to feed into each task array 
write.table(index_df_complete, file = "align_trees_indices.csv", 
            quote = FALSE, sep = ",", row.names = FALSE, col.names = TRUE)

# Okay, we still need to create the indices for the chunks each task array element should use

# We also need to make sure we only group the same site together since we don't want all the empty
# space between sites included in our indices
flag <- 0
for (i in 1:length(sites)){
  index_df_site <- dplyr::filter(index_df_complete, site == sites[i])
  n <-  dim(index_df_site)[1]
  idx_n <- index_df_site$idx[n]
  task_idx <- seq(index_df_site$idx[1],idx_n, by = N_per_array)
  
  if (task_idx[length(task_idx)] < idx_n){
    task_idx <- c(task_idx, idx_n)
  }
  
  idx_start <- task_idx[1:(length(task_idx)-1)]
  idx_end <- task_idx[2:length(task_idx)]-1
  idx_end[length(idx_end)]<-idx_n
  
  if (flag == 0){
    task_indices_df <- data.frame(idx_start = idx_start,
                                  idx_end = idx_end)
    flag <- 1
  } else{
    task_indices_temp <- data.frame(idx_start = idx_start,
                                  idx_end = idx_end)
    task_indices_df <- rbind.data.frame(task_indices_df, task_indices_temp)
  }
}

# Check
head(task_indices_df)
tail(task_indices_df)

write.table(task_indices_df, file = "align_trees_task_array_indices.txt", quote = FALSE,
            row.names = FALSE, col.names = FALSE, sep = ",")
