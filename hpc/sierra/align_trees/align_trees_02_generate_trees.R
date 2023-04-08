#!/usr/bin/env Rscript
task_idx <- commandArgs(trailingOnly = TRUE)

# Script to read in all NEON lidar files and create aligned tree polygon shapefiles
# The inputs are the NEON .las files and align_trees_indices.csv 
# The outputs are 4 shapefiles, one for each year of lidar and a csv file of ancillary data

# HPC
data_dir <- "/path/to/your/data/directory/"
home_dir <- paste0(data_dir, "sierra")
lidar_dir <- paste0(data_dir, "NEON/lidar")

library(tidyverse)
library(raster)
library(sf)
library(lidR)
library(exactextractr)
library(dplyr)
library(concaveman)
library(stringr)

# Read in and split the task array index string
task_idx <- str_split(task_idx, pattern = ",")
task_idx <- do.call(c, task_idx)

# Create and print the tag for the output files
tag <- paste(task_idx, collapse = "_")

# Create a numeric array for the starting and ending index in the csv file
# of all the indices we need to loop through
task_idx <- as.numeric(task_idx)

task_seq <- task_idx[1]:task_idx[2]
print(tag)

# Read in the csv file with all the indices we need to loop through
idx_df <- read.csv("align_trees_indices.csv")
print("read in index file")

# Filter the resulting data frame for only the indices indicated by the task array inputs
task_df <- dplyr::filter(idx_df, idx %in% task_seq)
site <- task_df$site[1]
xseq_lb <- task_df$xseq_lb
xseq_ub <- task_df$xseq_ub
yseq_lb <- task_df$yseq_lb
yseq_ub <- task_df$yseq_ub
N <- dim(task_df)[1]

print(site)

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


## Helpful functions

# Function file for drawing a polygon around x,y-coordinates for each group in a grouped dataframe
tree_polygons <- function(tree_df, concavity){
  # This function goes through one tree for a given year at a time
  tree_df <- st_as_sf(tree_df, coords = c("x", "y"), crs = 32611, agr = "constant")
  pgon <- concaveman(tree_df, concavity)
  # Find the maximum for this year
  idx <- which.max(tree_df$z)
  return(mutate(pgon, 
                treeID = tree_df$treeID[1], 
                x_las = tree_df$x_las[idx], 
                y_las = tree_df$y_las[idx],
                x_naip = tree_df$x_naip[idx],
                y_naip = tree_df$y_naip[idx],
                zmax = tree_df$z[idx],
                las_year = tree_df$las_year[1]))
}

# This function ...
pivot_las <- function(tree){
  las_years <- unique(tree$las_year)
  empty_pgon <- st_sfc(st_polygon(), crs = 32611)
  df_template <- data.frame(treeID = tree$treeID[1], geo2013 = empty_pgon, geo2017 = empty_pgon, geo2018 = empty_pgon, geo2019 = empty_pgon, geo2021 = empty_pgon,
                            xlas2013 = NA, xlas2017 = NA, xlas2018 = NA, xlas2019 = NA, xlas2021 = NA,
                            ylas2013 = NA, ylas2017 = NA, ylas2018 = NA, ylas2019 = NA, ylas2021 = NA,
                            zmax2013 = NA, zmax2017 = NA, zmax2018 = NA, zmax2019 = NA, zmax2021 = NA)
  # Enforce the geometry names
  colnames(df_template)<-(c("treeID", "geo2013", "geo2017", "geo2018", "geo2019", "geo2021", 
                            "xlas2013", "xlas2017", "xlas2018", "xlas2019", "xlas2021",
                            "ylas2013", "ylas2017", "ylas2018", "ylas2019", "ylas2021",
                            "zmax2013", "zmax2017", "zmax2018", "zmax2019", "zmax2021"))
  for (it in 1:length(las_years)){
    yr <- las_years[it]
    temp <- dplyr::filter(tree, las_year == yr)
    if (yr == 2013){
      df_template$geo2013 <- temp$polygons
      df_template$xlas2013 <- temp$x_las
      df_template$ylas2013 <- temp$y_las
      df_template$zmax2013 <- temp$zmax
    }
    
    if (yr == 2017){
      df_template$geo2017 <- temp$polygons
      df_template$xlas2017 <- temp$x_las
      df_template$ylas2017 <- temp$y_las
      df_template$zmax2017 <- temp$zmax
      
    }
    
    if (yr == 2018){
      df_template$geo2018 <- temp$polygons
      df_template$xlas2018 <- temp$x_las
      df_template$ylas2018 <- temp$y_las
      df_template$zmax2018 <- temp$zmax
      
    }
    
    if (yr == 2019){
      df_template$geo2019 <- temp$polygons
      df_template$xlas2019 <- temp$x_las
      df_template$ylas2019 <- temp$y_las
      df_template$zmax2019 <- temp$zmax
      
    }
    
    if (yr == 2021){
      df_template$geo2021 <- temp$polygons
      df_template$xlas2021 <- temp$x_las
      df_template$ylas2021 <- temp$y_las
      df_template$zmax2021 <- temp$zmax
      
    }
    
  }
  return(df_template)
}

las_intersect <- function(las_ext_list){
  N_list <- length(las_ext_list)
  xmin_vec <- vector(length = N_list)
  xmax_vec <- vector(length = N_list)
  ymin_vec <- vector(length = N_list)
  ymax_vec <- vector(length = N_list)
  for (i in 1:N_list){
    xmin_vec[i] <- las_ext_list[[i]]@xmin
    xmax_vec[i] <- las_ext_list[[i]]@xmax
    ymin_vec[i] <- las_ext_list[[i]]@ymin
    ymax_vec[i] <- las_ext_list[[i]]@ymax
  }
  new_extent <- extent(c(xmin = max(xmin_vec), xmax = min(xmax_vec),
                         ymin = max(ymin_vec), ymax = min(ymax_vec)))
  return(new_extent)
}

print("Read in functions")

print("Collecting neon lidar files")
# Collect all neon lidar files in the data directory with their full filenames
fnames_laz <- list.files(lidar_dir, pattern = glob2rx(paste0("*",site,"*_classified_point_cloud*.laz")), 
                          recursive = TRUE, full.names = TRUE)

# Record the number of laz files
N_las <- length(fnames_laz)

# Check that we found the laz files
print("Number of laz files found:")
print(N_las)

print("First laz file")
print(fnames_laz[1])

# Function to extract the year from a NEON lidar filename
lidar_year_extract <- function(fname){
  yr <- str_extract(fname, pattern = paste0("/[0-9]{4}/",site,"/"))
  yr <- str_extract(yr, pattern = "[0-9]{4}")
  return(as.numeric(yr))
}

# Extract the years for each filename
lidar_years <- lapply(fnames_laz, lidar_year_extract)

# Bind the list together into a column
lidar_years <- do.call(c, lidar_years)

# Create a data frame for the laz files with their filenames and lidar years
df_laz <- data.frame(fnames_laz = fnames_laz, lidar_years = lidar_years, stringsAsFactors = FALSE)

print("Laz file data frame")
print(str(df_laz))

# How many unique lidar years did we find?
lidar_years_unique <- unique(lidar_years)
print(lidar_years_unique)

print("Reading in las catalogs")

# Reading the catalog allows you to save a catalog
# object that is quite small memory-wise. You can then clip it to 
# a region of interest as a full las object (takes up a lot of memory)

# The 2013 catalog has some outliers far west of the rest of the returns. Let's clip anything west of 280000
# to start (pretty wide window around returns)

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


w_bnd <- 280000
L_buffer <- 10

L_catalog_2013 <- readLAScatalog(dplyr::filter(df_laz, lidar_years == 2013)$fnames_laz)
L_catalog_2013 <- remove_outlying_returns(L_catalog_2013, w_bnd, L_buffer)
L_catalog_2017 <- readLAScatalog(dplyr::filter(df_laz, lidar_years == 2017)$fnames_laz)
L_catalog_2017 <- remove_outlying_returns(L_catalog_2017, w_bnd, L_buffer)
L_catalog_2018 <- readLAScatalog(dplyr::filter(df_laz, lidar_years == 2018)$fnames_laz)
L_catalog_2018 <- remove_outlying_returns(L_catalog_2018, w_bnd, L_buffer)
L_catalog_2019 <- readLAScatalog(dplyr::filter(df_laz, lidar_years == 2019)$fnames_laz)
L_catalog_2019 <- remove_outlying_returns(L_catalog_2019, w_bnd, L_buffer)
L_catalog_2021 <- readLAScatalog(dplyr::filter(df_laz, lidar_years == 2021)$fnames_laz)
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
                     ymax = max(c(L_extent_2013@ymax, L_extent_2017@ymax, L_extent_2018@ymax, L_extent_2019@ymax, L_extent_2021@ymax)))

# Make this column vector with the extent an extent object (raster package)
L_extent <- extent(L_extent)

# Designate a resolution for the canopy height model
r <- 1 # meters

print("Timing the loop")

# In this loop, we run our dx by dy filter through the extent
ptm <- proc.time()


filter_noise = function(las){
  # Determine the center of the data and clip any noise clouds above or below the trees
  for (i in 1:2){ # Complete two passes
    meanz <- mean(las$Z, na.rm = TRUE)
    sdz <- sd(las$Z, na.rm = TRUE)
    print(sdz)
    idx <- las$Z < meanz - 5*sdz | las$Z > meanz + 5*sdz
    las@data <- las@data[!idx,]
  }
  return(las)
}

# Wait to clip the lidar clouds by height until they've been normalized
las_corrected_clip <- function(las){
  idx <- las$Z>120
  las@data <- las@data[!idx,]
  return(las)
}

colnames_check <- function(las_obj){
  las_dataframe <- las_obj@data
  if (any(c("R", "G", "B") %in% colnames(las_dataframe))){
    las_dataframe <- dplyr::select(las_dataframe, -R, -G, -B)
    las_obj@data <- las_dataframe
  }
  return(las_obj)
}


flag <- 0

print("Starting for-loop")

for (i in 1:N){
  print("i = ")
  i
  # Clip rectangle - we only need one of them to be available
  skip_to_next <<- FALSE
  skip_to_next_1 <<- FALSE
  skip_to_next_2 <<- FALSE
  skip_to_next_3 <<- FALSE
  skip_to_next_4 <<- FALSE
  skip_to_next_5 <<- FALSE
  
  # Clip the rectangles
  print("Clip the rectangles")
  las_2013 <- tryCatch(clip_rectangle(L_catalog_2013, xseq_lb[i], yseq_lb[i], xseq_ub[i], yseq_ub[i], filter = "-drop_z_below 0"), error = function(e) {skip_to_next_1 <<- TRUE})
  if (!skip_to_next_1){
    if (area(las_2013) < 0.95*(dx + 2*bff)*(dy + 2*bff)){
      skip_to_next_1 <<- TRUE
    }
  }
  las_2017 <- tryCatch(clip_rectangle(L_catalog_2017, xseq_lb[i], yseq_lb[i], xseq_ub[i], yseq_ub[i], filter = "-drop_z_below 0"), error = function(e) {skip_to_next_2 <<- TRUE})
  if (!skip_to_next_2){
    if (area(las_2017) < 0.95*(dx + 2*bff)*(dy + 2*bff)){
      skip_to_next_2 <<- TRUE
    }
  }
  las_2018 <- tryCatch(clip_rectangle(L_catalog_2018, xseq_lb[i], yseq_lb[i], xseq_ub[i], yseq_ub[i], filter = "-drop_z_below 0"), error = function(e) {skip_to_next_3 <<- TRUE})
  if (!skip_to_next_3){
    if (area(las_2018) < 0.95*(dx + 2*bff)*(dy + 2*bff)){
      skip_to_next_3 <<- TRUE
    }
  }
  las_2019 <- tryCatch(clip_rectangle(L_catalog_2019, xseq_lb[i], yseq_lb[i], xseq_ub[i], yseq_ub[i], filter = "-drop_z_below 0"), error = function(e) {skip_to_next_4 <<- TRUE})
  if (!skip_to_next_4){
    if (area(las_2019) < 0.95*(dx + 2*bff)*(dy + 2*bff)){
      skip_to_next_4 <<- TRUE
    }
  }
  
  las_2021 <- tryCatch(clip_rectangle(L_catalog_2021, xseq_lb[i], yseq_lb[i], xseq_ub[i], yseq_ub[i], filter = "-drop_z_below 0"), error = function(e) {skip_to_next_5 <<- TRUE})
  if (!skip_to_next_5){
    if (area(las_2021) < 0.95*(dx + 2*bff)*(dy + 2*bff)){
      skip_to_next_5 <<- TRUE
    }
  }
  
  skip_to_next <- all(c(skip_to_next_1, skip_to_next_2, skip_to_next_3, skip_to_next_4, skip_to_next_5))
  
  # If there are no points in any of the LAS files for this window, skip to the next iteration to try the next window
  if(skip_to_next) { next }
  
  print("Filter noise and combine")
  # Filter noise and combine
  las <- list()
  las_gt <- list()
  it <- 1
  print("2013")
  if (!skip_to_next_1){
    las_2013 <- filter_noise(las_2013)
    tryCatch(las_gt_2013 <- grid_terrain(las_2013, r, knnidw()), error = function(e) { skip_to_next_1 <<- TRUE})
    tryCatch(las_corrected_2013 <- normalize_height(las_2013, algorithm  = las_gt_2013, na.rm = TRUE), error = function(e) { skip_to_next_1 <<- TRUE})
    if (!skip_to_next_1){ las_corrected_2013 <- las_corrected_clip(las_corrected_2013)}
    tryCatch(chm_2013 <- grid_canopy(las_corrected_2013, res = r, pitfree(c(0,2,5,10,15), c(0, 1.5))), error = function(e) { skip_to_next_1 <<- TRUE})
    if (!skip_to_next_1){
      chm_2013[chm_2013<0] <- 0
      chm_2013[chm_2013>120] <- NA
      las[[it]] <- las_corrected_2013
      las_gt[[it]] <- las_gt_2013
      it <- it + 1
    }
  }
  print("2017")
  if (!skip_to_next_2){
    las_2017 <- filter_noise(las_2017)
    
    tryCatch(las_gt_2017 <- grid_terrain(las_2017, r, knnidw()), error = function(e) { skip_to_next_2 <<- TRUE})
    tryCatch(las_corrected_2017 <- normalize_height(las_2017, algorithm  = las_gt_2017, na.rm = TRUE), error = function(e) { skip_to_next_2 <<- TRUE})
    if (!skip_to_next_2){ las_corrected_2017 <- las_corrected_clip(las_corrected_2017)}
    tryCatch(chm_2017 <- grid_canopy(las_corrected_2017, res = r, pitfree(c(0,2,5,10,15), c(0, 1.5))), error = function(e) { skip_to_next_2 <<- TRUE})
    if (!skip_to_next_2){
      chm_2017[chm_2017<0] <- 0
      chm_2017[chm_2017>120] <- NA
      las[[it]] <- las_corrected_2017
      las_gt[[it]] <- las_gt_2017
      it <- it + 1
    }
  }
  print("2018")
  if (!skip_to_next_3){
    las_2018 <- filter_noise(las_2018)
    if ("reversible index (lastile)" %in% colnames(las_2018@data)){
      las_2018@data <- dplyr::select(las_2018@data, -`reversible index (lastile)`)
    }
    tryCatch(las_gt_2018 <- grid_terrain(las_2018, r, knnidw()), error = function(e) { skip_to_next_3 <<- TRUE})
    tryCatch(las_corrected_2018 <- normalize_height(las_2018, algorithm  = las_gt_2018, na.rm = TRUE), error = function(e) { skip_to_next_3 <<- TRUE})
    if (!skip_to_next_3){ las_corrected_2018 <- las_corrected_clip(las_corrected_2018)}
    tryCatch(chm_2018 <- grid_canopy(las_corrected_2018, res = r, pitfree(c(0,2,5,10,15), c(0, 1.5))), error = function(e) { skip_to_next_3 <<- TRUE})
    if (!skip_to_next_3){
      chm_2018[chm_2018<0] <- 0
      chm_2018[chm_2018>120] <- NA
      las[[it]] <- las_corrected_2018
      las_gt[[it]] <- las_gt_2018
      it <- it + 1
    }
  }
  print("2019")
  if (!skip_to_next_4){
    las_2019 <- filter_noise(las_2019)
    tryCatch(las_gt_2019 <- grid_terrain(las_2019, r, knnidw()), error = function(e) { skip_to_next_4 <<- TRUE})
    tryCatch(las_corrected_2019 <- normalize_height(las_2019, algorithm  = las_gt_2019, na.rm = TRUE), error = function(e) { skip_to_next_4 <<- TRUE})
    if (!skip_to_next_4){ las_corrected_2019 <- las_corrected_clip(las_corrected_2019)}
    tryCatch(chm_2019 <- grid_canopy(las_corrected_2019, res = r, pitfree(c(0,2,5,10,15), c(0, 1.5))), error = function(e) { skip_to_next_4 <<- TRUE})
    if (!skip_to_next_4){
      chm_2019[chm_2019<0] <- 0
      chm_2019[chm_2019>120] <- NA
      las[[it]] <- las_corrected_2019
      las_gt[[it]] <- las_gt_2019
      it <- it + 1
    }
  }
  
  print("2021")
  if (!skip_to_next_5){
    las_2021 <- filter_noise(las_2021)
    tryCatch(las_gt_2021 <- grid_terrain(las_2021, r, knnidw()), error = function(e) { skip_to_next_5 <<- TRUE})
    tryCatch(las_corrected_2021 <- normalize_height(las_2021, algorithm  = las_gt_2021, na.rm = TRUE), error = function(e) { skip_to_next_5 <<- TRUE})
    if (!skip_to_next_5){ las_corrected_2021 <- las_corrected_clip(las_corrected_2021)}
    tryCatch(chm_2021 <- grid_canopy(las_corrected_2021, res = r, pitfree(c(0,2,5,10,15), c(0, 1.5))), error = function(e) { skip_to_next_5 <<- TRUE})
    if (!skip_to_next_5){
      chm_2021[chm_2021<0] <- 0
      chm_2021[chm_2021>120] <- NA
      las[[it]] <- las_corrected_2021
      las_gt[[it]] <- las_gt_2021
    }
  }
  
  print("Combine the the las objects together")
  las <- lapply(las, colnames_check)
  las_corrected <- do.call(rbind, las)
  
  skip_to_next <- FALSE 
  # So here, let's add in a check to see if las_corrected@data even exists 
  tryCatch(las_corrected@data <- filter(las_corrected@data, Z>=0 & Z<120), error = function(e) { skip_to_next <<- TRUE})
  if (skip_to_next){
    next
  }
  
  # Next, the ground truth might not perfectly match up since we're requiring the area to be
  # 95% of the specified area and not 100%
  
  if (length(las_gt)>1){
    las_ext <- lapply(las_gt, extent)
    ext_match <- las_intersect(las_ext)
    las_gt <- lapply(las_gt, crop, y = ext_match)
    las_gt <- do.call(stack, las_gt)
    las_gt <- mean(las_gt, na.rm = TRUE)
  } else{
    las_gt <- las_gt[[1]]
  }
  rm(las)

  print("Finished creating the corrected las object")
  
  tryCatch(chm <- grid_canopy(las_corrected, res = r, pitfree(c(0,2,5,10,15), c(0, 1.5))), error = function(e) { skip_to_next <<- TRUE})
  if (skip_to_next){next}
  
  print("Finished creating the combined canopy height model")
  
  # Clip the canopy height model to avoid noise (below-ground returns and very high returns)
  chm[chm<0] <- 0
  chm[chm>120] <- NA
  
  # Smooth and filter the canopy height model
  ker <- matrix(rep(1, 9), ncol = 3)
  chm_smooth <- focal(chm, ker, fun = median, na.rm = TRUE)
  
  
  f <- function(x){
    y <- (1/8)*x
    y[y<4] <- 4
    y[y>10] <- 10
    return(y)
  }
  
  ttops <- find_trees(chm_smooth, lmf(f))
  #plot(chm)
  #plot(ttops, add = TRUE)
  
  # Mak e a similar one based on diameter
  #p <- plot(las_corrected)
  #add_treetops3d(p, ttops)
  #p
  
  # Collect the coordinates for the identified tree tops
  xy <- coordinates(ttops)
  x <- xy[,1]
  y <- xy[,2]
  
  print("locating tree tops")
  idx <- which(x > (xseq_lb[i] + bff) & x <= (xseq_ub[i] - bff) & 
                 y > (yseq_lb[i] + bff) & y <= (yseq_ub[i] - bff))
  treeIDs <- ttops$treeID[idx]
  
  print(paste0("found ", as.character(length(idx))))
  
  # If there are returns within the window of interest:
  if (length(idx) > 0){
    
    # Make a dataframe from the identified treetops
    print('ttops_df')
    ttops_df <- cbind(data.frame(ttops@data), data.frame(coordinates(ttops)))
    str(ttops_df)
    # Segment the trees using the Dalponte et al 2016 method
    print('segment trees')
    
    
    it <- 1
    las_trees <- list()
    # Need to apply these to the correct canopy height models
    if (!skip_to_next_1){
      #print("Smoothing 2013")
      #chm_2013_smooth <- raster::focal(chm_2013, ker, fun = median, na.rm = TRUE)
      print("Segmentation")
      #las_trees_2013 <- segment_trees(las_corrected_2013, dalponte2016(chm = chm_2013_smooth, treetops = ttops, th_tree = 4, th_seed = 0.65, th_cr = 0.75, max_cr = 10))
      las_trees_2013 <- segment_trees(las_corrected_2013, dalponte2016(chm = chm_2013, treetops = ttops, th_tree = 3, th_seed = 0.75, th_cr = 0.85, max_cr = 10))
      print("Store data")
      las_temp <- las_trees_2013@data
      print("las_temp structure")
      str(las_temp)
      print("Mutate year")
      las_year <- rep(2013, times = dim(las_temp)[1])
      las_temp <- cbind.data.frame(las_temp, las_year) %>% dplyr::select(X, Y, Z, treeID, las_year) %>% drop_na()
      check <- !(treeIDs %in% las_temp$treeID)
      if (any(check)){
        missing_treeIDs <- treeIDs[check]
        las_missing <- dplyr::filter(ttops_df, treeID %in% missing_treeIDs) %>% rename(X = coords.x1, Y = coords.x2) %>% mutate(las_year = rep(2013, times = sum(check))) %>% dplyr::select(-Z)
        xy_sp <- las_missing %>% st_as_sf(coords = c("X", "Y"), crs = 32611) %>% as('Spatial')
        Z <- extract(chm_2013, xy_sp)
        las_missing <- mutate(las_missing, Z=Z)
        las_temp <- rbind.data.frame(las_temp, las_missing) %>% arrange(treeID, las_year)
      }
      str(las_temp)
      las_trees[[it]] <- las_temp
      it <- it + 1
    }
    
    if (!skip_to_next_2){
      #chm_2017_smooth <- raster::focal(chm_2017, ker, fun = median, na.rm = TRUE)
      #las_trees_2017 <- segment_trees(las_corrected_2017, dalponte2016(chm = chm_2017_smooth, treetops = ttops, th_tree = 4, th_seed = 0.65, th_cr = 0.75, max_cr = 10))
      las_trees_2017 <- segment_trees(las_corrected_2017, dalponte2016(chm = chm_2017, treetops = ttops, th_tree = 3, th_seed = 0.75, th_cr = 0.85, max_cr = 10))
      las_temp <- las_trees_2017@data
      print("2017 las_temp structure")
      str(las_temp)
      las_year <- rep(2017, times	= dim(las_temp)[1])
      las_temp <- cbind.data.frame(las_temp, las_year) %>% dplyr::select(X, Y, Z, treeID, las_year) %>% drop_na()
      check <- !(treeIDs %in% las_temp$treeID)
      if (any(check)){
        missing_treeIDs <- treeIDs[check]
        las_missing <- dplyr::filter(ttops_df, treeID %in% missing_treeIDs) %>% rename(X = coords.x1, Y = coords.x2) %>% mutate(las_year = rep(2017, times = sum(check))) %>% dplyr::select(-Z)
        xy_sp <- las_missing %>% st_as_sf(coords = c("X", "Y"), crs = 32611) %>% as('Spatial')
        Z <- extract(chm_2017, xy_sp)
        las_missing <- mutate(las_missing, Z=Z)
        las_temp <- rbind.data.frame(las_temp, las_missing) %>% arrange(treeID, las_year)
      }
      str(las_temp)
      las_trees[[it]] <- las_temp
      it <- it + 1
    }
    
    if (!skip_to_next_3){
      #chm_2018_smooth <- raster::focal(chm_2018, ker, fun = median, na.rm = TRUE)
      #las_trees_2018 <- segment_trees(las_corrected_2018, dalponte2016(chm = chm_2018_smooth, treetops = ttops, th_tree = 4, th_seed = 0.65, th_cr = 0.75, max_cr = 10))
      las_trees_2018 <- segment_trees(las_corrected_2018, dalponte2016(chm = chm_2018, treetops = ttops, th_tree = 3, th_seed = 0.75, th_cr = 0.85, max_cr = 10))
      las_temp <- las_trees_2018@data
      print("2018 las_temp structure")
      str(las_temp)
      las_year <- rep(2018, times	= dim(las_temp)[1])
      las_temp <- cbind.data.frame(las_temp, las_year) %>% dplyr::select(X, Y, Z, treeID, las_year) %>% drop_na()
      check <- !(treeIDs %in% las_temp$treeID)
      if (any(check)){
        missing_treeIDs <- treeIDs[check]
        las_missing <- dplyr::filter(ttops_df, treeID %in% missing_treeIDs) %>% rename(X = coords.x1, Y = coords.x2) %>% mutate(las_year = rep(2018, times = sum(check))) %>% dplyr::select(-Z)
        xy_sp <- las_missing %>% st_as_sf(coords = c("X", "Y"), crs = 32611) %>% as('Spatial')
        Z <- extract(chm_2018, xy_sp)
        las_missing <- mutate(las_missing, Z=Z)
        las_temp <- rbind.data.frame(las_temp, las_missing) %>% arrange(treeID, las_year)
      }
      str(las_temp)
      las_trees[[it]] <- las_temp
      it <- it + 1
    }
    
    if (!skip_to_next_4){
      #chm_2019_smooth <- raster::focal(chm_2019, ker, fun = median, na.rm = TRUE)
      #las_trees_2019 <- segment_trees(las_corrected_2019, dalponte2016(chm = chm_2019_smooth, treetops = ttops, th_tree = 4, th_seed = 0.65, th_cr = 0.75, max_cr = 10))
      las_trees_2019 <- segment_trees(las_corrected_2019, dalponte2016(chm = chm_2019, treetops = ttops, th_tree = 3, th_seed = 0.75, th_cr = 0.85, max_cr = 10))
      las_temp <- las_trees_2019@data
      print("2019 las_temp structure")
      str(las_temp)
      las_year <- rep(2019, times = dim(las_temp)[1])
      las_temp <- cbind.data.frame(las_temp, las_year) %>% dplyr::select(X, Y, Z, treeID, las_year) %>% drop_na()
      check <- !(treeIDs %in% las_temp$treeID)
      if (any(check)){
        missing_treeIDs <- treeIDs[check]
        las_missing <- dplyr::filter(ttops_df, treeID %in% missing_treeIDs) %>% rename(X = coords.x1, Y = coords.x2) %>% mutate(las_year = rep(2019, times = sum(check))) %>% dplyr::select(-Z)
        xy_sp <- las_missing %>% st_as_sf(coords = c("X", "Y"), crs = 32611) %>% as('Spatial')
        Z <- extract(chm_2019, xy_sp)
        las_missing <- mutate(las_missing, Z=Z)
        las_temp <- rbind.data.frame(las_temp, las_missing) %>% arrange(treeID, las_year)
      }
      str(las_temp)
      las_trees[[it]] <- las_temp
      it <- it + 1
    }
    
    if (!skip_to_next_5){
      #chm_2021_smooth <- raster::focal(chm_2021, ker, fun = median, na.rm = TRUE)
      #las_trees_2021 <- segment_trees(las_corrected_2021, dalponte2016(chm = chm_2021_smooth, treetops = ttops, th_tree = 4, th_seed = 0.65, th_cr = 0.75, max_cr = 10))
      las_trees_2021 <- segment_trees(las_corrected_2021, dalponte2016(chm = chm_2021, treetops = ttops, th_tree = 3, th_seed = 0.75, th_cr = 0.85, max_cr = 10))
      las_temp <- las_trees_2021@data
      print("2021 las_temp structure")
      str(las_temp)
      las_year <- rep(2021, times = dim(las_temp)[1])
      las_temp <- cbind.data.frame(las_temp, las_year) %>% dplyr::select(X, Y, Z, treeID, las_year) %>% drop_na()
      check <- !(treeIDs %in% las_temp$treeID)
      if (any(check)){
        missing_treeIDs <- treeIDs[check]
        las_missing <- dplyr::filter(ttops_df, treeID %in% missing_treeIDs) %>% rename(X = coords.x1, Y = coords.x2) %>% mutate(las_year = rep(2021, times = sum(check))) %>% dplyr::select(-Z)
        xy_sp <- las_missing %>% st_as_sf(coords = c("X", "Y"), crs = 32611) %>% as('Spatial')
        Z <- extract(chm_2021, xy_sp)
        las_missing <- mutate(las_missing, Z=Z)
        las_temp <- rbind.data.frame(las_temp, las_missing) %>% arrange(treeID, las_year)
      }
      str(las_temp)
      las_trees[[it]] <- las_temp
    }
    
    las_trees <- do.call(rbind.data.frame, las_trees)      
    las_trees <- drop_na(las_trees)
    # Make an sf data frame (make sure to save original coordinates as well for year-to-year matching)
    print('sf dataframe')
    tree_data <- data.frame(x = las_trees$X, y = las_trees$Y, x_las = las_trees$X, y_las = las_trees$Y, z = las_trees$Z,
                            treeID = las_trees$treeID, las_year = las_trees$las_year) %>%
      dplyr::filter(treeID %in% treeIDs) 
    if (dim(tree_data)[1] > 0){
      # Group the tree data file which includes all the coordinates of the tree returns by treeID
      # Each group contains all the x,y coordinates for a given tree
      # Draw a polygon around each group (tree) using the concaveman package
      print('tree list')
      str(tree_data)
      tree_list <- group_by(tree_data, treeID, las_year) %>% dplyr::group_map(~tree_polygons(.x,concavity = 5), .keep = TRUE)

      print('tree temp do call')
      # Create a temporary tree dataframe 
      tree_temp <- do.call(rbind.data.frame, tree_list)
      LAS_years <- unique(tree_temp$las_year)
      
      # Next, group the trees by tree ID and pivot the data frame so that each
      # tree has only one row with all of the las data by year in the columns
      tree_temp <- group_by(tree_temp, treeID) %>% group_split() 
      tree_temp <- lapply(tree_temp, FUN = pivot_las)
      tree_temp <- do.call(rbind.data.frame, tree_temp)
      
      
      if (dim(tree_temp)[1]>0){
        
        
       if (flag == 0){
          
            print('initializing tree objects')
            tree_objects <- tree_temp
           
            flag <- 1
        } else{
            print('appending tree objects')
            tree_objects <- rbind.data.frame(tree_objects, tree_temp)
        }
      }
    }
  }
}

# Print the time it took to run the loop
print("Time stats for this loop:")
proc.time() - ptm  

if (flag != 0){
  # Write the tree objects to a shape file
  tree_objects <- mutate(tree_objects, treeID = 1:dim(tree_objects)[1])
  str(tree_objects)
  
  tree_objects_2013 <- dplyr::select(tree_objects, geo2013, treeID)
  tree_objects_2017 <- dplyr::select(tree_objects, geo2017, treeID)
  tree_objects_2018 <- dplyr::select(tree_objects, geo2018, treeID)
  tree_objects_2019 <- dplyr::select(tree_objects, geo2019, treeID)
  tree_objects_2021 <- dplyr::select(tree_objects, geo2021, treeID)
  tree_objects <- dplyr::select(tree_objects, -geo2013, -geo2017, -geo2018, -geo2019, -geo2021)
  
  print("Write tree objects for each lidar year")
  # Save the important part before trying anything else...
  str(tree_objects_2013)
  str(tree_objects_2017)
  str(tree_objects_2018)
  str(tree_objects_2019)
  str(tree_objects_2021)
  
  
  if (!all(is.na(tree_objects$zmax2013))){
    fname <- paste0("tree_objects_2013_", tag, ".shp")
    fname_check <- list.files(home_dir, pattern = fname)
    if (length(fname_check) == 0){
      st_write(tree_objects_2013, fname)
    } else{
      print("file exists- deleting")
      st_write(tree_objects_2013, fname, append = FALSE, delete_dsn = TRUE)
    }
  }

  if (!all(is.na(tree_objects$zmax2017))){
    fname <- paste0("tree_objects_2017_", tag, ".shp")
    fname_check <- list.files(home_dir, pattern = fname)
    if (length(fname_check) == 0){
      st_write(tree_objects_2017, fname)
    } else{
      st_write(tree_objects_2017, fname, append = FALSE, delete_dsn = TRUE)
      
    }
  }
  if (!all(is.na(tree_objects$zmax2018))){ 
    fname <- paste0("tree_objects_2018_", tag, ".shp")
    fname_check <- list.files(home_dir, pattern = fname)
    if (length(fname_check) == 0){
      st_write(tree_objects_2018, fname)
    } else{
      st_write(tree_objects_2018, fname, append = FALSE, delete_dsn = TRUE)
      
    }
  }
  if (!all(is.na(tree_objects$zmax2019))){
    fname <- paste0("tree_objects_2019_", tag, ".shp")
    fname_check <- list.files(home_dir, pattern = fname)
    if (length(fname_check) == 0){
      st_write(tree_objects_2019, fname)
    } else{
      st_write(tree_objects_2019, fname, append = FALSE, delete_dsn = TRUE)
      
    }
  }
  
  if (!all(is.na(tree_objects$zmax2021))){
    fname <- paste0("tree_objects_2021_", tag, ".shp")
    fname_check <- list.files(home_dir, pattern = fname)
    if (length(fname_check) == 0){
      st_write(tree_objects_2021, fname)
    } else{
      st_write(tree_objects_2021, fname, append = FALSE, delete_dsn = TRUE)
      
    }
  }

  
  print("Write csv file for ancillary information")
  fname <- paste0("tree_objects_", tag, ".csv")
  write.csv(tree_objects, fname)

}

print("Completed")


