#!/usr/bin/env Rscript

library(tidyverse)
library(dplyr)
library(sf)
library(ggplot2)
library(raster)
library(lidR)
library(stringr)

# How many Landsat images do we want to collect (in case of clouds, etc.)
n_landsat <- 4

# HPC3
data_dir <- "/path/to/your/data/directory/"
ls_dir <- paste0(data_dir, "Landsat")
home_dir <- paste0(data_dir, "sierra")

# Set home directory
setwd(home_dir)

# Buffer around NEON shapefiles
bff <- 1000 # meters

buffer_extent <- function(ext, d){
  ext@xmin <- ext@xmin-d
  ext@xmax <- ext@xmax+d
  ext@ymin <- ext@ymin-d
  ext@ymax <- ext@ymax+d
  return(ext)
}

# Select a Landsat path and row of interest
pathrow <- "042034"

# Crop the Landsat images to our region of interest (with a buffer)
roi_extent <- st_read("tree_locations_las_intersection.shp") %>% extent() %>% buffer_extent(d=bff)

# Helpful Landsat functions

# Function to extract the dates YYYYMMDD
ls_date_extract <- function(fname){
  yr <- str_extract(fname, pattern = paste0("_", pathrow,"_[0-9]{8}"))
  return(as.numeric(gsub(paste0("_", pathrow,"_"), "", yr)))
}

ls_sat_id_extract <- function(fname){
  ls_sat_id <- str_extract(fname, pattern = paste0("L[A-Z]0[5-8]"))
  return(ls_sat_id)
}

ls_id_extract <- function(fname){
  ls_id <- str_extract(fname, pattern = 
                         paste0("L[A-Z]0[5|8]_[A-Z][0-9][A-Z]{2}_", 
                                pathrow,"_[0-9]{8}_[0-9]{8}_[0-9]{2}_[A-Z][0-9]"))
  return(ls_id)
}

# This cloud mask follows documentation on Google Earth Engine
cloudBitMask <- function(qa_pixel){
  binbits <- intToBits(qa_pixel) %>% as.integer()
  # Cloud is bit 3 and therefore element 4
  # Medium and High confidence for cloud correspond to bit 8 and therefore element 9 
  # (Bits 8-9 are assigned cloud confidence where 0 is none (binary 00),
  # (1 is low (01), 2 is medium (binary for 2 is 10), and 3 is high (binary 11)))
  if (binbits[4] == 1 & binbits[9] == 1){
    # Then we have medium to high confidence of clouds
    clouds <- NA
  } else if(binbits[5] == 1 & binbits[11] == 1){
    # So if bit 4 (i.e. element 5) and confidence is medium or high (bit 10 aka element 11 == 1)
    clouds <- NA 
    # Check for high confidence of cirrus clouds
  } else if(binbits[4] == 1 & binbits[15] == 1){
    clouds <- NA
  } else{
    clouds <- 1 # ignoring cirrus for now
  }
  return(clouds)
}

# Let's look at dry season NDMI to avoid confusion from grass

reference_dates <- c(20110915, 20130915, 20170915)
for (ref_date in reference_dates){
  
  # List all the Landsat files we have downloaded
  ls_fnames <- list.files(path = ls_dir, pattern = ".TIF", recursive = TRUE)
  ls_dates <- unname(sapply(ls_fnames, ls_date_extract)) %>% unique()
  d_diff <- abs(ls_dates - ref_date)
  
  # Find the indices of the Landsat dates of interest for the NAIP image date
  idx <- order(d_diff, na.last = TRUE, decreasing = FALSE)[1:n_landsat]
  ls_nearest <- ls_dates[idx]
  print(ls_nearest)
  
  # Make this into an or-statement to search for both of these dates
  ls_nearest_or <- paste0(ls_nearest, collapse = "|")
  
  # Pull the Landsat images
  ls_selected <- list.files(path = ls_dir, pattern = ls_nearest_or, recursive = TRUE, full.names = TRUE)
  print(ls_selected)
  
  # Pull the unique landsat IDs for the reference Landsat images
  ls_id <- unname(sapply(ls_selected, ls_id_extract)) %>% unique()
  ls_sat_id <- unname(sapply(ls_id, ls_sat_id_extract))
  ls_date <- unname(sapply(ls_id, ls_date_extract))
  
  # Organize this information into a dataframe
  ls_df <- data.frame(ls_id = ls_id, ls_sat_id = ls_sat_id, ls_date = ls_date, ls_blue = NA,
                      ls_green = NA, ls_red = NA, ls_nir = NA, ls_swir1 = NA, ls_qa = NA)
  
  for (i in 1:dim(ls_df)[1]){
    # If the file is from Landsat 5
    if (ls_df$ls_sat_id[i] %in% c("LT05")){
      ls_df$ls_blue[i] <- paste0(ls_df$ls_id[i], "_[A-Z]{2}_B1.TIF")
      ls_df$ls_green[i] <- paste0(ls_df$ls_id[i], "_[A-Z]{2}_B2.TIF")
      ls_df$ls_red[i] <- paste0(ls_df$ls_id[i], "_[A-Z]{2}_B3.TIF")
      ls_df$ls_nir[i] <- paste0(ls_df$ls_id[i], "_[A-Z]{2}_B4.TIF")
      ls_df$ls_swir1[i] <- paste0(ls_df$ls_id[i], "_[A-Z]{2}_B5.TIF")
      
      # If the file is from Landsat 8 (skip 7 due to striping)
    } else if(ls_df$ls_sat_id[i] %in% c("LC08", "LT08")){
      ls_df$ls_blue[i] <- paste0(ls_df$ls_id[i], "_[A-Z]{2}_B2.TIF")
      ls_df$ls_green[i] <- paste0(ls_df$ls_id[i], "_[A-Z]{2}_B3.TIF")
      ls_df$ls_red[i] <- paste0(ls_df$ls_id[i], "_[A-Z]{2}_B4.TIF")
      ls_df$ls_nir[i] <- paste0(ls_df$ls_id[i], "_[A-Z]{2}_B5.TIF")
      ls_df$ls_swir1[i] <- paste0(ls_df$ls_id[i], "_[A-Z]{2}_B6.TIF")
      
    } else{
      print(ls_df$ls_sat_id[i])
      errorCondition("Satellite ID not understood")
    }
    ls_df$ls_qa[i] <- paste0(ls_df$ls_id[i], "_QA_PIXEL.TIF")
  }
  # Check data frame
  print("check landsat data frame, ls_df")
  str(ls_df)
 
  # Stack by band: blue, green, red, and near infrared 
  # (Remove bad pixels using QA pixel tif before stacking)

  # Crop the Landsat images to the extent of the NAIP image
  sr_blue <- list()
  sr_green <- list()
  sr_red <- list()
  sr_ndmi <- list()
  sr_ndvi <- list()
  cloud_mask <- list()
  
  for (i in 1:n_landsat){
  
    # Find the file and crop it
    qa_pixel <- list.files(path = ls_dir, pattern = ls_df$ls_qa[i], recursive = TRUE, full.names = TRUE)[1]
    print("read in QA pixel")
    qa_pixel <- raster(qa_pixel)
    qa_pixel <- crop(qa_pixel, roi_extent)
    
    # Generate a mask
    cloud_mask[[i]] <- raster::calc(qa_pixel, cloudBitMask)

    blue <- list.files(path = ls_dir, pattern = ls_df$ls_blue[i], recursive = TRUE, full.names = TRUE)[1]
    print("read in blue")
    blue <- raster(blue)
    blue <- crop(blue, roi_extent)
    blue <- mask(blue, cloud_mask[[i]])
    
    print("read in green")
    green <- list.files(path = ls_dir, pattern = ls_df$ls_green[i], recursive = TRUE, full.names = TRUE)[1]
    green <- raster(green)
    green <- crop(green, roi_extent)
    green <- mask(green, cloud_mask[[i]])
    
    print("read in red")
    red <- list.files(path = ls_dir, pattern = ls_df$ls_red[i], recursive = TRUE, full.names = TRUE)[1]
    red <- raster(red)
    red <- crop(red, roi_extent)
    red <- mask(red, cloud_mask[[i]])
    
    print("read in near infrared")
    nir <- list.files(path = ls_dir, pattern = ls_df$ls_nir[i], recursive = TRUE, full.names = TRUE)[1]
    nir <- raster(nir)
    nir <- crop(nir, roi_extent)
    nir <- mask(nir, cloud_mask[[i]])
    
    print("read in shortwave infrared 1")
    swir1 <- list.files(path = ls_dir, pattern = ls_df$ls_swir1[i], recursive = TRUE, full.names = TRUE)[1]
    swir1 <- raster(swir1)
    swir1 <- crop(swir1, roi_extent)
    swir1 <- mask(swir1, cloud_mask[[i]])
    
    print("Store blue, green, red, NDMI, and NDVI")
    sr_blue[[i]] <- blue
    sr_green[[i]] <- green
    sr_red[[i]] <- red
    sr_ndmi[[i]] <- (nir-swir1)/(swir1+nir)
    sr_ndvi[[i]] <- (nir-red)/(nir+red)
  
  }
  
  # Take the mean for each band
  sr_blue <- do.call(stack, sr_blue)
  sr_blue <- calc(sr_blue, fun = median, na.rm = TRUE)
  
  sr_green <- do.call(stack, sr_green)
  sr_green <- calc(sr_green, fun = median, na.rm = TRUE)
  
  sr_red <- do.call(stack, sr_red)
  sr_red <- calc(sr_red, fun = median, na.rm = TRUE)
  
  sr_ndmi <- do.call(stack, sr_ndmi)
  sr_ndmi <- calc(sr_ndmi, fun = median, na.rm = TRUE)
  
  sr_ndvi <- do.call(stack, sr_ndvi)
  sr_ndvi <- calc(sr_ndvi, fun = median, na.rm = TRUE)
  
  # Stack the 30m x 30m resolution items 
  sr_stack <- stack(sr_blue, sr_green, sr_red, sr_ndmi, sr_ndvi)
  names(sr_stack) <- c("LS_B", "LS_G", "LS_R", "LS_NDMI", "LS_NDVI")
  
  # Generate the Landsat bands and aggregated NAIP bands for the NAIP input
  tag <- "landsat"
  new_fname <- paste0(tag, as.character(ref_date), ".tif")
  
  check <- list.files(pattern = new_fname)
  if (length(check)>0){
    writeRaster(sr_stack, new_fname, overwrite = TRUE)
  } else{
    writeRaster(sr_stack, new_fname)
  }
}
