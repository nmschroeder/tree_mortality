#!/usr/bin/env Rscript
fname <- commandArgs(trailingOnly = TRUE)
library(dplyr)
library(sf)
library(raster)
library(stringr)

## Make a raster file for all the waterbodies in the NEON CHM tif extent for each CHM tile

# HPC
data_dir <- "/path/to/your/data/directory/"
home_dir <- paste0(data_dir, "sierra")

setwd(home_dir)

# Extract the tag for later
pat <- "[0-9]{6}_[0-9]{7}"

xy_tag <- str_extract(fname, pattern = pat)

# Convert granite raster to polygons
granite <- raster(fname)
granite[granite != 1] <- NA

test <- granite[!is.na(granite)]
if (length(test)>0){

  # Convert to polygons
  granite_polys <- rasterToPolygons(granite)

  # Apply a 2-meter buffer to dissolve internal holes
  granite_polys_b <- st_buffer(as(granite_polys, 'sf'), dist = 2) %>% as('Spatial')
  granite_polys_ba <- aggregate(granite_polys_b, dissolve = TRUE)
  granite_polys_sf <- disaggregate(granite_polys_ba) %>% as('sf')

  # Remove the buffer 
  shps <- st_buffer(granite_polys_sf, dist = -2)
  poly_area <- st_area(shps) %>% as.numeric()

  # Keep all values that are greater than 2 pixels by 2 pixels (2m x 2m = 4m^2)
  shps <- mutate(shps, poly_area = poly_area)
  shps <- dplyr::filter(shps, poly_area >= 4)
  N <- dim(shps)[1]

  if (N>0){
    shp_fname <- paste0("granite_", xy_tag, ".shp")
  
    check <- list.files(path = ".", pattern = shp_fname)
    if (length(check)>0){
      st_write(shps, shp_fname, append = FALSE)
    } else{
      st_write(shps, shp_fname)
    }
  }
} else{
  print("No granite found")
}
