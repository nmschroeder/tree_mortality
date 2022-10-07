#!/usr/bin/env Rscript
library(dplyr)
library(raster)
library(stringr)
library(sf)
data_dir <- "/dfs4/jranders_lab/users/hemmingn/sierra"
setwd(data_dir)

fnames <- list.files(data_dir, pattern = glob2rx("granite_*.shp"), full.names = TRUE)
fnames

# Read in all the polygon files
shps <- lapply(fnames, read_sf)

# Merge them into one data frame
shps <- do.call(rbind.data.frame, shps)
shps <- dplyr::filter(shps, poly_area>1000)
shps <- st_simplify(shps, dTolerance=10)

# Add a buffer (remove later) to help dissolve small islands in granite slabs
shps_buffer <- st_buffer(shps, dist = 10) %>% as("Spatial")
shps_buffer_agg <- aggregate(shps_buffer, dissolve = TRUE)

# Pull the pieces back apart and remove the buffer
shps_buffer_disagg <- disaggregate(shps_buffer_agg) %>% as('sf')
shps_unbuffer <- st_buffer(shps_buffer_disagg, dist = -10)
rm(shps_buffer, shps_buffer_agg, shps_buff_disagg)
poly_area <- st_area(shps_unbuffer) %>% as.numeric()
shps_complete <- mutate(shps_unbuffer, poly_area = poly_area)

# Save the file
shps_fname <- "granite_polygons_simplify.shp"
st_write(shps_complete, shps_fname, append = FALSE) 

print("Completed")
