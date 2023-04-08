#!/usr/bin/env Rscript
start_end <- commandArgs(trailingOnly = TRUE)

library(dplyr)
library(sf)
library(stringr)
library(raster)
library(exactextractr)
start_end <- str_split(start_end, pattern = ",")[[1]]
tag <- paste(start_end, collapse = "_")
start_end <- as.numeric(start_end)
print(start_end)

# HPC
data_dir <- "/path/to/your/data/directory/"

shp_dir <- paste0(data_dir, "sierra")
raster_dir <- paste0(data_dir, "sierra/spectral/merged_rasters")

d_buffer <- 20

setwd(shp_dir)

print("Read in the point locations of each tree and buffer")
shps <- read_sf(paste0(shp_dir,"/tree_locations_las_intersection.shp"))
shps <- shps[start_end[1]:start_end[2],] %>% arrange(treeID)
treeIDs <- shps$treeID

crds <- st_coordinates(shps)
site <- ifelse(crds[,1]<305000,"SOAP","TEAK")

shp_buffers <- st_buffer(shps$geometry, dist = d_buffer) %>% cbind.data.frame(site = site, treeID = treeIDs)


rm(site)

print("Read in the crown area polygons for each tree in 2013")
shps2013 <- read_sf(paste0(shp_dir, "/tree_objects_2013_las_intersection.shp")) %>% arrange(treeID)
shps2013 <- dplyr::filter(shps2013, treeID %in% treeIDs) %>% arrange(treeID)

print("Compute the difference polygons")

pgon_list <- list()

for (i in 1:length(treeIDs)){
  shp_temp <- dplyr::filter(shp_buffers, treeID == treeIDs[i])
  tree_crown <- dplyr::filter(shps2013, treeID == treeIDs[i])$geometry
  tree_diff <- st_difference(shp_temp$geometry, tree_crown)
  temp_df <- data.frame(treeID=treeIDs[i], site = shp_temp$site, geometry = tree_diff)
  pgon_list[[i]] <- st_as_sf(temp_df, crs = 32611)
}

print("Combine the difference polygons")
shps_combined <- do.call(rbind.data.frame, pgon_list)
str(shps_combined)

i <- 1
shp_list <- list()

print("Work on Soaproot Saddle")

soap_shps <- dplyr::filter(shps_combined, site=="SOAP")

if (dim(soap_shps)[1]>0){
  soap_chm <- raster(paste0(raster_dir, "/chm_SOAP_2013.tif"))
  
  # Keep these light with booleans

  print("Compute boolean rasters")
  soap_canopy <- soap_chm>=5

  soap_valid <-!is.na(soap_chm)
  rm(soap_chm)

  print("Exact extract the canopy")
  canopy <- exact_extract(soap_canopy, soap_shps$geometry, fun = 'sum')

  print("Exact extract the valid spaces")
  chm_area <- exact_extract(soap_valid, soap_shps$geometry, fun = 'sum')

  soap_shps <- mutate(soap_shps, canopy=canopy, chm_area = chm_area)
  shp_list[[i]] <- soap_shps
  i <- i + 1
}
# Teakettle

teak_shps <- dplyr::filter(shps_combined, site=="TEAK")

if (dim(teak_shps)[1]>0){
  teak_chm <- raster(paste0(raster_dir, "/chm_TEAK_2013.tif"))

  teak_canopy <- teak_chm>=5

  teak_valid <- !is.na(teak_chm)                                        
  rm(teak_chm)

  canopy <- exact_extract(teak_canopy, teak_shps$geometry, fun = 'sum')
  chm_area <- exact_extract(teak_valid, teak_shps$geometry, fun = 'sum')

  teak_shps <- mutate(teak_shps, canopy=canopy, chm_area = chm_area)
  shp_list[[i]] <- teak_shps
}


shps <- do.call(rbind.data.frame, shp_list)

canopy_csv <- mutate(shps, cover = canopy/chm_area) %>% st_drop_geometry() %>% dplyr::select(treeID, canopy, chm_area, cover)

# Write csv file for the ratio granite & add to HPC3
str(canopy_csv)
fname <- paste0("canopy_cover_", tag, ".csv")
write.csv(canopy_csv, fname)


