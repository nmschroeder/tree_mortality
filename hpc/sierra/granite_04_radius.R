#!/usr/bin/env Rscript

# File to extract granite within 20 meters
library(dplyr)
library(sf)
library(raster)
library(exactextractr)

# HPC
data_dir <- "/path/to/your/data/directory/"
shp_dir <- paste0(data_dir, "sierra")
raster_dir <- paste0(data_dir, "sierra/spectral/merged_rasters")

setwd(data_dir)

shps <- read_sf(paste0(shp_dir,"/tree_locations_las_intersection.shp"))
crds <- st_coordinates(shps)
site <- ifelse(crds[,1]<305000,"SOAP","TEAK")
shps <- mutate(shps, site = site)
shps <- st_buffer(shps, dist = 20)

soap_shps <- dplyr::filter(shps, site=="SOAP")
soap_granite <- raster(paste0(raster_dir, "/granite_SOAP.tif"))
soap_granite[is.na(soap_granite)] <- 0
soap_granite[soap_granite>0] <- 1
soap_ground <- raster(paste0(raster_dir, "/ground_SOAP.tif"))

granite <- exact_extract(soap_granite, soap_shps$geometry, fun = 'mean')
ground <- exact_extract(soap_ground, soap_shps$geometry, fun = 'mean')

soap_shps <- mutate(soap_shps, granite = granite, ground = ground)

# Teakettle

teak_shps <- dplyr::filter(shps, site=="TEAK")
teak_granite <- raster(paste0(raster_dir, "/granite_TEAK.tif"))
teak_granite[is.na(teak_granite)] <- 0
teak_granite[teak_granite>0] <- 1
teak_ground <- raster(paste0(raster_dir, "/ground_TEAK.tif"))

granite <- exact_extract(teak_granite, teak_shps$geometry, fun = 'mean')
ground <- exact_extract(teak_ground, teak_shps$geometry, fun = 'mean')
teak_shps <- mutate(teak_shps, granite = granite, ground = ground)

shps <- rbind.data.frame(soap_shps, teak_shps)
rgranite_csv <- mutate(shps, rgranite = granite/ground) %>% st_drop_geometry() %>% dplyr::select(treeID, granite, ground, rgranite)

# Write csv file for the ratio granite & add to HPC3
str(rgranite_csv)
write.csv(rgranite_csv, "rgranite.csv")


