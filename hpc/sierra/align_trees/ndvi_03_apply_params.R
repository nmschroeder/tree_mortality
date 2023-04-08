#!/usr/bin/env Rscript
param_inputs <- commandArgs(trailingOnly = TRUE)
print(param_inputs)

## Apply percent mortality thresholds for best fit

library(raster)
library(sf)
library(dplyr)
library(exactextractr)
library(tidyr)
library(ggplot2)
library(pracma)
library(stringr)

t1 <- param_inputs[1] %>% as.numeric()
t2  <- param_inputs[2] %>% as.numeric()
year <- param_inputs[3]

print("Rgreen threshold")
t1

print("Ratio threshold")
t2

print("Year")
year

# HPC3
data_dir <- "/path/to/your/data/directory/"
sierra_dir <- paste0(data_dir, "sierra")
spectral_dir <-paste0(sierra_dir, "/spectral/merged_rasters") 
mask_dir <- paste0(sierra_dir, "/masks")

setwd(sierra_dir)

# Read in year (2013, 2017, 2018, 2019, or 2021)
shps <- read_sf(paste0("tree_objects_",year,"_las_intersection.shp"))
print("Dimensions of shp data.frame")
dim(shps)

# Read in trees dead by area
dead_by_area <- read.csv("trees_dead_by_area.csv")

# Read in the locations (we need these to ID the site, SOAP or TEAK)
shp_locs <- read_sf("tree_locations_las_intersection.shp")
str(shp_locs)

# Determine which trees from 2013 and year (may be 2013, 2017, 2018, 2019, or 2021)
# are in the intersection
treeID_int <- shp_locs$treeID

dead_by_area <- dead_by_area %>% dplyr::filter(treeID %in% treeID_int)
shps <- shps %>% dplyr::filter(treeID %in% treeID_int)

# Obtain the x and y values of the tree locations
xy <- st_coordinates(shp_locs)
x <- xy[,1]
y <- xy[,2]
rm(xy)

# Drop the sf geometry
shp_locs <- shp_locs %>% st_drop_geometry()

# Append the x and y values to the tree locations
shp_locs <- cbind.data.frame(treeID = shp_locs$treeID, x = x, y = y)

# Join the locations to the intersection shapefile by the treeID
shps <- right_join(shps, shp_locs, by = "treeID")

# Add dead by area for this year
if (year != "2013"){
  mort_yr <- paste0("mort", year)
  dead_by_area <- dplyr::select(dead_by_area, treeID = treeID, mort_area = matches(mort_yr))
  str(dead_by_area)
  shps <- right_join(shps, dead_by_area, by = "treeID")
  str(shps)
}

# Determine which site the trees are from based on the x-coordinate
print("Classify by site")
sites <- ifelse(shps$x<308000, "SOAP", "TEAK")
shps <- mutate(shps, sites = sites) %>% st_as_sf(crs = 32611)

# Partition trees as dead by area or not
if (year != "2013"){
  print("Partition of trees labeled dead by area")
  shps_labeled_by_area <- dplyr::filter(shps, mort_area == 1)
  N <- dim(shps_labeled_by_area)[1]

  print("Number labeled as fallen trees or standing dead boles using area:")
  N

  # This data frame is not labeled by area and needs to go through the spectral index
  print("Partition of trees that still need to be labeled")
  shps_ndvi <- dplyr::filter(shps, mort_area != 1)
} else {
  shps_ndvi <- shps
}

# Keep the remaining trees for further analysis
print("Number of remaining trees:")
dim(shps_ndvi)[1]

print("Separate by site")
shps_teak <- dplyr::filter(shps_ndvi, sites == "TEAK")
shps_soap <- dplyr::filter(shps_ndvi, sites == "SOAP")

print("Read in rasters")

soap_mask <- raster(paste0(mask_dir, "/SOAP_mask.tif"))
teak_mask <- raster(paste0(mask_dir, "/TEAK_mask.tif"))

ndvi_soap <- raster(paste0(spectral_dir, "/ndvi_SOAP_", year, ".tif"))
ndvi_teak <- raster(paste0(spectral_dir, "/ndvi_TEAK_", year, ".tif"))

ndvi_soap <- ndvi_soap*soap_mask
ndvi_teak <- ndvi_teak*teak_mask

proj_string <- "+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

print("Assign CRS")
crs(ndvi_soap) <- proj_string
crs(ndvi_teak) <- proj_string

print("Check raster structure")
ndvi_soap
ndvi_teak

print("Find the pixels where NDVI is less than threshold 1 for teakettle")
ndvi_teak_bool <- ndvi_teak<t1

print("Check shapefile structure")
str(shps_teak)
str(shps_soap)

print("Apply exact extract")
r_ndvi <- exact_extract(ndvi_teak_bool, shps_teak$geometry, fun = "mean")
mean_ndvi <- exact_extract(ndvi_teak, shps_teak$geometry, fun = "mean")
print("Add the ratio NDVI above threshold")
shps_teak <- cbind.data.frame(shps_teak, r_ndvi = r_ndvi, mean_ndvi = mean_ndvi)
 
print("Next find the pixels for soaproot saddle")
ndvi_soap_bool <- ndvi_soap<t1
r_ndvi <- exact_extract(ndvi_soap_bool, shps_soap$geometry, fun = "mean")
mean_ndvi <- exact_extract(ndvi_soap, shps_soap$geometry, fun = "mean")
shps_soap <- cbind.data.frame(shps_soap, r_ndvi = r_ndvi, mean_ndvi = mean_ndvi)

shps_ndvi <- rbind.data.frame(shps_teak, shps_soap)

live <- ifelse(shps_ndvi$r_ndvi>=t2, 0, 1)

shps_ndvi <- mutate(shps_ndvi, live = live)

if (year != "2013"){
  r_ndvi <- rep(NA, times = N)
  live <- rep(0, times = N)
  mean_ndvi <- rep(NA, times = N)
  shps_labeled_by_area <- mutate(shps_labeled_by_area, r_ndvi = r_ndvi, live = live, mean_ndvi = mean_ndvi)
  shps_labeled <- rbind.data.frame(shps_ndvi, shps_labeled_by_area)
} else{
  shps_labeled <- shps_ndvi
}
shps_labeled <- arrange(shps_labeled, by = treeID)
idx <- which(shps_labeled$area2013<1)
shps_labeled$live[idx] <- NA
fname <- paste0("trees_", year, "_ndvi.shp")

print("Compare")

dim(shps)

dim(shps_labeled)

st_write(shps_labeled, fname, append=FALSE, delete_dsn = TRUE)


