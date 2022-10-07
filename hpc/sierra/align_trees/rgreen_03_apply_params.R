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
sierra_dir <- "/dfs5/bio/hemmingn/sierra/align_trees"
spectral_dir <- "/dfs5/bio/hemmingn/sierra/spectral/merged_rasters"
wd <- "/dfs5/bio/hemmingn/sierra/align_trees"

setwd(wd)

# Read in year (2013, 2017, 2018, 2019, or 2021)
shps <- read_sf(paste0(sierra_dir,"/tree_objects_",year,"_las_intersection.shp"))
print("Dimensions of shp data.frame")
dim(shps)
# Read in control year (2013)
shps2013 <- read_sf(paste0(sierra_dir, "/tree_objects_2013_las_intersection.shp"))

print("Compute the area for the 2013 polygons")
area2013 <- st_area(shps2013$geometry) %>% as.numeric()

print("Select the treeIDs for 2013")
treeID <- shps2013$treeID

# Create a data frame matching the treeIDs up with their 2013 crown areas
shps2013 <- data.frame(area2013 = area2013, treeID = treeID)

# Read in the locations (we need these to ID the site, SOAP or TEAK)
shp_locs <- read_sf(paste0(sierra_dir, "/tree_locations_las_intersection.shp"))
str(shp_locs)

# Obtain the x and y values of the tree locations
xy <- st_coordinates(shp_locs)
x <- xy[,1]
y <- xy[,2]
rm(xy)

# Append the x and y values to the tree locations
shp_locs <- cbind.data.frame(treeID = shp_locs$treeID, x = x, y = y)
shp_locs <- dplyr::filter(shp_locs, treeID %in% shps2013$treeID)

# Join the locations to the intersection shapefile by the treeID
shps <- right_join(shps, shp_locs, by = "treeID")

# Also add the 2013 crown area
shps <- right_join(shps, shps2013, by = "treeID")

# Determine which site the trees are from based on the x-coordinate
sites <- ifelse(shps$x<308000, "SOAP", "TEAK")
shps <- mutate(shps, sites = sites) %>% st_as_sf(crs = 32611)

# Compute the area (removing units) and add to data frame
poly_area <- st_area(shps$geometry) %>% as.numeric()
shps <- mutate(shps, poly_area = poly_area)

# Select only the trees with a crown area greater than 1 sq meter


idx1 <- which(shps$poly_area<1)
shps_labeled_by_area <- shps[idx1,]
idx2 <- which(shps$poly_area>=1)
shps_idx <- shps[idx2,]

N <- dim(shps_labeled_by_area)[1]

print("Number labeled as fallen trees or standing dead boles using area:")
N

# Keep the remaining trees for further analysis
print("Number of remaining trees:")
dim(shps_idx)[1]

print("Separate by site")
shps_teak <- dplyr::filter(shps_idx, sites == "TEAK")
shps_soap <- dplyr::filter(shps_idx, sites == "SOAP")

print("Read in rasters")

soap_mask <- raster(paste0(spectral_dir, "/tree_mask_SOAP_", year, ".tif"))
teak_mask <- raster(paste0(spectral_dir, "/tree_mask_TEAK_", year, ".tif"))

idx_soap <- raster(paste0(spectral_dir, "/rgreen_SOAP_", year, ".tif"))
idx_teak <- raster(paste0(spectral_dir, "/rgreen_TEAK_", year, ".tif"))

idx_soap <- idx_soap*soap_mask
idx_teak <- idx_teak*teak_mask

proj_string <- "+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

print("Assign CRS")
crs(idx_soap) <- proj_string
crs(idx_teak) <- proj_string

print("Check raster structure")
idx_soap
idx_teak

print("Find the pixels where index is less than threshold 1 for teakettle")
idx_teak_bool <- idx_teak<t1

print("Check shapefile structure")
str(shps_teak)
str(shps_soap)

print("Apply exact extract")
r_green <- exact_extract(idx_teak_bool, shps_teak$geometry, fun = "mean")
mean_green <- exact_extract(idx_teak, shps_teak$geometry, fun = "mean")
print("Add the ratio veg index above threshold")
shps_teak <- cbind.data.frame(shps_teak, r_green = r_green, mean_green = mean_green)
 
print("Next find the pixels for soaproot saddle")
idx_soap_bool <- idx_soap<t1
r_green <- exact_extract(idx_soap_bool, shps_soap$geometry, fun = "mean")
mean_green <- exact_extract(idx_soap, shps_soap$geometry, fun = "mean")
shps_soap <- cbind.data.frame(shps_soap, r_green = r_green, mean_green = mean_green)

shps_green <- rbind.data.frame(shps_teak, shps_soap)

live <- ifelse(shps_green$r_green>=t2, 0, 1)

shps_green <- mutate(shps_green, live = live)

r_green <- rep(NA, times = N)
live <- rep(0, times = N)
mean_green <- rep(NA, times = N)
shps_labeled_by_area <- mutate(shps_labeled_by_area, r_green = r_green, live = live, mean_green = mean_green)
shps_labeled <- rbind.data.frame(shps_green, shps_labeled_by_area)
shps_labeled <- arrange(shps_labeled, by = treeID)
idx <- which(shps_labeled$area2013<1)
shps_labeled$live[idx] <- NA
fname <- paste0("trees_", year, "_rgreen.shp")

print("Compare")

dim(shps)

dim(shps_labeled)

st_write(shps_labeled, fname, append=FALSE, delete_dsn = TRUE)


