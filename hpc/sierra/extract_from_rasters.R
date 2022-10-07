#!/usr/bin/env Rscript

library(dplyr)
library(sf)
library(raster)

#data_dir <- "/Volumes/LaCie/sierra"
data_dir <- "/dfs4/jranders_lab/users/hemmingn/sierra"
out_dir <- "/dfs5/bio/hemmingn/sierra/align_trees"
setwd(data_dir)

print("Read in tree locations")
shps <- read_sf(paste0(out_dir, "/tree_locations_las_intersection.shp"))

print("Read in precip")
ppt <- raster("prism/prism_ppt.tif")
ppt_anom <- raster("prism/prism_ppt_anom.tif")

print("Read in tmean")
tmean <- raster("prism/prism_tmean.tif")
tmean_anom <- raster("prism/prism_tmean_anom.tif")

print("Read in tmax")
tmax <- raster("prism/prism_tmax.tif")
tmax_anom <- raster("prism/prism_tmax_anom.tif")

print("Read in tmin")
tmin <- raster("prism/prism_tmin.tif")
tmin_anom <- raster("prism/prism_tmin_anom.tif")

print("Read in vpdmax")
vpdmax <- raster("prism/prism_vpdmax.tif")
vpdmax_anom <- raster("prism/prism_vpdmax_anom.tif")

print("Read in vpdmin")
vpdmin <- raster("prism/prism_vpdmin.tif")
vpdmin_anom <- raster("prism/prism_vpdmin_anom.tif")

print("Read in trees per hectare")
trees_per_ha <- raster("/data/homezvol0/hemmingn/sierra/trees_per_hectare.tif")

print("Read in SRTM tifs")
aspect <- raster("srtm/srtm_aspect.tif")
aspect_cats <- raster("srtm/srtm_aspect_categories.tif")
elev <- raster("srtm/srtm_elevation.tif")
slope <- raster("srtm/srtm_slope.tif")

# Extract values from rasters

ppt <- raster::extract(ppt, shps)
ppt_anom <- raster::extract(ppt_anom, shps)

tmean <- raster::extract(tmean, shps)
tmean_anom <- raster::extract(tmean_anom, shps)

tmax <- raster::extract(tmax, shps)
tmax_anom <- raster::extract(tmax_anom, shps)

tmin <- raster::extract(tmin, shps)
tmin_anom <- raster::extract(tmin_anom, shps)

vpdmax <- raster::extract(vpdmax, shps)
vpdmax_anom <- raster::extract(vpdmax_anom, shps)

vpdmin <- raster::extract(vpdmin, shps)
vpdmin_anom <- raster::extract(vpdmin_anom, shps)

trees_per_ha <- raster::extract(trees_per_ha, shps)

aspect <- raster::extract(aspect, shps)
aspect_cats <- raster::extract(aspect_cats, shps)
slope <- raster::extract(slope, shps)
elev <- raster::extract(elev, shps)

env_data <- data.frame(treeID = shps$treeID, ppt = ppt, tmean = tmean, tmax = tmax, tmin = tmin,
                          vpdmax = vpdmax, vpdmin = vpdmin, tpa = trees_per_ha, 
                          aspect = aspect, asp_cats = aspect_cats, slope = slope, elev = elev)
setwd(out_dir)
write.csv(env_data, "env_data_las_intersection.csv")
