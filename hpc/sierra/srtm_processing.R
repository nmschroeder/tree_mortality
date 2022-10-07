#!/usr/bin/env Rscript
library(dplyr)
library(raster)

setwd("/dfs5/bio/hemmingn/sierra/align_trees/")

nasa_srtm <- list.files(path = "/dfs4/jranders_lab/users/hemmingn/SRTM", pattern = "hgt", full.names = TRUE)
nasa_srtm

ls_dir <- "/dfs4/jranders_lab/users/hemmingn/Landsat/"

raster_merge <- raster(nasa_srtm[1])
for (i in 2:length(nasa_srtm)){
  raster_merge <- raster::merge(raster_merge, raster(nasa_srtm[i]))
}

aspect_srtm <- terrain(raster_merge, opt = "aspect")
slope_srtm <- terrain(raster_merge, opt = "slope")
slope_srtm <- slope_srtm*180/pi

## Next, let's read in our Landsat file
ls8_files <- list.files(ls_dir, pattern = glob2rx("LC08*_042034_*B4.TIF"), 
                        recursive = TRUE,
                        full.names = TRUE)

ls8 <- raster(ls8_files[1])


# Project aspect and slope to the landsat8 CRS and resolution
aspect_aligned <- projectRaster(aspect_srtm, ls8)
slope_aligned <- projectRaster(slope_srtm, ls8)
elev_aligned <- projectRaster(raster_merge, ls8)

aspect_ns <- 0.5*cos(aspect_srtm) + 0.5
writeRaster(aspect_ns, "aspect_ns.tif", overwrite = TRUE)
#writeRaster(slope_aligned, "slope.tif", overwrite = TRUE)
#writeRaster(aspect_aligned, "aspect.tif", overwrite = TRUE)
#writeRaster(elev_aligned, "elev.tif", overwrite = TRUE)

aspect_ns_2 <- raster::focal(aspect_ns, w = matrix(1,3,3), fun = median)
writeRaster(aspect_ns_2, "aspect_ns_v2.tif")


