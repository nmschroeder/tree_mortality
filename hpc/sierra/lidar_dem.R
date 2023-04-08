#!/usr/bin/env Rscript
library(dplyr)
library(raster)

data_dir <- "/path/to/your/data/directory/"
lidar_dir <- paste0(data_dir, "NEON/elev/2013")
data_dir <- paste0(data_dir, "sierra/")
setwd(data_dir)

fnames <- list.files(path = lidar_dir, pattern = "DTM.tif", recursive = TRUE, full.names = TRUE)

fnames

raster_merge <- raster(fnames[1])
for (i in 2:length(fnames)){
  raster_merge <- raster::merge(raster_merge, raster(fnames[i]))
}

aspect_neon <- terrain(raster_merge, opt = "aspect")
slope_neon <- terrain(raster_merge, opt = "slope")
slope_neon <- slope_neon*180/pi

aspect_ns <- 0.5*cos(aspect_neon) + 0.5
writeRaster(aspect_ns, "neon_aspect_ns.tif", overwrite = TRUE)
writeRaster(slope_neon, "neon_slope.tif", overwrite = TRUE)
writeRaster(aspect_neon, "neon_aspect.tif", overwrite = TRUE)
writeRaster(raster_merge, "neon_elev.tif", overwrite = TRUE)



