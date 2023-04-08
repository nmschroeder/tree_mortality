#!/usr/bin/env Rscript
library(dplyr)
library(raster)

data_dir <- "/path/to/your/data/directory/"
srtm_dir <- paste0(data_dir, "SRTM")
data_dir <- paste0(data_dir, "sierra")
setwd(data_dir)

nasa_srtm <- list.files(path = srtm_dir, pattern = "hgt", full.names = TRUE)

raster_merge <- raster(nasa_srtm[1])
for (i in 2:length(nasa_srtm)){
  raster_merge <- raster::merge(raster_merge, raster(nasa_srtm[i]))
}

aspect_srtm <- terrain(raster_merge, opt = "aspect")

aspect_ns <- 0.5*cos(aspect_srtm) + 0.5
writeRaster(aspect_ns, "aspect_ns.tif", overwrite = TRUE)

