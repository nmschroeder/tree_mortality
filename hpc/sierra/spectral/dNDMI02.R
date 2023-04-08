#!/usr/bin/env Rscript

library(dplyr)
library(raster)

# HPC3
data_dir <- "/path/to/your/data/directory/"
home_dir <- paste0(data_dir, "sierra")

# Set home directory
setwd(home_dir)

# Let's look at dry season NDMI to avoid confusion from grass

reference_dates <- c(20110915, 20130915, 20170915) %>% as.character()

N <- length(reference_dates)

ref_raster <- raster(paste0("landsat", reference_dates[N], ".tif"), band = 4)

for (i in 1:(N-1)){
  fname_compare <- paste0("landsat", reference_dates[i], ".tif")
  print(fname_compare)
  r <- raster(fname_compare, band = 4)
  dNDMI <- ref_raster - r
  fname <- paste0("landsat_", reference_dates[i], "_", reference_dates[N], ".tif")
  check <- list.files(pattern = fname)
  if (length(check)>0){
    writeRaster(dNDMI, fname, overwrite = TRUE)
  } else{
    writeRaster(dNDMI, fname)
  }
}
