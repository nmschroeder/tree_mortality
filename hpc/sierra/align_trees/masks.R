#!/usr/bin/env Rscript

# Alternative method to create a data mask; use the extents of the canopy height models to generate
# raster data of valid values and then merge them.

library(raster)
data_dir <- "/path/to/your/data/directory/"
out_dir <- paste0(data_dir, "sierra/masks")
data_dir <- paste0(data_dir, "sierra/spectral/merged_rasters")

sites <- c("SOAP", "TEAK")

for (i in 1:length(sites)){
  site <- sites[i]
  
  fnames <- list.files(path = data_dir, pattern = glob2rx(paste0("chm_", site, "*.tif")), full.names = TRUE)
  r_list <- lapply(fnames, raster)
  ext_list <- lapply(r_list, extent)
  ext <- do.call(merge, ext_list)
  r_ext <- lapply(r_list, FUN = extend, y = ext, value = NA)
  s <- stack(r_ext)
  m <- calc(s,fun = min, na.rm = FALSE)
  m <- !is.na(m)
  fname <- paste0(out_dir, "/", site, "_mask.tif")
  writeRaster(m, fname, overwrite = TRUE)
}
