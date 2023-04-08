#!/usr/bin/env Rscript
siteyear <- commandArgs(trailingOnly = TRUE)
siteyear

library(dplyr)
library(raster)
library(stringr)

data_dir <- "/path/to/your/data/directory/"
wd <- paste0(data_dir, "sierra/spectral/merged_rasters")
setwd(wd)

siteyear <- str_split(siteyear, ",")[[1]]
site <- siteyear[1]
year <- siteyear[2]

print("Site and year check:")
print("Site:")
print(site)
print("Year:")
print(year)

find_extent <- function(rlist){
  exts <- lapply(rlist, extent)
  xmn <- exts[[1]]@xmin
  xmx <- exts[[1]]@xmax
  ymn <- exts[[1]]@ymin
  ymx <- exts[[1]]@ymax
  for (i in 2:length(rlist)){
    xmn <- max(xmn, exts[[i]]@xmin)
    ymn <- max(ymn, exts[[i]]@ymin)
    xmx <- min(xmx, exts[[i]]@xmax)
    ymx <- min(ymx, exts[[i]]@ymax)
  }
  ext <- extent(c(xmn, xmx, ymn, ymx))
  return(ext)
}

print("reading in rasters")
chm <- raster(paste0("chm_", site, "_", year, ".tif"))
chm
lum <- raster(paste0("luminosity_", site, "_",year, ".tif"))
lum
#granite <- raster(paste0("granite_", site, ".tif"))
#granite

print("Setting CRS for spectral-derived rasters")
crs(lum) <- crs(chm)

print("Finding the extent of intersection")
ext <- find_extent(list(chm, lum))

print("Cropping to the same extent")
chm <- crop(chm, ext)
lum <- crop(lum, ext)

print("Storing criteria")
tree_mask <- (lum < 0.2) & (chm > 4)

print("Set FALSE values to NA")
tree_mask[tree_mask==0] <- NA

print("Create tree mask file name and save")
fname <- paste0("tree_mask_",site,"_",year,".tif")
check <- list.files(wd, pattern=fname)
if (length(check)>0){
  writeRaster(tree_mask, fname, overwrite=TRUE)
}else{
  writeRaster(tree_mask, fname)
}


print("Completed")
