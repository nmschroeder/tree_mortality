library(dplyr)
library(sf)
library(raster)

# Here, you can use the script in hpc/sierra/trees_per_hectare.R to generate the
# stovall_trees_per_pixel.tif raster using the data set Stovall et al. (2019) stored on figshare.
# We suggest creating a directory called "data" and putting in the same folder as the
# tree_mortality directory to store stovall_trees_per_pixel.tif and the outputs of this 
# script.

trees_per_pixel_stovall <- raster("../data/stovall_trees_per_pixel.tif")
trees_per_pixel <- raster("data/deliverables/raster/trees_per_pixel.tif")

trees_per_pixel[trees_per_pixel==0] <- NA
trees_per_pixel_stovall[trees_per_pixel_stovall==0] <- NA

trees_per_pixel_difference <- trees_per_pixel - trees_per_pixel_stovall
writeRaster(trees_per_pixel, "data/deliverables/raster/trees_per_pixel_mask.tif")
writeRaster(trees_per_pixel_stovall, "../data/stovall_trees_per_pixel_mask.tif")
writeRaster(trees_per_pixel_difference, "../data/trees_per_pixel_difference.tif")

# Let's try an easier way to see the differences
tppd_positive <- trees_per_pixel_difference
tppd_positive[tppd_positive<=0] <- NA

tppd_negative <- trees_per_pixel_difference
tppd_negative[tppd_negative>=0] <- NA
tppd_negative <- tppd_negative*-1

writeRaster(tppd_positive, "../data/tppd_positive.tif")
writeRaster(tppd_negative, "../data/tppd_negative.tif")
