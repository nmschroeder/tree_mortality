# This script converts tree mortality point data from our study to 30-meter mortality
# fraction raster data for all years of data (2013, 2017, 2018, 2019, and 2021). 
# We also convert data from Stovall et al. (2019) to raster data using the same method.

# Note that we save raster data derived from the Stovall data set to a data 
# directory called data located in the relative path ../data where our current
# working directory is the tree_mortality directory uploaded to Github.

# This keeps these data directories outside of the Github directory to avoid 
# sharing the outside data set. We suggest creating a directory ../data for
# storing these output data sets to match the script below.

library(ggplot2)
library(dplyr)
library(raster)
library(sp)
library(sf)

# Download and change to location of Stovall et al. (2019) dataset
fname_figshare <- "~/Documents/R/tree_mortality_stovall/figshare/ALLtrees_v2.csv"

# Read in the data masks
soap_mask <- raster("data/deliverables/raster/masks/soap_mask.tif")
teak_mask <- raster("data/deliverables/raster/masks/teak_mask.tif")
soap_mask[soap_mask==0] <- NA
plot(soap_mask)

ext <- extent(285000, 305000, 4096000, 4106000)
soap_mask <- crop(soap_mask, ext)
plot(soap_mask)

# Count how many values have been cut and compare to your raster files
soap_mask_df <- rasterToPoints(soap_mask) %>% as.data.frame()
str(soap_mask_df)
test <- sum(!is.na(soap_mask_df$soap_mask))
test2 <- sum(soap_mask_df$soap_mask==1)
percent_lost <- (1-(test2/test))*100
percent_lost

soap_mask_30_strict <- soap_mask
soap_mask_30_strict[soap_mask!=1] <- NA

writeRaster(soap_mask_30_strict, "data/intermediate/study_region/soap_mask_30_strict.tif", overwrite = TRUE)

## Lower Teakettle
# Read in mask
teak_mask[teak_mask==0] <- NA
plot(teak_mask)

# Count how many values have been cut and compare to your raster files
teak_mask_df <- rasterToPoints(teak_mask) %>% as.data.frame()
str(teak_mask_df)
test <- sum(!is.na(teak_mask_df$teak_mask))
test2 <- sum(teak_mask_df$teak_mask==1)
percent_lost <- (1-(test2/test))*100
percent_lost

teak_mask_30_strict <- teak_mask
teak_mask_30_strict[teak_mask!=1] <- NA

writeRaster(teak_mask_30_strict, "data/intermediate/study_region/teak_mask_30_strict.tif", overwrite = TRUE)

## All of the above is okay 2022-04-14 NHS

shps_2017 <- read_sf("data/deliverables/vector/trees_2017_rgreen.shp") %>% st_as_sf(crs = 32611)
idx <- which(!is.na(shps_2017$live))
shps_2017 <- shps_2017[idx,]

treeIDs <- shps_2017$treeID

fname <- list.files("../data", pattern = glob2rx("landsat*.tif"), recursive = TRUE, full.names = TRUE)[1]
r <- raster(fname)

shps_xy <- shps_2017 %>% st_drop_geometry()

# Divide into Soaproot saddle and teakettle

idx1 <- which(shps_xy$xlas2017 <= 310000 & shps_xy$xlas2017 > 285000)
idx2 <- which(shps_xy$xlas2017 > 310000)

r_neon <- crop(r, extent(shps_2017[idx1,]))

shps_2017_xy <- shps_2017 %>% 
  st_drop_geometry() %>% 
  as.data.frame() %>% 
  st_as_sf(coords = c("xlas2017", "ylas2017"), crs=32611)

raster_soap <- rasterize(shps_2017_xy[idx1,], r_neon, 1-shps_2017_xy[idx1,]$live, fun = mean)
raster_soap <- raster_soap*soap_mask_30_strict
raster_soap_df <- rasterToPoints(raster_soap) %>% as.data.frame()
str(raster_soap_df)
test <- sum(!is.na(raster_soap_df$layer))

ggplot() + geom_raster(data = raster_soap_df, mapping = aes(x=x, y=y, fill=layer))
writeRaster(raster_soap, "data/deliverables/raster/soap_mortality_2017.tif", overwrite = TRUE)

# Calculate for TEAK
r_neon <- crop(r, extent(shps_2017[idx2,]))
raster_teak <- rasterize(shps_2017_xy[idx2,], r_neon, 1-shps_2017_xy[idx2,]$live, fun = mean)
raster_teak <- raster_teak*teak_mask_30_strict
raster_teak_df <- rasterToPoints(raster_teak, spatial = TRUE) %>% data.frame()

ggplot() + geom_raster(data = raster_teak_df, mapping = aes(x = x, y = y, fill = layer))

writeRaster(raster_teak, "data/deliverables/raster/teak_mortality_2017.tif", overwrite = TRUE)


# Do the same thing for the original dataset and then subtract them
shps <- read.csv(fname_figshare)
idx1 <- which(shps$x<=310000 & shps$x > 285000)
idx2 <- which(shps$x>310000)
shps <- dplyr::select(shps, x, y, dead)
coordinates(shps) <- ~x+y
prj4string <- proj4string(raster_soap)
proj4string(shps) <- prj4string
r_stovall <- crop(r, extent(shps))
raster_stovall_soap <- rasterize(shps[idx1,], r_stovall, shps[idx1,]$dead, fun = mean)
soap_mask_30_strict <- raster("data/intermediate/study_region/soap_mask_30_strict.tif")
raster_stovall_soap <- raster_stovall_soap*soap_mask_30_strict
plot(raster_stovall_soap)
writeRaster(raster_stovall_soap, "../data/stovall_soap_mortality.tif", overwrite = TRUE)

raster_stovall_teak <- rasterize(shps[idx2,], r_stovall, shps[idx2,]$dead, fun = mean)
teak_mask_30_strict <- raster("data/intermediate/study_region/teak_mask_30_strict.tif")
raster_stovall_teak <- raster_stovall_teak*teak_mask_30_strict
writeRaster(raster_stovall_teak, "../data/stovall_teak_mortality.tif", overwrite = TRUE)

raster_difference_soap <- raster_soap - raster_stovall_soap 
writeRaster(raster_difference_soap, "../data/soap_mortality_difference.tif", overwrite = TRUE)
raster_difference_teak <- raster_teak - raster_stovall_teak 
writeRaster(raster_difference_teak, "../data/teak_mortality_difference.tif", overwrite = TRUE)
plot(raster_difference_soap)

## Create rasters for additional years

## 2013

shps_2013 <- read_sf("data/deliverables/vector/trees_2013_rgreen.shp") %>% st_as_sf(crs = 32611)
shps_2013 <- dplyr::filter(shps_2013, treeID %in% treeIDs)

shps_xy <- shps_2013 %>% st_drop_geometry()

# Divide into Soaproot saddle and teakettle

idx1 <- which(shps_xy$xlas2013 <= 310000 & shps_xy$xlas2013 > 285000)
idx2 <- which(shps_xy$xlas2013 > 310000)

r_neon <- crop(r, extent(shps_2013[idx1,]))

shps_2013_xy <- shps_2013 %>% 
  st_drop_geometry() %>% 
  as.data.frame() %>% 
  st_as_sf(coords = c("xlas2013", "ylas2013"), crs=32611)

raster_soap <- rasterize(shps_2013_xy[idx1,], r_neon, 1-shps_2013_xy[idx1,]$live, fun = mean)
raster_soap <- raster_soap*soap_mask_30_strict
raster_soap_df <- rasterToPoints(raster_soap) %>% as.data.frame()
str(raster_soap_df)
test <- sum(!is.na(raster_soap_df$layer))

ggplot() + geom_raster(data = raster_soap_df, mapping = aes(x=x, y=y, fill=layer))
writeRaster(raster_soap, "data/deliverables/raster/soap_mortality_2013.tif", overwrite = TRUE)

# Calculate for TEAK
r_neon <- crop(r, extent(shps_2013[idx2,]))
raster_teak <- rasterize(shps_2013_xy[idx2,], r_neon, 1-shps_2013_xy[idx2,]$live, fun = mean)
raster_teak <- raster_teak*teak_mask_30_strict
raster_teak_df <- rasterToPoints(raster_teak, spatial = TRUE) %>% data.frame()

ggplot() + geom_raster(data = raster_teak_df, mapping = aes(x = x, y = y, fill = layer))

writeRaster(raster_teak, "data/deliverables/raster/teak_mortality_2013.tif", overwrite = TRUE)

## 2018

shps_2018 <- read_sf("data/deliverables/vector/trees_2018_rgreen.shp") %>% st_as_sf(crs = 32611)
shps_2018 <- dplyr::filter(shps_2018, treeID %in% treeIDs)

shps_xy <- shps_2018 %>% st_drop_geometry()

# Divide into Soaproot saddle and teakettle

idx1 <- which(shps_xy$x <= 310000 & shps_xy$x> 285000)
idx2 <- which(shps_xy$x > 310000)

r_neon <- crop(r, extent(shps_2018[idx1,]))

shps_2018_xy <- shps_2018 %>% 
  st_drop_geometry() %>% 
  as.data.frame() %>% 
  st_as_sf(coords = c("x", "y"), crs=32611)

raster_soap <- rasterize(shps_2018_xy[idx1,], r_neon, 1-shps_2018_xy[idx1,]$live, fun = mean)
raster_soap <- raster_soap*soap_mask_30_strict
raster_soap_2018_df <- rasterToPoints(raster_soap) %>% as.data.frame()
test <- sum(!is.na(raster_soap_2018_df$layer))

ggplot() + geom_raster(data = raster_soap_2018_df, mapping = aes(x=x, y=y, fill=layer))
writeRaster(raster_soap, "data/deliverables/raster/soap_mortality_2018.tif", overwrite = TRUE)

# Calculate for TEAK
r_neon <- crop(r, extent(shps_2018[idx2,]))
raster_teak <- rasterize(shps_2018_xy[idx2,], r_neon, 1-shps_2018_xy[idx2,]$live, fun = mean)
raster_teak <- raster_teak*teak_mask_30_strict
raster_teak_2018_df <- rasterToPoints(raster_teak, spatial = TRUE) %>% data.frame()

ggplot() + geom_raster(data = raster_teak_2018_df, mapping = aes(x = x, y = y, fill = layer))

writeRaster(raster_teak, "data/deliverables/raster/teak_mortality_2018.tif", overwrite = TRUE)

## 2019

shps_2019 <- read_sf("data/deliverables/vector/trees_2019_rgreen.shp") %>% st_as_sf(crs = 32611)
shps_2019 <- dplyr::filter(shps_2019, treeID %in% treeIDs)

shps_xy <- shps_2019 %>% st_drop_geometry()

# Divide into Soaproot saddle and teakettle

idx1 <- which(shps_xy$x<= 310000 & shps_xy$x > 285000)
idx2 <- which(shps_xy$x > 310000)

r_neon <- crop(r, extent(shps_2019[idx1,]))

shps_2019_xy <- shps_2019 %>% 
  st_drop_geometry() %>% 
  as.data.frame() %>% 
  st_as_sf(coords = c("x", "y"), crs=32611)

raster_soap <- rasterize(shps_2019_xy[idx1,], r_neon, 1-shps_2019_xy[idx1,]$live, fun = mean)
raster_soap <- raster_soap*soap_mask_30_strict
raster_soap_2019_df <- rasterToPoints(raster_soap) %>% as.data.frame()
str(raster_soap_2019_df)
test <- sum(!is.na(raster_soap_2019_df$layer))

ggplot() + geom_raster(data = raster_soap_df, mapping = aes(x=x, y=y, fill=layer))
writeRaster(raster_soap, "data/deliverables/raster/soap_mortality_2019.tif", overwrite = TRUE)

# Calculate for TEAK
r_neon <- crop(r, extent(shps_2019[idx2,]))
raster_teak <- rasterize(shps_2019_xy[idx2,], r_neon, 1-shps_2019_xy[idx2,]$live, fun = mean)
raster_teak <- raster_teak*teak_mask_30_strict
raster_teak_2019_df <- rasterToPoints(raster_teak, spatial = TRUE) %>% data.frame()

ggplot() + geom_raster(data = raster_teak_2019_df, mapping = aes(x = x, y = y, fill = layer))

writeRaster(raster_teak, "data/deliverables/raster/teak_mortality_2019.tif", overwrite = TRUE)

## 2021

shps_2021 <- read_sf("data/deliverables/vector/trees_2021_rgreen.shp") %>% st_as_sf(crs = 32611)
shps_2021 <- dplyr::filter(shps_2021, treeID %in% treeIDs)
shps_xy <- shps_2021 %>% st_drop_geometry()

# Divide into Soaproot saddle and teakettle

idx1 <- which(shps_xy$x <= 310000 & shps_xy$x > 285000)
idx2 <- which(shps_xy$x > 310000)

r_neon <- crop(r, extent(shps_2021[idx1,]))

shps_2021_xy <- shps_2021 %>% 
  st_drop_geometry() %>% 
  as.data.frame() %>% 
  st_as_sf(coords = c("x", "y"), crs=32611)

raster_soap <- rasterize(shps_2021_xy[idx1,], r_neon, 1-shps_2021_xy[idx1,]$live, fun = mean)
raster_soap <- raster_soap*soap_mask_30_strict
raster_soap_2021_df <- rasterToPoints(raster_soap) %>% as.data.frame()
str(raster_soap_2021_df)
test <- sum(!is.na(raster_soap_2021_df$layer))

ggplot() + geom_raster(data = raster_soap_2021_df, mapping = aes(x=x, y=y, fill=layer))
writeRaster(raster_soap, "data/deliverables/raster/soap_mortality_2021.tif", overwrite = TRUE)

# Calculate for TEAK
r_neon <- crop(r, extent(shps_2021[idx2,]))
raster_teak <- rasterize(shps_2021_xy[idx2,], r_neon, 1-shps_2021_xy[idx2,]$live, fun = mean)
raster_teak <- raster_teak*teak_mask_30_strict
raster_teak_2021_df <- rasterToPoints(raster_teak, spatial = TRUE) %>% data.frame()

ggplot() + geom_raster(data = raster_teak_2021_df, mapping = aes(x = x, y = y, fill = layer))

writeRaster(raster_teak, "data/deliverables/raster/teak_mortality_2021.tif", overwrite = TRUE)

