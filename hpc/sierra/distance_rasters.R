#!/usr/bin/env Rscript

library(raster)
library(dplyr)
library(sf)
library(sp)
library(ggplot2)
library(stringr)

# HPC
data_dir <- "/path/to/your/directory/"

ls_dir <- paste0(data_dir, "Landsat")
out_dir <- data_dir
data_dir <- paste0(data_dir, "sierra/landscape")

# Read in las extent polygon
las_extent <- read_sf("neon_las_extent.shp") %>% st_buffer(dist = 1000)


# Read in landsat file

ls8 <- list.files(ls_dir,
                  pattern = glob2rx("LC08_*_042034_*B*.TIF"),
                  recursive = TRUE,
                  full.names = TRUE)

# In case this file is located in more than one location, just pick the first one
ls8 <- ls8[1]
ls8_raster <- raster(ls8)
ls8_raster <- crop(ls8_raster, las_extent)*NA
n_rows <- nrow(ls8_raster)

pts <- as.data.frame(ls8_raster, xy = TRUE) %>% st_as_sf(coords = c("x", "y"), dim = "XY", crs = crs(ls8_raster)) %>% dplyr::select(geometry)


# Read in shapefiles of landscape features
fnames <- list.files(path = paste0(data_dir,"/Summary"), pattern = glob2rx("Trans_RoadSegment_USFS.shp"), recursive = TRUE, full.names = TRUE)
roads <- lapply(fnames, read_sf)
roads <- do.call(rbind.data.frame, roads)

roads <- st_transform(roads, crs = st_crs(pts))
d_roads <- st_distance(pts, roads)
d_roads <- apply(d_roads, 1, FUN = min, na.rm = TRUE)

pts_features <- mutate(pts, d_roads = d_roads)
pts_features_sp <- as(pts_features, "Spatial")
rm(pts_features)
raster_set <- rasterFromXYZ(pts_features_sp, crs = ls8_raster)
writeRaster(raster_set, "d_roads.tif")
rm(raster_set, pts_features_sp, roads, d_roads)


# Read in water bodies (some have different sets of column names)
fnames <- list.files(path = paste0(data_dir,"/NHD_HR"), pattern = glob2rx("NHD*Waterbody*.shp"), recursive = TRUE, full.names = TRUE)
lakes <- lapply(fnames, read_sf)
for (i in 1:length(lakes)){
  if ("Visibility" %in% colnames(lakes[[i]])){
    lakes[[i]] <- dplyr::select(lakes[[i]], -Visibility)
  }
}

lakes <- do.call(rbind.data.frame, lakes)
idx <- which(st_contains(st_buffer(las_extent, dist = 10000), st_zm(st_transform(lakes, crs = 32611), drop = TRUE, what = "ZM"), sparse = FALSE))
lakes <- lakes[idx,]

lakes <- st_transform(lakes, crs = st_crs(pts))
d_lakes <- st_distance(pts, lakes)
rm(lakes)
d_lakes <- apply(d_lakes, 1, FUN = min, na.rm = TRUE)
pts_features <- mutate(pts, d_lakes = d_lakes)
pts_features_sp <- as(pts_features, "Spatial")
rm(pts_features)
raster_set <- rasterFromXYZ(pts_features_sp, crs = ls8_raster)
writeRaster(raster_set, "d_lakes.tif")
rm(raster_set, pts_features_sp, d_lakes, lakes) 


rivers<- list.files(path = paste0(data_dir, "/NHD_HR"), pattern = glob2rx("NHD*BurnLine*.shp"), recursive = TRUE, full.names = TRUE)
rivers <- lapply(rivers, read_sf)
rivers <- do.call(rbind.data.frame, rivers)
idx <- which(st_contains(st_buffer(las_extent, dist = 10000), st_zm(st_transform(rivers, crs = 32611), drop = TRUE, what = "ZM"), sparse = FALSE))
rivers <- st_zm(st_transform(rivers[idx,], crs = 32611), drop = TRUE, what = "ZM")
str(rivers)

# Rows are the individual points
d_rivers <- st_distance(pts, rivers)
d_rivers <- apply(d_rivers, 1, FUN = min, na.rm = TRUE)

pts_features <- mutate(pts, d_rivers = d_rivers)
pts_features_sp <- as(pts_features, "Spatial")
rm(pts_features)
raster_set <- rasterFromXYZ(pts_features_sp, crs = ls8_raster)
writeRaster(raster_set, "d_rivers.tif")
rm(pts_features_sp, d_rivers, rivers, raster_set)


# Electric lines
elec <- read_sf(paste0(data_dir,"/California_Electric_Transmission_Lines-shp/California_Electric_Transmission_Lines.shp"))
elec <- st_transform(elec, crs = 32611)
idx <- which(st_contains(st_buffer(las_extent, dist = 10000), elec, sparse = FALSE))
elec <- elec[idx,]

elec <- st_transform(elec, crs = st_crs(pts))
d_elec <- st_distance(pts, elec)
d_elec <- apply(d_elec, 1, FUN = min, na.rm = TRUE)

pts_features <-	mutate(pts, d_elec = d_elec)
pts_features_sp <- as(pts_features, "Spatial")
rm(pts_features)
raster_set <- rasterFromXYZ(pts_features_sp, crs = ls8_raster)
writeRaster(raster_set,	"d_elec.tif")

