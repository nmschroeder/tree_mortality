#!/usr/bin/env Rscript
start_end <- commandArgs(trailingOnly = TRUE)

library(dplyr)
library(sf)
library(stringr)
start_end <- str_split(start_end, pattern = ",")
start_end <- do.call(c, start_end)
start_end <- as.numeric(start_end)
print(start_end)

data_dir <- "/path/to/your/data/directory/"

shp_dir <- paste0(data_dir, "sierra/")
data_dir <- paste0(shp_dir, "landscape")
out_dir <- data_dir

# Read in extents of LAS catalogs to identify the study region (the union of the las files)
las_list <- list.files(path = data_dir, pattern = glob2rx("neon_lidar*.shp"), full.names = TRUE)
las_list <- lapply(las_list, read_sf)
las_df <- do.call(rbind.data.frame, las_list)
las_union <- st_combine(las_df$geometry)
ex <- st_bbox(las_union)

# Check the extent4
p1 <- st_point(x = c(ex$xmin, ex$ymin), dim = "XY")
p2 <- st_point(x = c(ex$xmin, ex$ymax), dim = "XY")
p3 <- st_point(x = c(ex$xmax, ex$ymax), dim = "XY")
p4 <- st_point(x = c(ex$xmax, ex$ymin), dim = "XY")
pgon <- st_polygon(x = list(rbind(p1, p2, p3, p4, p1)))
pgon <- st_sfc(pgon, crs = 32611)

# Read in shapefiles of landscape features
fnames <- list.files(path = paste0(data_dir,"/Summary"), pattern = glob2rx("Trans_RoadSegment_USFS.shp"), recursive = TRUE, full.names = TRUE)
roads <- lapply(fnames, read_sf)
roads <- do.call(rbind.data.frame, roads)

# Read in water bodies (some have different sets of column names)
fnames <- list.files(path = paste0(data_dir,"/NHD_HR"), pattern = glob2rx("NHD*Waterbody*.shp"), recursive = TRUE, full.names = TRUE)
lakes <- lapply(fnames, read_sf)
for (i in 1:length(lakes)){
  if ("Visibility" %in% colnames(lakes[[i]])){
    lakes[[i]] <- dplyr::select(lakes[[i]], -Visibility)
  }
}

lakes <- do.call(rbind.data.frame, lakes)
idx <- which(st_contains(st_buffer(pgon, dist = 10000), st_zm(st_transform(lakes, crs = 32611), drop = TRUE, what = "ZM"), sparse = FALSE))
lakes <- lakes[idx,]

# Read in trails
fnames <- list.files(path = paste0(data_dir,"/Summary"), pattern = glob2rx("Trans_TrailSegment.shp"), recursive = TRUE, full.names = TRUE)
trails <- lapply(fnames, read_sf)
trails <- do.call(rbind.data.frame, trails)

# Rivers from the NHD high-resolution product 
rivers<- list.files(path = paste0(data_dir, "/NHD_HR"), pattern = glob2rx("NHD*BurnLine*.shp"), recursive = TRUE, full.names = TRUE)
rivers <- lapply(rivers, read_sf)
rivers <- do.call(rbind.data.frame, rivers)
idx <- which(st_contains(st_buffer(pgon, dist = 10000), st_zm(st_transform(rivers, crs = 32611), drop = TRUE, what = "ZM"), sparse = FALSE))
rivers <- st_zm(st_transform(rivers[idx,], crs = 32611), drop = TRUE, what = "ZM")

# Electric lines
elec <- read_sf(paste0(data_dir,"/California_Electric_Transmission_Lines-shp/California_Electric_Transmission_Lines.shp"))
elec <- st_transform(elec, crs = 32611)
idx <- which(st_contains(st_buffer(pgon, dist = 10000), elec, sparse = FALSE))
elec <- elec[idx,]


# Read in the shapefile for the 2016 trees
shps <- read_sf(paste0(shp_dir,"/tree_locations_las_intersection.shp"))
st_crs(shps) <- 32611

# Read in all the shape files
# transform the landscape shape files into the CRS of the tree file

# Transform the landscape features into 32611 (meters)
rivers <- st_transform(rivers$geometry, crs = 32611)
trails <- st_transform(trails$geometry, crs = 32611)
roads <- st_transform(roads$geometry, crs = 32611)
elec <- st_transform(elec$geometry, crs = 32611)
lakes <- st_transform(lakes$geometry, crs = 32611)

ptm <- proc.time()

d <- list()

# Minimum distance
for (i in start_end[1]:start_end[2]){
  shp <- shps[i,]
  d1 <- min(sf::st_distance(shp, rivers)) # rivers
  d2 <- min(sf::st_distance(shp, trails)) # trails
  d3 <- min(sf::st_distance(shp, roads)) # roads
  d4 <- min(sf::st_distance(shp, elec)) # elec
  d5 <- min(sf::st_distance(shp, lakes))
  d[[i]] <- data.frame(treeID = shp$treeID, d_rivers = d1, d_trails = d2, d_roads = d3, d_elec = d4, d_lakes = d5)
}

min_dist <- do.call(rbind.data.frame, d)

# Collect process time
proc.time() - ptm

# Save output
write.csv(min_dist, paste0(out_dir,"/landscape_distance_", as.character(start_end[1]),"_",as.character(start_end[2]),".csv"))



