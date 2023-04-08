## 3D lidar point cloud figure treetop comparison

library(lidR)
library(raster)
library(sf)
library(ggplot2)
library(plotly)
library(stringr)
library(dplyr)
library(RColorBrewer)
library(viridisLite)

# Replace with path to NEON lidar data; download the Soaproot Saddle lidar tiles from NEON with a southeast corner coordinates of 
# 298000 meters Easting and 4100000 meters Northing
lidar_dir <- "~/Data/NEON/lidar"

# Replace with directory where Stovall et al. (2019) figshare data set is located
stovall_dir <- "~/Documents/R/tree_mortality_stovall"

site <- "SOAP"
xlb <- 298050
xub <- 298150
ylb <- 4100800
yub <- 4100900

floor1000 <- function(x){
  x_temp <- round(x, -3)
  if (x_temp>x){
    x_temp <- x_temp-1000
  }
  return(x_temp)
}

query <- paste0("NEON*", site, "*", 
                as.character(floor1000(xlb)), 
                "_", 
                as.character(floor1000(ylb)), "*.laz")

fnames <- list.files(path = lidar_dir, 
                     pattern = glob2rx(query), 
                     recursive = TRUE, 
                     full.names = TRUE)

fnames

# Function to extract the year from a NEON lidar filename
lidar_year_extract <- function(fname){
  yr <- str_extract(fname, pattern = paste0("/[0-9]{4}/",site,"/"))
  yr <- str_extract(yr, pattern = "[0-9]{4}")
  return(as.numeric(yr))
}

# Collect the year for each filename
years <- lidar_year_extract(fnames)

# Function to remove outlying returns
remove_outlying_returns <- function(ctg, west_bnd, L_buffer){
  # Pull the extent of the input catalog
  L_ext <- extent(ctg)
  
  if (L_ext@xmin < west_bnd){
    L_ext@xmin <- west_bnd
    # Add a small buffer to the new extent to avoid clipping small sections
    # of the LAS catalog
    L_ext@xmax <- L_ext@xmax + L_buffer
    L_ext@ymin <- L_ext@ymin - L_buffer
    L_ext@ymax <- L_ext@ymax + L_buffer
    
    # Make a spatial points polygon to clip
    x_coord <- c(L_ext@xmin, L_ext@xmin, L_ext@xmax, L_ext@xmax)
    y_coord <- c(L_ext@ymin, L_ext@ymax, L_ext@ymax, L_ext@ymin)
    xy <- cbind(x_coord, y_coord)
    polyclip <- Polygon(xy)
    polyclip <- Polygons(list(polyclip), 1)
    polyclip <- SpatialPolygons(list(polyclip))
    proj4string(polyclip) <- "+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
    ctg_new <- catalog_intersect(ctg, polyclip)
  } else{
    ctg_new <- ctg
  }
  return(ctg_new)
}

w_bnd <- 280000
L_buffer <- 10

# Read in the files for each year as catalogs
las_cat <- lapply(fnames, readLAScatalog) %>% 
  lapply(remove_outlying_returns, w_bnd, L_buffer)

# Clip to the region of interest
las_clip <- lapply(las_cat, clip_rectangle, xlb, ylb, xub, yub, filter = "-drop_z_below 0")

# Use a resolution of 1 meter by 1 meter
r <- 1

# Grid the terrain from the lidar point cloud
gt <- lapply(las_clip, grid_terrain, r, knnidw())

# Normalize the heights of the lidar point cloud using the gridded terrain
las_data <- list()
las_corrected <- list()
for (i in 1:length(las_clip)){
  las_temp <- normalize_height(las_clip[[i]], gt[[i]], na.rm = TRUE)
  las_corrected[[i]] <- las_temp
  data_temp <- las_temp@data %>% dplyr::select(X, Y, Z)
  las_data[[i]] <- mutate(data_temp, year = years[i])
}

# Let's look at the 2013 corrected lidar point cloud using XQuartz
plot(las_corrected[[5]])

print("Combine the the las objects together")
las <- do.call(rbind.data.frame, las_data) %>% dplyr::filter(Z<80, Z>=0)
str(las)

las_sf <- st_as_sf(las, coords = c("X", "Y", "Z"), dim = "XYZ", crs = 32611)

chm <- grid_canopy(las_corrected[[1]], res = r, pitfree(c(0,2,5,10,15), c(0, 1.5)))

shps2013 <- read_sf("data/deliverables/vector/trees_2013_rgreen.shp")
str(shps2013)

data2013 <- dplyr::select(shps2013, x = xlas2013, y = ylas2013, z = zmax2013) %>% st_drop_geometry()
data2013 <- dplyr::filter(data2013, x >= xlb, x<= xub, y>=ylb, y<=yub)
data2013sf <- st_as_sf(data2013, coords = c("x", "y", "z"), dim = "XYZ", crs = 32611)

# Read in CSV of Stovall et al. (2019) dataset
shps <- read.csv(paste0(stovall_dir, "/figshare/ALLtrees_v2.csv"))
idx <- which(shps$x >= xlb & shps$x <= xub & 
               shps$y >= ylb & shps$y <= yub)
shps <- shps[idx,] 

# Convert to sf object
shps <- shps %>% st_as_sf(coords = c("x", "y", "zmax"), dim = "XYZ", crs = 32611)

# Plot our data set using XQuartz and lidR package
p1 <- plot(las_corrected[[1]], pal = magma(n = 8, begin = 0, end = 0.8, direction = 1), axis = FALSE)
add_treetops3d(p1, data2013sf, color = 'white')
p1

# Plot Stovall et al. (2019) data set using XQuartz and lidR package
p2 <- plot(las_corrected[[1]], pal = magma(n = 8, begin = 0, end = 0.8, direction = 1), axis = FALSE)
add_treetops3d(p2, shps, color = 'white')
p2
