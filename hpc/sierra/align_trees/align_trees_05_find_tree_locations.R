library(tidyr)
library(dplyr)
library(sf)
library(raster)
data_dir <- "/path/to/your/data/directory/"
home_dir <- paste0(data_dir, "sierra")
srtm_dir <- paste0(data_dir, "SRTM")
setwd(home_dir)

nasa_srtm <- list.files(srtm_dir, pattern = "hgt", full.names = TRUE)
nasa_srtm

elev_raster <- raster(nasa_srtm[1])
for (i in 2:length(nasa_srtm)){
  elev_raster <- raster::merge(elev_raster, raster(nasa_srtm[i]))
}

elev_raster <- projectRaster(elev_raster, crs = "+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Read in trees
trees_csv <- read.csv(paste0(home_dir, "tree_objects.csv"))

x <- ifelse(!is.na(trees_csv$xlas2013), trees_csv$xlas2013, NA) 
x <- ifelse(!is.na(x), x, trees_csv$xlas2017)
x <- ifelse(!is.na(x), x, trees_csv$xlas2018)
x <- ifelse(!is.na(x), x, trees_csv$xlas2019)
x <- ifelse(!is.na(x), x, trees_csv$xlas2021)

y <- ifelse(!is.na(trees_csv$ylas2013), trees_csv$ylas2013, NA) 
y <- ifelse(!is.na(y), y, trees_csv$ylas2017)
y <- ifelse(!is.na(y), y, trees_csv$ylas2018)
y <- ifelse(!is.na(y), y, trees_csv$ylas2019)
y <- ifelse(!is.na(y), y, trees_csv$ylas2021)

tree_locs <- data.frame(treeID = trees_csv$treeID, x = x, y = y) %>% drop_na()
tree_locs <- st_as_sf(tree_locs, coords = c("x", "y"), crs = 32611)
z <- raster::extract(elev_raster, tree_locs)
tree_locs_xyz <- mutate(tree_locs, z = z)

check <- list.files(pattern = "tree_locations.shp")
if (length(check)>0){
  st_write(tree_locs_xyz, "tree_locations.shp", update = FALSE, delete_dsn = TRUE)
} else{
  st_write(tree_locs_xyz, "tree_locations.shp")
}
