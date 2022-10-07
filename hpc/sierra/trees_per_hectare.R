## Computing trees per hectare
library(raster)
library(sf)
library(ggplot2)
library(dplyr)
library(stringr)
library(exactextractr)
library(concaveman)

ls_dir <- "~/Data/Landsat"
sierra_dir <- "/Volumes/LaCie/tree_objects"
home_dir <- "~/Documents/R/sierra_resilience/align_trees"

ls_dir <- "/dfs4/jranders_lab/users/hemmingn/Landsat"
sierra_dir <- "/dfs5/bio/hemmingn/sierra/align_trees"
home_dir <- "/data/homezvol0/hemmingn/sierra"

# Read in shapefile
shps <- read_sf(paste0(sierra_dir, "/tree_locations_las_intersection.shp"))
shps <- dplyr::filter(shps, zmax2013>=5, ca2013>=1)

setwd(home_dir)

crds <- st_coordinates(shps$geometry)
x <- crds[,1]
y <- crds[,2]

shps <- mutate(shps, x = x, y = y)

xmn <- min(shps$x)
xmx <- max(shps$x)
ymn <- min(shps$y)
ymx <- max(shps$y)

p1 <- st_point(x = c(xmn, ymn), dim = "XY")
p2 <- st_point(x = c(xmn, ymx), dim = "XY")
p3 <- st_point(x = c(xmx, ymx), dim = "XY")
p4 <- st_point(x = c(xmx, ymn), dim = "XY")
pgon <- st_polygon(x = list(rbind(p1, p2, p3, p4, p1)), dim = "XY")
bbox_utm <- st_sfc(pgon, crs = 32611)

## Next, let's read in our Landsat file

ls8_files <- list.files(ls_dir, pattern = glob2rx("*4.*"), 
                        recursive = TRUE,
                        full.names = TRUE)

ls8 <- raster(ls8_files[1])
bbox_sp <- as(bbox_utm, "Spatial")
ls8 <- crop(ls8, bbox_sp)

r <- ls8*NA

pts <- data.frame(x = shps$x, y = shps$y)

pointcount = function(r, pts){
  # make a raster of zeroes like the input
  r2 = r
  r2[] = 0
  # get the cell index for each point and make a table:
  counts = table(cellFromXY(r, pts))
  # fill in the raster with the counts from the cell index:
  r2[as.numeric(names(counts))] = counts
  return(r2)
}

# Original
r2 = pointcount(r, pts)

soap <- dplyr::filter(shps, x < 310000)
soap_poly <- concaveman(soap, concavity = 2)
r <- mean(sqrt(st_area(soap$geometry)/pi))
soap_poly <- st_buffer(soap_poly, dist = r)

teak <- dplyr::filter(shps, x > 310000)
teak_poly <- concaveman(teak, concavity = 2)
r <- mean(sqrt(st_area(teak$geometry)/pi))
teak_poly <- st_buffer(teak_poly, dist = r)

rm(soap, teak)
sites_sf <- rbind(soap_poly, teak_poly)
sites_sp <- as(sites_sf, "Spatial")

r2_mask <- mask(r2, sites_sp)
writeRaster(r2_mask, "trees_per_pixel.tif")
r2_ha_mask <- r2_mask*100/9
writeRaster(r2_ha_mask, "trees_per_hectare.tif", overwrite = TRUE)




