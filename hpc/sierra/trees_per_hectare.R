## Computing trees per hectare
library(raster)
library(sf)
library(ggplot2)
library(dplyr)
library(stringr)
library(exactextractr)
library(concaveman)

# Describe data directories
data_dir <- "/path/to/your/data/directory/"
ls_dir <- paste0(data_dir, "Landsat")
sierra_dir <- paste0(data_dir, "sierra")
stovall_dir <- paste0(data_dir, "Stovall")

# Read in shapefile
shps <- read_sf(paste0(sierra_dir, "/tree_locations_las_intersection.shp"))
shps <- dplyr::filter(shps, zmax2013>=5, ca2013>=1)
print("filtered data frame")
str(shps)

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
crown_rad <- mean(sqrt(st_area(shps$geometry)/pi))
soap_poly <- st_buffer(soap_poly, dist = crown_rad)
soap_mask <- coverage_fraction(r2, soap_poly, crop = TRUE)[[1]]
writeRaster(soap_mask, "soap_mask.tif", overwrite = TRUE)
st_write(soap_poly, "soap_mask_polygon.shp", delete_dsn = TRUE)

teak <- dplyr::filter(shps, x > 310000)
teak_poly <- concaveman(teak, concavity = 2)
teak_poly <- st_buffer(teak_poly, dist = crown_rad)
teak_mask <- coverage_fraction(r2, teak_poly, crop = TRUE)[[1]]
writeRaster(teak_mask, "teak_mask.tif", overwrite = TRUE)
st_write(teak_poly, "teak_mask_polygon.shp", delete_dsn = TRUE)

rm(soap, teak)
sites_sf <- rbind(soap_poly, teak_poly)

# These were previously computed for masked r2 rasters; now we will mask during
# analysis
writeRaster(r2, "trees_per_pixel.tif", overwrite = TRUE)
r2_ha <- r2*100/9
writeRaster(r2_ha, "trees_per_hectare.tif", overwrite = TRUE)

## Let's do the same thing for Stovall et al. data

stovall <- read.csv(paste0(stovall_dir, "/ALLtrees_v2.csv"))
pts <- dplyr::select(stovall, x, y)
r2 = pointcount(r, pts)

soap <- dplyr::filter(pts, x<310000) %>% st_as_sf(coords = c("x", "y"), dim = "XY", crs = 32611)
soap_poly <- concaveman(soap, concavity = 2) %>% st_buffer(dist=crown_rad)
soap_mask <- coverage_fraction(r2, soap_poly, crop = TRUE)[[1]]
writeRaster(soap_mask, "stovall_soap_mask.tif", overwrite = TRUE)
st_write(soap_poly, "stovall_soap_poly.shp", delete_dsn = TRUE)

teak <- dplyr::filter(pts, x > 310000) %>% st_as_sf(coords = c("x", "y"), dim = "XY", crs = 32611)
teak_poly <- concaveman(teak, concavity = 2) %>% st_buffer(dist = crown_rad)
teak_mask <- coverage_fraction(r2, teak_poly, crop = TRUE)[[1]]
writeRaster(teak_mask, "stovall_teak_mask.tif", overwrite = TRUE)
st_write(teak_poly, "stovall_teak_poly.shp", delete_dsn = TRUE)


writeRaster(r2, "stovall_trees_per_pixel.tif", overwrite = TRUE)
