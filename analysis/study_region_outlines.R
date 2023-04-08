library(dplyr)
library(sf)
library(raster)
library(smoothr)

# Read in the data masks
soap_mask <- raster("data/deliverables/raster/masks/soap_mask.tif")
teak_mask <- raster("data/deliverables/raster/masks/teak_mask.tif")

# Assign any value greater than 0 to 1 and the rest to NA
soap_mask[soap_mask==0] <- NA
soap_mask[!is.na(soap_mask)]<- 1
teak_mask[teak_mask==0] <- NA
teak_mask[!is.na(teak_mask)]<- 1

# Sum up the study area
study_area <- sum(colSums(soap_mask, na.rm = TRUE), na.rm = TRUE) + sum(colSums(teak_mask, na.rm = TRUE), na.rm = TRUE)
study_area_m2 <- study_area*900 # each pixel is 30m x 30m

# Convert the area from sq. meters to sq. kilometers
study_area_km2 <- study_area_m2 * (1/(1000)^2)
study_area_km2 

# Convert the rasters to polygons
soap_polys <- rasterToPolygons(soap_mask, na.rm = TRUE, dissolve = TRUE)
teak_polys <- rasterToPolygons(teak_mask, na.rm = TRUE, dissolve = TRUE)

# Convert the polygons to spatial feature objects
soap_polys <- as(soap_polys, 'sf')
teak_polys <- as(teak_polys, 'sf')

st_write(soap_polys, "data/intermediate/study_region/soap_mask_polygon.shp", delete_dsn = TRUE)
st_write(teak_polys, "data/intermediate/study_region/teak_mask_polygon.shp", delete_dsn = TRUE)

# Smooth the polygons for visualization
soap_smooth <- smooth(soap_polys, method = 'ksmooth', smoothness = 4)
teak_smooth <- smooth(teak_polys, method = "ksmooth", smoothness = 4)

# Save 
st_write(soap_smooth, "data/intermediate/study_region/soap_smoothed_outline.shp", delete_dsn = TRUE)
st_write(teak_smooth, "data/intermediate/study_region/teak_smoothed_outline.shp", delete_dsn = TRUE)

