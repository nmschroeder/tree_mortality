#!/usr/bin/env Rscript
fname <- commandArgs(trailingOnly = TRUE)
print(fname)
library(dplyr)
library(raster)
library(rhdf5)
library(stringr)

data_dir <- "/path/to/your/data/directory/"

wd <- paste0(data_dir, "spectral")


setwd(wd)

pat <- "[0-9]{6}_[0-9]{7}"
xy_tag <- str_extract(fname, pattern = pat)
xy <- as.numeric(str_split(xy_tag, pattern = "_")[[1]])

year_extract <- function(fname){
  yr <- str_extract(fname, pattern = "/[0-9]{4}/")
  return(gsub("/", "", yr))
}

site_extract <- function(fname){
  s <- str_extract(fname, pattern = "/NEON/spectral/[0-9]{4}/[A-Z]{4}")
  return(gsub("/NEON/spectral/[0-9]{4}/", "", s))
}

year <- year_extract(fname)
site <- site_extract(fname)

print("Year and site")
year
site

fid <- H5Fopen(fname)

if (site == "TEAK"){
  reflec <- fid$TEAK$Reflectance$Reflectance_Data
  reflec[reflec == -9999] <- NA
  wv_nm <- fid$TEAK$Reflectance$Metadata$Spectral_Data$Wavelength
  map_info <- fid$TEAK$Reflectance$Metadata$Coordinate_System$Map_Info
  wvc <- fid$TEAK$Reflectance$Metadata$Ancillary_Imagery$Water_Vapor_Column
  haze <- fid$TEAK$Reflectance$Metadata$Ancillary_Imagery$Haze_Cloud_Water_Map
}

if (site == "SOAP"){
  reflec <- fid$SOAP$Reflectance$Reflectance_Data
  reflec[reflec == -9999] <- NA
  wv_nm <- fid$SOAP$Reflectance$Metadata$Spectral_Data$Wavelength
  map_info <- fid$SOAP$Reflectance$Metadata$Coordinate_System$Map_Info
  wvc <- fid$SOAP$Reflectance$Metadata$Ancillary_Imagery$Water_Vapor_Column
  haze <- fid$SOAP$Reflectance$Metadata$Ancillary_Imagery$Haze_Cloud_Water_Map
}


# Pull wavelength ranges from Landsat 8 Surface Reflectance Tier 2 (units in nanometers)
# We'll use this one to start, since a lot of previous analyses on NDVI and NDMI utilizes Landsat but
# will also include absorption bands [1449, 1455] and [1945, 1956] following Carter (1991), as referenced
# by Jensen (2006), pg. 366
lb <- c(452, 533, 636, 851, 1440, 1566, 1935)
ub <- c(512, 590, 673, 879, 1460, 1651, 1955)
bnd_names <- c("blue", "green", "red", "nir", "w1", "swir1", "w2", "wvc", "haze")
bnds <- cbind(lb, ub)
N <- nrow(bnds)

idx <- list()

for (i in 1:nrow(bnds)){
  idx[[i]] <- which(wv_nm >= bnds[i, 1] & wv_nm <= bnds[i, 2])
}

raster_data <- list()
for (i in 1:N){
  if (length(idx[[i]])>1){
    r <- colMeans(reflec[idx[[i]],,], dims = 1, na.rm = TRUE)
  }
  r <- t(r)/10000
  r <- raster(r)
  extent(r)<- extent(c(xy[1], xy[1]+1000, xy[2], xy[2] + 1000))
  raster_data[[i]] <- r
}


wvc <- t(wvc)
wvc <- raster(wvc)
extent(wvc) <- extent(c(xy[1], xy[1]+1000, xy[2], xy[2]+1000))
raster_data[[N+1]] <- wvc

haze <- t(haze)
haze <- raster(haze)
extent(haze) <- extent(c(xy[1],xy[1]+1000,xy[2],xy[2]+1000))
raster_data[[N+2]] <- haze


raster_stack <- do.call(stack, raster_data)
names(raster_stack) <- bnd_names

tifname <- paste0("spectral_",site,"_",year,"_",xy_tag, ".tif")

check <- list.files('.', pattern=tifname)
if (length(check)>0){
  writeRaster(raster_stack, tifname, overwrite = TRUE)
} else{
  writeRaster(raster_stack, tifname)
}
