#!/usr/bin/env Rscript

library(dplyr)
library(raster)
library(sf)
setwd("/dfs4/jranders_lab/users/hemmingn/sierra/landsat_analysis")
fname <- list.files("/dfs4/jranders_lab/users/hemmingn/Landsat", pattern = glob2rx("LC08*_042034_*.TIF"), recursive = TRUE, full.names = TRUE)[1]
ls8 <- raster(fname)*NA

for (year in c("2013", "2017","2018","2019","2021")){

  data_dir <- paste0("/dfs4/jranders_lab/users/hemmingn/NEON/chm/", year, "/SOAP")
  
  fnames <- list.files(path = data_dir, full.names = TRUE) 
  tf <- list()
  
  for (i in 1:length(fnames)){
    fname <- fnames[i]
    r <- raster(fname)
    tf[[i]] <- !is.na(r)
  }
  
  soap_mask <- do.call(merge, tf)
  spdf <- rasterToPoints(soap_mask, spatial=TRUE)
  ls8_soap <- crop(ls8, extent(spdf))
  soap_mask_30 <- rasterize(x=spdf, y=ls8, field="layer", fun = mean)
  
  fname <- paste0("soap_mask_30_",year,".tif")
  check <- list.files(path = ".", pattern = fname)
  if (length(check)>0){
    writeRaster(soap_mask_30, fname, overwrite = TRUE)
  } else{
    writeRaster(soap_mask_30, fname)
  }
  rm(spdf, ls8_soap, soap_mask_30, tf, r)
  data_dir <- paste0("/dfs4/jranders_lab/users/hemmingn/NEON/chm/", year, "/TEAK")
  
  fnames <- list.files(path = data_dir, full.names = TRUE) 
  tf <- list()
  
  for (i in 1:length(fnames)){
    fname <- fnames[i]
    r <- raster(fname)
    tf[[i]] <- !is.na(r)
  }
  
  teak_mask <- do.call(merge, tf)
  spdf <- rasterToPoints(teak_mask, spatial = TRUE)
  ls8_teak <- crop(ls8, extent(spdf))
  teak_mask_30 <- rasterize(spdf, ls8_teak, field='layer', fun = mean)
  fname <- paste0("teak_mask_30_",year,".tif")
  check <- list.files(path = ".", pattern = fname)
  if (length(check)>0){
    writeRaster(teak_mask_30, fname, overwrite = TRUE)
  } else{
    writeRaster(teak_mask_30, fname)
  }
  rm(spdf, ls8_teak, teak_mask_30, tf, r)
}


