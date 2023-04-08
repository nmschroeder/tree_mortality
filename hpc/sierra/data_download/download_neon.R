#!/usr/bin/env Rscript

library(neonUtilities)
library(raster)
library(dplyr)

data_dir <- "/path/to/your/data/directory/"
dir_name <- paste0(data_dir, "NEON")

sites = c("SOAP", "TEAK")
years = c(2013, 2017, 2018, 2019, 2021) %>% as.character()

for (site in sites){
  for (year in years){
    # Download lidar-based elevation
    byFileAOP("DP3.30024.001", site = site, year = year, check.size = F, savepath = paste0(dir_name, "/elev/", year, "/", site))

    # Download canopy height model data
    byFileAOP("DP3.30015.001", site = site, year = year, check.size = F, savepath = paste0(dir_name, "/chm/",year,"/",site))
  
    # Download discrete lidar colorized
    byFileAOP("DP1.30003.001", site = site, year = year, check.size = F, savepath = paste0(dir_name, "/lidar/",year,"/",site))

    # Download spectral data
    byFileAOP("DP3.30006.001", site = site, year = year, check.size = F, savepath = paste0(dir_name, "/spectral/",year,"/",site))
  }
}
