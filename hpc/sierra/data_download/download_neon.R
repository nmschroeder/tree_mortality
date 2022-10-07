#!/usr/bin/env Rscript

library(neonUtilities)
library(raster)
library(dplyr)

site = "NIWO"
years = seq(2017,2020) %>% as.character()

for (year in years){
 # Download canopy height model data
  byFileAOP("DP3.30015.001", site = site, year = year, check.size = F, savepath = paste0("/dfs4/jranders_lab/users/hemmingn/NEON/chm/",year,"/",site))

  # Download discrete lidar colorized
  byFileAOP("DP1.30003.001", site = site, year = year, check.size = F, savepath = paste0("/dfs4/jranders_lab/users/hemmingn/NEON/lidar/",year,"/",site))

  # Download spectral data
  byFileAOP("DP3.30006.001", site = site, year = year, check.size = F, savepath = paste0("/dfs4/jranders_lab/users/hemmingn/NEON/spectral/",year,"/",site))
}
