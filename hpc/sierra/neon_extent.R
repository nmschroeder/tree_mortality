#!/usr/bin/env Rscript

library(dplyr)
library(sf)

data_dir <- "/path/to/your/data/directory/"
data_dir <- paste0(data_dir, "sierra")
setwd(data_dir)

shps <- read_sf("tree_locations_las_intersection.shp")

ext <- st_bbox(shps)

p1 <- st_point(x = c(ext$xmin, ext$ymin), dim = "XY")
p2 <- st_point(x = c(ext$xmin, ext$ymax), dim = "XY")
p3 <- st_point(x = c(ext$xmax, ext$ymax), dim = "XY")
p4 <- st_point(x = c(ext$xmax, ext$ymin), dim = "XY")

pgon <- st_polygon(x=list(rbind(p1, p2, p3, p4, p1)), dim = "XY")
pgon <- st_sfc(pgon, crs = 32611)
st_write(pgon, "neon_las_extent.shp")
