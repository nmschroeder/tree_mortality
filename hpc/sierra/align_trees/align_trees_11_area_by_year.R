#!/usr/bin/env Rscript

# Create data frame of the area of each crown perimeter for each year

# HPC
data_dir <- "/path/to/your/data/directory/"
home_dir <- paste0(data_dir, "sierra")

library(dplyr)
library(sf)

setwd(home_dir)

shps2013 <- read_sf("tree_objects_2013.shp") %>% dplyr::select(treeID)
shps2017 <- read_sf("tree_objects_2017.shp") %>% dplyr::select(treeID) 
shps2018 <- read_sf("tree_objects_2018.shp") %>% dplyr::select(treeID) 
shps2019 <- read_sf("tree_objects_2019.shp") %>% dplyr::select(treeID) 
shps2021 <- read_sf("tree_objects_2021.shp") %>% dplyr::select(treeID) 

shp_locs <- read_sf("tree_locations_las_intersection.shp")

treeIDs <- shp_locs$treeID

area2013 <- dplyr::filter(shps2013) %>% st_area() %>% as.numeric()
shps2013 <- shps2013 %>% st_drop_geometry() %>% mutate(area2013 = area2013)

area2017 <- dplyr::filter(shps2017) %>% st_area() %>% as.numeric()
shps2017 <- shps2017 %>% st_drop_geometry() %>% mutate(area2017 = area2017)

area2018 <- dplyr::filter(shps2018) %>% st_area() %>% as.numeric()
shps2018 <- shps2018 %>% st_drop_geometry() %>% mutate(area2018 = area2018)

area2019 <- dplyr::filter(shps2019) %>% st_area() %>% as.numeric()
shps2019 <- shps2019 %>% st_drop_geometry() %>% mutate(area2019 = area2019)

area2021 <- dplyr::filter(shps2021) %>% st_area() %>% as.numeric()
shps2021 <- shps2021 %>% st_drop_geometry() %>% mutate(area2021 = area2021)

area_df <- right_join(shps2013, shps2017, by = "treeID") %>% right_join(shps2018, by = "treeID") %>% 
  right_join(shps2019, by = "treeID") %>% 
  right_join(shps2021, by = "treeID")

write.csv(area_df, "tree_crown_area_by_year.csv")




