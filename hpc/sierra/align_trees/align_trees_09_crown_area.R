#!/usr/bin/env Rscript

library(tidyr)
library(dplyr)
library(sf)
library(units)

data_dir <- "/path/to/your/data/directory/"
home_dir <- paste0(data_dir, "sierra/")
setwd(home_dir)

# Read in trees
trees_2013 <- read_sf(paste0(home_dir, "trees_2013_sample.shp")) 
trees_2017 <- read_sf(paste0(home_dir, "trees_2017_sample.shp")) 
trees_2018 <- read_sf(paste0(home_dir, "trees_2018_sample.shp")) 
trees_2019 <- read_sf(paste0(home_dir, "trees_2019_sample.shp")) 
trees_2021 <- read_sf(paste0(home_dir, "trees_2021_sample.shp"))

crown_area_2013 <- st_area(trees_2013$geometry) %>% drop_units()
crown_area_2017 <- st_area(trees_2017$geometry) %>% drop_units()
crown_area_2018 <- st_area(trees_2018$geometry) %>% drop_units()
crown_area_2019 <- st_area(trees_2019$geometry) %>% drop_units()
crown_area_2021 <- st_area(trees_2021$geometry) %>% drop_units()

# Add crown area ratio between 2013 and	2017
crown_ratio <- crown_area_2017/crown_area_2013

trees_2017 <- trees_2017 %>% dplyr::select(-xlas2017, -xlas2018, -xlas2019, -xlas2021, -ylas2017, -ylas2018, -ylas2019, -ylas2021, -X, -X_1)
str(trees_2017)

training_data <- mutate(trees_2017, 
                     ca2013 = crown_area_2013,
                     ca2017 = crown_area_2017,
                     ca2018 = crown_area_2018,
                     ca2019 = crown_area_2019,
                     ca2021 = crown_area_2021,
                     )

fname <- "trees_2017_training_data.shp"

check <- list.files(pattern = glob2rx(fname))
if (length(check) >= 1){
  st_write(training_data, fname, update = FALSE, delete_dsn = TRUE)
} else{
  st_write(training_data, fname)
}

# Filter the training data for easier labeling
training_data <- dplyr::filter(training_data, zmax2013>=5 & ca2013>= 1 & ca2017>=1)
training_data <- mutate(training_data, live = NA, certainty = NA)

fname <- "trees_2017_training_data_filtered.shp"

check <- list.files(pattern = glob2rx(fname))
if (length(check) >= 1){
  st_write(training_data, fname, append = FALSE, delete_dsn = TRUE)
} else{
  st_write(training_data, fname)
}
