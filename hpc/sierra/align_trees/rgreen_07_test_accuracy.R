#!/usr/bin/env Rscript

## Compute percent mortality for different mortality thresholds

require(sp)
require(Rcpp)
library(raster)
library(sf)
library(dplyr)

# HPC3
data_dir <- "/path/to/your/data/directory/"
sierra_dir <- paste0(data_dir, "sierra")
spectral_dir <-paste0(sierra_dir, "/spectral/merged_rasters") 
home_dir <- getwd()

setwd(sierra_dir)

shps <- read_sf(paste0(sierra_dir,"/trees_2017_rgreen_labels.shp"))

# Pull the test indices
test_idx <- which(shps$train == 'test')

# Determine the optimal threshold to the nearest 100th
idx_thresholds <- seq(0, 1, 0.01)
ratios <- seq(0, 1, 0.01)

# Need to remove sf, sfc classes
df_idx <- dplyr::select(shps, starts_with("rgreen")) %>% st_drop_geometry()

idx_values <- read.table(paste0(home_dir, "/rgreen_optim_params_years.txt"))

rgreen <- idx_values$V1[2]
prop <- idx_values$V2[2]

# Determine which NDVI and proportion created the optimal accuracy
i <- which(idx_thresholds==rgreen)
j <- which(ratios==prop)

print('indices')
print(i)
print(j)

# Calculate and save the same table for the test training set for the optimal parameters
N <- sum(!is.na(df_idx[test_idx,i]), na.rm = TRUE)
rd <- (sum(df_idx[test_idx,i] >= rgreen, na.rm = TRUE))/N
#How many are labeled alive? Label which ones are below the dead ratio
live_trees <- df_idx[test_idx,i]<rgreen
N_correct <- sum((shps$live[test_idx] - live_trees*1.0)==0, na.rm = TRUE)
rdead <- rd
acc <- N_correct/N
count00 <- sum(shps$live[test_idx]==0 & live_trees == 0, na.rm = TRUE)
count01 <- sum(shps$live[test_idx]==0 & live_trees == 1, na.rm = TRUE)
count10 <- sum(shps$live[test_idx]==1 & live_trees == 0, na.rm = TRUE)
count11 <- sum(shps$live[test_idx] == 1 & live_trees == 1, na.rm = TRUE)
total <- N

idx_indices <- data.frame(rgreen = rgreen, prop = prop, rdead = rdead, acc = acc, count00 = count00, count01 = count01, 
    count10 = count10, count11 = count11, total = total)
fname <- paste0(home_dir,"/rgreen_optim_params_test.csv")
write.csv(idx_indices, fname)

