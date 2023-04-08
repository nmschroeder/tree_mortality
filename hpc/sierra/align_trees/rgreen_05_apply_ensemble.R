#!/usr/bin/env Rscript
param_inputs <- commandArgs(trailingOnly = TRUE)
print(param_inputs)

## Compute percent mortality for different mortality thresholds

require(sp)
require(Rcpp)
library(raster)
library(sf)
library(dplyr)
library(exactextractr)
library(tidyr)
library(ggplot2)
library(pracma)
library(stringr)

ensemble_id <- param_inputs[1] %>% as.numeric()
m <- param_inputs[2] %>% as.numeric()

# HPC3
data_dir <- "/path/to/your/data/directory/"
sierra_dir <- paste0(data_dir, "sierra")
spectral_dir <-paste0(sierra_dir, "/spectral/merged_rasters") 

setwd(sierra_dir)

shps <- read_sf(paste0(sierra_dir,"/trees_2017_rgreen_labels.shp"))

# Pull the train indices
train_idx <- which(shps$train == 'train')

N <- length(train_idx)

set.seed(ensemble_id)
# Sample only m of them
train_idx <- sample(train_idx, N, replace = FALSE)
train_idx <- train_idx[1:m]

# Pull the test indices
valid_idx <- which(shps$train == 'valid')

# Determine the optimal threshold to the nearest 100th
idx_thresholds <- seq(0, 1, 0.01)
ratios <- seq(0, 1, 0.01)

# Need to remove sf, sfc classes
df_idx <- dplyr::select(shps, starts_with("rgreen")) %>% st_drop_geometry()

rgreen <- vector()
prop <- vector()
rdead <- vector()
acc <- vector()
count00 <- vector()
count01 <- vector() # False positive
count10 <- vector() # False negative
count11 <- vector()
total <- vector()

count <- 1

for (i in 1:length(idx_thresholds)){
  for (j in 1:length(ratios)){
    N <- m
    rd <- mean(df_idx[train_idx,i] >= ratios[j])
    
    # How many are labeled alive? Label which ones are below the dead ratio
    live_trees <- (df_idx[train_idx,i]<ratios[j])
    N_correct <- sum((shps$live[train_idx] - live_trees*1.0)==0, na.rm = TRUE)
    rgreen[count] <- idx_thresholds[i]
    prop[count] <- ratios[j]
    rdead[count] <- rd
    acc[count] <- N_correct/N
    count00[count] <- sum(shps$live[train_idx]==0 & live_trees == 0, na.rm = TRUE)
    count01[count] <- sum(shps$live[train_idx]==0 & live_trees == 1, na.rm = TRUE)
    count10[count] <- sum(shps$live[train_idx]==1 & live_trees == 0, na.rm = TRUE)
    count11[count] <- sum(shps$live[train_idx] == 1 & live_trees == 1, na.rm = TRUE)
    total[count] <- N
    count <- count + 1
  }
}

print('lengths of arrays')
length(rgreen)
length(prop)
length(rdead)
length(acc)
length(count00)
length(count01)
length(count10)
length(count11)
length(total)

print('create data frame for training data')
idx_indices <- data.frame(rgreen = rgreen, prop = prop, rdead = rdead, acc = acc, count00 = count00, count01 = count01, count10 = count10, count11 = count11, total = total)

# Determine the optimal paramemter based on the training data
idx <- which.max(idx_indices$acc)
idx_values <- idx_indices[idx,]

fname <- paste0("rgreen_optim_params_train_",as.character(ensemble_id),"_",as.character(m),".csv")
write.csv(idx_values, fname)

# Determine which NDVI and proportion created the optimal accuracy
i <- which(idx_thresholds==idx_values$rgreen)
j <- which(ratios==idx_values$prop)

print('indices')
print(i)
print(j)

# Calculate and save the same table for the test training set for the optimal parameters
N <- sum(!is.na(df_idx[valid_idx,i]), na.rm = TRUE)
rd <- (sum(df_idx[valid_idx,i] >= ratios[j], na.rm = TRUE))/N
#How many are labeled alive? Label which ones are below the dead ratio
live_trees <- df_idx[valid_idx,i]<ratios[j]
N_correct <- sum((shps$live[valid_idx] - live_trees*1.0)==0, na.rm = TRUE)
rgreen <- idx_thresholds[i]
prop <- ratios[j]
rdead <- rd
acc <- N_correct/N
count00 <- sum(shps$live[valid_idx]==0 & live_trees == 0, na.rm = TRUE)
count01 <- sum(shps$live[valid_idx]==0 & live_trees == 1, na.rm = TRUE)
count10 <- sum(shps$live[valid_idx]==1 & live_trees == 0, na.rm = TRUE)
count11 <- sum(shps$live[valid_idx] == 1 & live_trees == 1, na.rm = TRUE)
total <- m
total_test <- N

idx_indices <- data.frame(rgreen = rgreen, prop = prop, rdead = rdead, acc = acc, count00 = count00, count01 = count01, 
    count10 = count10, count11 = count11, total = total, total_test)
fname <- paste0("rgreen_optim_params_valid_",as.character(ensemble_id),"_",as.character(m),".csv")
write.csv(idx_indices, fname)

