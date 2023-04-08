#!/usr/bin/env Rscript

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

train_pct <- 0.6
valid_pct <- 0.2

# HPC
data_dir <- "/path/to/your/data/directory/"
sierra_dir <- paste0(data_dir, "sierra")
train_dir <- sierra_dir
spectral_dir <- paste0(sierra_dir, "/spectral/merged_rasters")
mask_dir <- paste0(sierra_dir, "/masks")

wd <- sierra_dir

setwd(wd)

shps <- read_sf(paste0(train_dir, "/trees_2017_training_filtered_labeled.shp"))

# Filter out NA and equal to -1 values
shps <- dplyr::filter(shps, !is.na(live), live!=-1)
str(shps)

shp_locs <- read_sf("tree_locations.shp")
str(shp_locs)

xy <- st_coordinates(shp_locs)
x <- xy[,1]
y <- xy[,2]
rm(xy)

shp_locs <- cbind.data.frame(shp_locs, x = x, y = y)
shp_locs <- shp_locs %>% dplyr::select(treeID, x)
shp_locs <- dplyr::filter(shp_locs, treeID %in% shps$treeID)

shps <- right_join(shps, shp_locs, by = "treeID")
sites <- ifelse(shps$x<308000, "SOAP", "TEAK")
shps <- mutate(shps, sites = sites) %>% st_as_sf()

shps_teak <- dplyr::filter(shps, sites == "TEAK")

shps_soap <- dplyr::filter(shps, sites == "SOAP")

print("Read in masks")
soap_mask <- raster(paste0(mask_dir, "/SOAP_mask.tif"))
teak_mask <- raster(paste0(mask_dir, "/TEAK_mask.tif"))

ndvi_soap <- raster(paste0(spectral_dir, "/ndvi_SOAP_2017.tif"))*soap_mask
ndvi_teak <- raster(paste0(spectral_dir, "/ndvi_TEAK_2017.tif"))*teak_mask

proj_string <- "+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

crs(ndvi_soap) <- proj_string
crs(ndvi_teak) <- proj_string

# Add columns of proportion dead for different NDVI indices

ndvi_thresholds <- seq(0, 1, 0.01)
ratios <- seq(0, 1, 0.01)

for (i in 1:length(ndvi_thresholds)){
  # How many pixels below the threshold?
  ndvi_teak_bool <- ndvi_teak<ndvi_thresholds[i]
  # Find the ratio of pixels below the threshold
  mean_ndvi <- exact_extract(ndvi_teak_bool, shps_teak$geometry, fun = "mean")
  shps_teak <- cbind.data.frame(shps_teak, mean_ndvi = mean_ndvi)
  names(shps_teak)[names(shps_teak) == 'mean_ndvi'] <- paste0('ndvi', as.character(i))
  
  ndvi_soap_bool <- ndvi_soap<ndvi_thresholds[i]
  mean_ndvi <- exact_extract(ndvi_soap_bool, shps_soap$geometry, fun = "mean")
  shps_soap <- cbind.data.frame(shps_soap, mean_ndvi = mean_ndvi)
  names(shps_soap)[names(shps_soap) == 'mean_ndvi'] <- paste0('ndvi', as.character(i))
}
print("Teakettle")
str(shps_teak)
print("Soaproot saddle")
str(shps_soap)

shps <- rbind.data.frame(shps_teak, shps_soap)
str(shps)

# Filter out values that are intersecting missing spectral tifs
shps <- dplyr::filter(shps, !is.na(ndvi1))

N_trees <- dim(shps)[1]
tree_seq <- 1:N_trees
set.seed(0)

# We are going to patition
N_train <- round(train_pct*N_trees)
N_valid <- round(valid_pct*N_trees)
N_test <- N_trees - N_train - N_valid

# Sample N_train trees from the sequence
train_idx <- sample(tree_seq, N_train)

# Create an array representing membership in the training set as a boolean
train_tf <- tree_seq %in% train_idx

# The remaining	indices	are the	negation of the	train_tf array
vt_tf <- !train_tf                  

# Obtain the indices to sample for the validation and testing dataset
vt_idx <- tree_seq[vt_tf]

# Sample the validation
valid_idx <- sample(vt_idx, N_valid)

# Create boolean arrays	for the	validation and testing dataset
valid_tf <- tree_seq %in% valid_idx
test_tf	<- !valid_tf & !train_tf

# Testing trees have to not be in the training dataset nor in the validation dataset
train <- vector(length = N_trees)
for (i in 1:N_trees){
  if (train_tf[i]){
    train[i] <- "train"
  }
  if (valid_tf[i]){
    train[i] <- "valid"
  }
  if (test_tf[i]){
    train[i] <- "test"
  }
}

shps <- cbind.data.frame(shps, train)

# Save this file in case the rest of the program fails...
st_write(shps, "trees_2017_ndvi_labels.shp", append=FALSE)
print('finished generating ndvi label shapefile')

# Need to remove sf, sfc classes
df_ndvi <- dplyr::select(shps, starts_with("ndvi")) 

ndvi <- vector()
prop <- vector()
rdead <- vector()
acc <- vector()
count00 <- vector()
count01 <- vector() # False positive
count10 <- vector() # False negative
count11 <- vector()
total <- vector()

count <- 1

for (i in 1:length(ndvi_thresholds)){
  for (j in 1:length(ratios)){
    N <- sum(!is.na(df_ndvi[train_idx,i]), na.rm = TRUE)
    rd <- (sum(df_ndvi[train_idx,i] >= ratios[j], na.rm = TRUE))/N
    
    # How many are labeled alive? Label which ones are below the dead ratio
    live_trees <- (df_ndvi[train_idx,i]<ratios[j])
    N_correct <- sum((shps$live[train_idx] - live_trees*1.0)==0, na.rm = TRUE)
    ndvi[count] <- ndvi_thresholds[i]
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

ndvi_indices <- data.frame(ndvi = ndvi, prop = prop, rdead = rdead, acc = acc, count00 = count00, count01 = count01, count10 = count10, count11 = count11, total = total)
write.csv(ndvi_indices, "ndvi_indices_labels_train.csv")

idx <- which.max(ndvi_indices$acc)
optim_params <- c(ndvi = ndvi_indices$ndvi[idx], prop = ndvi_indices$prop[idx])
write.table(optim_params, "optim_params.txt", quote = FALSE, sep = ",", row.names = FALSE, col.names = FALSE)

ndvi_plot <- ggplot(data = ndvi_indices) + geom_contour_filled(mapping = aes(x = ndvi, y = prop, z = rdead)) +
  xlim(c(-0.1, 1.1)) + ylim(c(-0.1, 1.1)) +
  labs(y = "Pixel ratio threshold", x = "NDVI threshold") +
  scale_fill_viridis_d(option = "B", name = "Proportion of dead trees") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_fixed()
ndvi_plot
ggsave("ndvi_parameter_labels.png", dpi = 600)

ndvi_plot <- ggplot(data = ndvi_indices) + geom_contour_filled(mapping = aes(x = ndvi, y = prop, z = acc)) +
  xlim(c(-0.1, 1.1)) + ylim(c(-0.1, 1.1)) +
  labs(y = "Pixel ratio threshold", x = "NDVI threshold") +
  scale_fill_viridis_d(option = "B", name = "Accuracy") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_fixed()
ndvi_plot
ggsave("ndvi_parameter_labels_accuracy.png", dpi = 600)


# Determine the optimal parameter based on the training data
idx <- which.max(ndvi_indices$acc)

i <- which(ndvi_thresholds == ndvi_indices$ndvi[idx])
j <- which(ratios == ndvi_indices$prop[idx])

# Calculate and save the same table for the valdiation set for the optimal parameters
N <- sum(!is.na(df_ndvi[valid_idx,i]), na.rm = TRUE)
rd <- (sum(df_ndvi[valid_idx,i] >= ratios[j], na.rm = TRUE))/N
#How many are labeled alive? Label which ones are below the dead ratio
live_trees <- df_ndvi[valid_idx,i]<ratios[j]
N_correct <- sum((shps$live[valid_idx] - live_trees*1.0)==0, na.rm = TRUE)
ndvi <- ndvi_thresholds[i]
prop <- ratios[j]
rdead <- rd
acc <- N_correct/N
count00 <- sum(shps$live[valid_idx]==0 & live_trees == 0, na.rm = TRUE)
count01 <- sum(shps$live[valid_idx]==0 & live_trees == 1, na.rm = TRUE)
count10 <- sum(shps$live[valid_idx]==1 & live_trees == 0, na.rm = TRUE)
count11 <- sum(shps$live[valid_idx] == 1 & live_trees == 1, na.rm = TRUE)
total <- N

ndvi_indices <- data.frame(ndvi = ndvi, prop = prop, rdead = rdead, acc = acc, count00 = count00, count01 = count01, 
    count10 = count10, count11 = count11, total = total)
write.csv(ndvi_indices, "ndvi_indices_labels_valid.csv")

