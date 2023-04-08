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

spectral_idx <- "ndmi2"
spectral_name <- "NDMI2"
train_pct <- 0.6
valid_pct <- 0.2

# HPC
data_dir <- "/path/to/your/data/directory/"
sierra_dir <- paste0(data_dir, "sierra")
train_dir <- sierra_dir
spectral_dir <- paste0(sierra_dir, "/spectral/merged_rasters")
mask_dir <- paste0(sierra_dir, "/masks")

setwd(sierra_dir)

shps <- read_sf(paste0(train_dir,"/trees_2017_training_filtered_labeled.shp"))

# Filter out NA and equal to -1 values
shps <- dplyr::filter(shps, !is.na(live), live!=-1)
str(shps)

shp_locs <- read_sf(paste0(sierra_dir, "/tree_locations.shp"))
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

sidx_soap <- raster(paste0(spectral_dir, "/", spectral_idx, "_SOAP_2017.tif"))*soap_mask
sidx_teak <- raster(paste0(spectral_dir, "/", spectral_idx, "_TEAK_2017.tif"))*teak_mask

proj_string <- "+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

crs(sidx_soap) <- proj_string
crs(sidx_teak) <- proj_string
 
# Add columns of proportion dead for different index values

sidx_thresholds <- seq(0, 1, 0.01)
ratios <- seq(0, 1, 0.01)

for (i in 1:length(sidx_thresholds)){
  # How many pixels below the threshold?
  sidx_teak_bool <- sidx_teak<sidx_thresholds[i]
  # Find the ratio of pixels below the threshold
  mean_sidx <- exact_extract(sidx_teak_bool, shps_teak$geometry, fun = "mean")
  shps_teak <- cbind.data.frame(shps_teak, mean_sidx = mean_sidx)
  names(shps_teak)[names(shps_teak) == 'mean_sidx'] <- paste0(spectral_idx, as.character(i))
  
  sidx_soap_bool <- sidx_soap<sidx_thresholds[i]
  mean_sidx <- exact_extract(sidx_soap_bool, shps_soap$geometry, fun = "mean")
  shps_soap <- cbind.data.frame(shps_soap, mean_sidx = mean_sidx)
  names(shps_soap)[names(shps_soap) == 'mean_sidx'] <- paste0(spectral_idx, as.character(i))
}
print("Teakettle")
str(shps_teak)
print("Soaproot saddle")
str(shps_soap)

shps <- rbind.data.frame(shps_teak, shps_soap)
str(shps)

# Find the number of columns to use the last added column
m <- dim(shps)[2]

# Find which rows in the last column are not NA
idx <- which(!is.na(shps[,m]))

# Filter out values that are intersecting missing spectral tifs
shps <- shps[idx,]

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
st_write(shps,paste0("trees_2017_",spectral_idx,"_labels.shp"), append=FALSE)
print('finished generating label shapefile')

# Need to remove sf, sfc classes
df_sidx <- dplyr::select(shps, starts_with(spectral_idx)) 

sidx <- vector()
prop <- vector()
rdead <- vector()
acc <- vector()
count00 <- vector()
count01 <- vector() # False positive
count10 <- vector() # False negative
count11 <- vector()
total <- vector()

count <- 1

for (i in 1:length(sidx_thresholds)){
  for (j in 1:length(ratios)){
    N <- sum(!is.na(df_sidx[train_idx,i]), na.rm = TRUE)
    rd <- (sum(df_sidx[train_idx,i] >= ratios[j], na.rm = TRUE))/N
    
    # How many are labeled alive? Label which ones are below the dead ratio
    live_trees <- (df_sidx[train_idx,i]<ratios[j])
    N_correct <- sum((shps$live[train_idx] - live_trees*1.0)==0, na.rm = TRUE)
    sidx[count] <- sidx_thresholds[i]
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

sidx_indices <- data.frame(sidx = sidx, prop = prop, rdead = rdead, acc = acc, count00 = count00, count01 = count01, count10 = count10, count11 = count11, total = total)
idx <- which.max(sidx_indices$acc)
optim_params <- c(sidx = sidx_indices$sidx[idx], prop = sidx_indices$prop[idx])

sidx_plot <- ggplot(data = sidx_indices) + geom_contour_filled(mapping = aes(x = sidx, y = prop, z = rdead)) +
  xlim(c(-0.1, 1.1)) + ylim(c(-0.1, 1.1)) +
  labs(y = "Pixel ratio threshold", x = paste0(spectral_name," threshold")) +
  scale_fill_viridis_d(option = "B", name = "Proportion of dead trees") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_fixed()
sidx_plot
ggsave(paste0(spectral_idx,"_parameter_labels.png"), dpi = 600)

sidx_plot <- ggplot(data = sidx_indices) + geom_contour_filled(mapping = aes(x = sidx, y = prop, z = acc)) +
  xlim(c(-0.1, 1.1)) + ylim(c(-0.1, 1.1)) +
  labs(y = "Pixel ratio threshold", x = paste0(spectral_name, " threshold")) +
  scale_fill_viridis_d(option = "B", name = "Accuracy") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_fixed()
sidx_plot
ggsave(paste0(spectral_idx,"_parameter_labels_accuracy.png"), dpi = 600)


# Determine the optimal parameter based on the training data
idx <- which.max(sidx_indices$acc)
i <- which(sidx_thresholds == sidx_indices$sidx[idx])
j <- which(ratios == sidx_indices$prop[idx])

# Calculate and save the same table for the validation training set for the optimal parameters
N <- sum(!is.na(df_sidx[valid_idx,i]), na.rm = TRUE)
rd <- (sum(df_sidx[valid_idx,i] >= ratios[j], na.rm = TRUE))/N
#How many are labeled alive? Label which ones are below the dead ratio
live_trees <- df_sidx[valid_idx,i]<ratios[j]
N_correct <- sum((shps$live[valid_idx] - live_trees*1.0)==0, na.rm = TRUE)
sidx <- sidx_thresholds[i]
prop <- ratios[j]
rdead <- rd
acc <- N_correct/N
count00 <- sum(shps$live[valid_idx]==0 & live_trees == 0, na.rm = TRUE)
count01 <- sum(shps$live[valid_idx]==0 & live_trees == 1, na.rm = TRUE)
count10 <- sum(shps$live[valid_idx]==1 & live_trees == 0, na.rm = TRUE)
count11 <- sum(shps$live[valid_idx] == 1 & live_trees == 1, na.rm = TRUE)
total <- N

sidx_indices_valid <- data.frame(sidx = sidx, prop = prop, rdead = rdead, acc = acc, count00 = count00, count01 = count01, 
    count10 = count10, count11 = count11, total = total)

# Save these using the specific index name
names(sidx_indices)[names(sidx_indices) == 'sidx'] <- spectral_idx
names(sidx_indices_valid)[names(sidx_indices_valid) == 'sidx'] <- spectral_idx
write.csv(sidx_indices, paste0(spectral_idx,"_indices_labels_train.csv"))
write.table(optim_params, paste0(spectral_idx,"_optim_params.txt"), quote = FALSE, sep = ",", row.names = FALSE, col.names = FALSE)
write.csv(sidx_indices_valid, paste0(spectral_idx,"_indices_labels_valid.csv"))

