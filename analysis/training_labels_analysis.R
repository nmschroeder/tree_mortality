# Script to analyze training labels

library(dplyr)
library(sf)

training_shps <- read_sf("data/training/trees_2017_training_filtered_labeled.shp")

str(training_shps)

# Total trees that we viewed in QGIS to label as live or dead
N_trees <- dim(training_shps)[1]
N_trees

# Check values assigned to training labels
unique(training_shps$live)

# How many of the training labels are assigned a valid live value (0 or 1)?
idx <- training_shps$live != -1
sum(idx)

# How many training labels were hand-labeled as live trees?
N_live <- (training_shps$live == 1) %>% sum()
N_live

# How many training labels were hand-labeled as dead trees?
N_dead <- (training_shps$live == 0) %>% sum()
N_dead

