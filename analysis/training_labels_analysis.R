library(dplyr)
library(sf)

# Read in the manually labeled training data
shps <- read_sf("data/training/trees_2017_training_filtered_labeled.shp")
str(shps)

# Filter out any trees that could not be labeled (live = -1)
shps <- dplyr::filter(shps, !is.na(live), live!=-1)
str(shps)

# Determine the number of live and dead trees
live_trees <- sum(shps$live==1)
dead_trees <- sum(shps$live==0)

# Print them to the console
live_trees
dead_trees

# Number of live and dead trees
N <- live_trees+dead_trees
N

# Fractions of live and dead trees
live_trees/N
dead_trees/N


