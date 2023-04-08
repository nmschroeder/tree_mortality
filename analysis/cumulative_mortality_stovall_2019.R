# This script calculates the cumulative mortality in the Stovall (2019) 
# dataset for the year 2016

# The Stovall (2019) dataset can be found here:
# https://doi.org/10.6084/m9.figshare.7609193.v2

library(dplyr)

# Fill in directory where figshare dataset is stored
data_dir <- "~/Documents/R/tree_mortality_stovall/"

# This dataset 
stovall_data <- read.csv(paste0(data_dir, "figshare/ALLtrees_v2.csv"))

# How many trees died cumulatively by 2016 in the Stovall (2019) dataset?

mean(stovall_data$dead)
