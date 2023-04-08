# Readme file

# This directory contains scripts for finding individual tree locations, drawing polygons around tree crown perimeters for each
# year of data, and estimating whether each tree is alive or dead.

# Note that some scripts below are dependent on the outputs from each of the following subdirectories
# /tree_mortality/hpc/sierra/data_download
# /tree_mortality/hpc/sierra/granite 
# /tree_mortality/hpc/sierra/spectral 

# Note that numbered files are typically (but not always) dependent on the previous files with the same prefix. For example, 
# canopy_cover_02.R (which is run by canopy_cover_02.sub) is dependent on the output of canopy_cover_01.R.

# Check the input line numbers for any task array files. For example, you can use
# wc -l cluster_02_inputs.txt
# to check the number of lines that need to be fed into the array file for cluster_02_metrics.sub.

# 1) First files to run:

# A) align_trees_01_generate_indices.R

# Generates indices for use a moving buffered window to explore the spatial extent of the lidar catalogs downloaded
# from NEON for Soaproot Saddle and Lower Teakettle

# B) align_trees_02_generate_trees.R

# Generates tree locations for each moving window for the combined point cloud and uses those locations to create
# crown perimeter polygons for each tree for each year of data.

# C) align_trees_03_combine_output.R

# Combines the output of the task array from align_trees_02.sub

# D) align_trees_04_find_intersection.R

# Find trees which have data entries for all of the years in the study (2013, 2017, 2018, 2019, and 2021). Note, some trees
# generated in the previous files have data for only some of the years.

# E) align_trees_05_find_tree_locations.R

# Create a point data frame for each tree top location (as opposed to the shape files with polygons describing the crown perimeters)

# F) align_trees_06_sample_indices.R

# Create a text file of indices to search the whole data frame of trees in smaller batches

# G) align_trees_07_find_training_set.R

# We created a shapefile of squares using the extents of Landsat pixels in a regular grid as described in the manuscript
# and called it training_sample_sites.shp. We placed this dataset in /tree_mortality/data/training
# This script reads that in and computes whether or not each tree in our dataset intersects our sampling sites for our 
# hand-labeled dataset. This is computationally expensive, so we only look at batches of trees at a time as determined by the sample 
# indices from the previous script.

# H) align_trees_08_combine_samples.R

# Combines the output from above to indicate which of our trees fall in a sampling site and need to be hand-labeled in QGIS.

# I) align_trees_09_crown_area.R

# Computes the crown area of each tree for each year and adds the crown area measures to the training data set data frame

# J) align_trees_10_intersection_locations.R

# Determines which of the tree locations are in the intersection of all 5 data years of lidar availability

# K) align_trees_11_area_by_year.R

# Creates a data frame for all of our trees in the data intersection of the area by year. This item is redundant with item I), but
# generates a data frame we can use to quickly determine which trees should be labeled dead by the change in their crown area.

# L) align_trees_12_dead_by_year.R

# This script tracks the year that the crown area of any tree that had a crown area greater than 1 sq. meter in 2013 fell below
# 1 sq. meter and labels the tree dead by area.

# 2) Data Masks

# A) masks.R

# This script generates a data mask using the extents of NEON's canopy height model and is used in estimating tree mortality
# using the vegetation indices

# B) chm_mask_01_masks_by_year.R 

# This is an alternate method of creating a data mask at the Landsat resolution used for Landsat-scale visualizations and
# analyses. Any Landsat pixel which contains a canopy height model pixel from NEON with a value of NA is also assigned to NA

# C) chm_mask_02_combine_masks.R

# This script combines the masks generated for each canopy height model tile in the previous script's task array.

# 3) Fitting vegetation indices to hand-labeled training data

# The following file prefixes correspond to the vegetation indices explored in our study:
# ndmi, ndmi01, ndmi02, ndvi, rgreen

# The files exploring these indices could be consolidated, but for now, they are computed separately. Since they each
# compute the same things, we'll use xxxx as the prefix to represent a vegetation index identifier. If no identifer is present,
# that files corresponds to NDVI which was at one point the only index we were exploring early in the study's history. Some
# parts of the analysis are only computed for NDVI (ndvi) and relative greenness (rgreen), because those were the top two 
# indices.

# A) xxxx_01_grid_search.R

# Computes a grid search of the parameter space from 0 to 1 for each index to look for the best pair of parameters to the nearest
# 100th. 

# B) xxxx_02_optim_params.R

# Uses the grid search results to identify the optimal parameters

# C) xxxx_03_apply_params.R

# Applies the optimal parameters to the entire dataset to create labels based on the vegetation index

# D) xxxx_04_ensemble_setup.R

# To investigate how quickly the algorithm converges (or not) the parameters, we arrange the training labels in 30
# different random arrangements (ensembles). For each arrangement, we then estimate the parameters from the first 10 trees and
# keep fitting the parameters adding 10 trees each time until we run out of training labels. This script sets up the
# ensembles.

# E) xxxx_05_apply_ensemble.R

# This script runs each piece of the ensembles separately in a task array using seeds to keep track of the ensemble numbers.

# F) xxxx_06_combine_ensemble.R

# This script combines the outputs of the ensembles.

# G) xxxx_07_test_accuracy.R

# Applies the optimal parameters to the test data set and computes the accuracy

# 4) Matching our dataset to the data set from Stovall et al.

# A) tree_match_01.R

# This script generates indices from the number of rows in the Stovall data set to perform our data matching in small batches

# B) tree_match_02.R

# Each tree in the Stovall et al. (2019) dataset is compared to all of the trees in our data set. If their tree's location falls 
# inside the crown perimeter of one of our trees, the trees are considered a match. If there is more than one tree meeting
# this criteria, we take the one with the closest tree height.

# C) tree_match_03.R

# This script combines the results from the previous one which is run as a task array.













