# Readme file

# This directory contains scripts for collecting feature variables for estimating mortality. 

# Note that some scripts below are dependent on the outputs from each of the following subdirectories
# /tree_mortality/hpc/sierra/align_trees 
# /tree_mortality/hpc/sierra/data_download
# /tree_mortality/hpc/sierra/granite 
# /tree_mortality/hpc/sierra/spectral 

# Note that numbered files are typically (but not always) dependent on the previous files with the same prefix. For example, 
# canopy_cover_02.R (which is run by canopy_cover_02.sub) is dependent on the output of canopy_cover_01.R.

# Check the input line numbers for any task array files. For example, you can use
# wc -l cluster_02_inputs.txt
# to check the number of lines that need to be fed into the array file for cluster_02_metrics.sub.

# 1) First files to run:

# A) Pull the extent of the NEON files
# neon_extent.R

# B) Generate raster data
# lidar_dem.R: computes slope and aspect from the NEON lidar digital terrain model
# prism_data.R: computes climate rasters during the drought from PRISM data
# srtm_processing.R: computes slope and aspect from NASA's SRTM datasets (needed for visuals only)
# trees_per_hectare.R: computes trees per hectare raster data for our tree locations and tree locations
# from Stovall et al. 2019, Nature Communications

# 2) Create additional metrics for each tree

# A) Create the clustering metrics and pre-step for the distance metrics
# cluster_01_indices.R: generates indices to feed into the task array in cluster_02*.sub
# cluster_02_metrics.R: computes clustering metrics for the set of trees fed in from the previous file
# cluster_03_combine.R: combines all the outputs of the task array in cluster_02*.sub
# distance_01_indices.R: generates indices to feed into a few task arrays below (not dependent on cluster_*.sub)

# B) Generate distances from landscape feature for each tree, granite metrics, and canopy cover
# distance_02_landscape.R: computes the distance of each tree from several landscape features
# distance_03_combine.R: combines the outputs of the task array in distance_02*.sub
# distance_rasters.sub: rasterizes the outputs from distance_03_combine.R (i.e. dependent on distance_03_combine.sub)

# granite_02_distance.sub: computes distance of each tree from large granite outcrops (uses output from distance_01_indices.sub)
# granite_03_combine.sub: combines the outputs from the task array granite_02*.sub
# granite_04_radius.sub: computes the fraction of granite within a 20-meter radius of each tree

# canopy_cover_02.R: computes the canopy cover fraction within 20 meters of each tree (uses output from distance_01_indices.sub)
# canopy_cover_03.R: combines the output of canopy_cover_02.sub

# 3) Collect feature variables from raster data files
# extract_01_indices.R: generates indices for a task array
# extract_02_rasters.R: extracts data for each tree location from raster data
# extract_03_array.R: extracts data for each buffered tree location from the NEON-based slope, aspect, and elevation rasters 
# and computes median within the buffer to smooth any noise
# extract_04_combine.R: combines a portion of the data produced above
# extract_05_combine.R: combines all of the raster-based metrics together

# 4) Collect all the feature variables (need to include rgranite from granite_04*)
# combine_features.R: combines all of the feature variables we computed together

# 5) Add live and dead labels for each feature variable
# add_labels.R: adds dead and live label to each tree


