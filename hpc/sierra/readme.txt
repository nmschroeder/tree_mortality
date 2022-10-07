# Readme file

# This directory contains scripts for collecting feature variables for estimating mortality

# 1) First files to run:

# Clustering 01, 02, 03

# Distance 01 (not dependent on cluster 01, 02, 03)

# Distance 02, 03 (not dependent on cluster 01, 02, 03)
# Granite 02, 03, 04 (depends on output from distance 01, not dependent on cluster XX)

# Extract from raster (not dependent on previous)

# 2) Collect all the feature variables (need to include rgranite from granite_04*)

# combine_features.sub

# 3) Add labels

# add_labels.sub

# Make sure to adjust the array inputs for all 02 batch files as the total number of
# arrays may vary
