# Readme file

# The scripts in these directories create tree locations and crown perimeter shape files
# for the Soaproot Saddle and Lower Teakettles sites in the Sierra National Forest in California.

# To run these files, several data sets need to be downloaded and arranged in a data storage
# directory.

# We ran these files on a high performance computing cluster at UCI which uses a SLURM scheduler.

# All of the batch files to run these R scripts on an HPC with a SLURM scheduler are included. While
# we replaced the account name and partition with filler variables, we left the module names to 
# indicate the version numbers of the software we used.

# Each R file has a variable data_dir = "/path/to/your/data/directory/" which needs to be replaced
# by a directory path with a lot of storage space. Within this directory, we suggest the directory
# structure below. Items in sierra are typically created throughout the algorithm, though there are 
# a few exceptions. 

## The first few directories are for storing data sets that are used in the algorithm

# Landsat

# We retrieved Landsat Collection 2 Level 2 Tier 1 surface reflectance data from the USGS
# Earth Explorer website for path 42, row 34 and the years 2013 and 2017. One could just download
# the four raster data files with a date nearest September 15th for 2013 and the same for 2017.

# Stovall/figshare

# We downloaded the data set provided by Stovall et al. (2019) from here:
# https://doi.org/10.6084/m9.figshare.7609193.v2

# sierra/landscape/Summary

# We downloaded USFS road shapefiles and placed them here.

# sierra/landscape/NHD_HR

# We downloaded rivers and waterbodies from the National Hydrography Dataset Plus High-Resolution 
# for region 1803 from the USGS Earth Explorer website and placed them here.

# sierra/landscape/California_Electric_Transmission_lines-shp

# We downloaded the California Electric Transmission Lines and placed them here.

# Please set up the following directories for downloading NEON data using the script
# in tree_mortality/hpc/sierra/download_data along with an access token that can be
# obtained from NEON

# NEON/elev/2013
# NEON/elev/2017
# NEON/elev/2018
# NEON/elev/2019
# NEON/elev/2021
# NEON/chm/2013
# NEON/chm/2017
# NEON/chm/2018
# NEON/chm/2019
# NEON/chm/2021
# NEON/lidar/2013
# NEON/lidar2017
# NEON/lidar/2018
# NEON/lidar/2019
# NEON/lidar/2021
# NEON/spectral/2013
# NEON/spectral/2017
# NEON/spectral/2018
# NEON/spectral/2019
# NEON/spectral/2021

## The remaining folders are used by the algorithm for storing various outputs

# sierra

# Copy the items in tree_mortality/data/training into sierra 

# sierra/spectral

# sierra/spectral/merged_rasters

# sierra/prism

# sierra/landsat_analysis

# sierra/masks



