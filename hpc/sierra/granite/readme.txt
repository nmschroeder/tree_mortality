# Readme file

# This directory contains scripts for create granite raster tiles and granite shapefiles from NEON canopy 
# height models and spectral data

# Please run the files spectral_01.sub through spectral_04.sub in 
# /tree_mortality/hpc/sierra/spectral 
# before running these files. Note that spectral_05*.sub forward depend on outputs from this directory.

# Please run the files in the following order:

# granite_01.R: creates a text file of NEON canopy height model file paths
# granite_02_water_tiles.R: rasterizes waterbody shapefiles from the National Hydrography Dataset 
# within the extent of each canopy height model tile 
# granite_03_roads.R: computes and rasterizes a 5-meter buffer of road shapefiles 
# granite_04_sr_tiles.R: computes the median luminosity for each pixel between 2017 and 2021 (2013
# is skipped due to missing data)
# granite_05_granite_tiles.R: computes granite tiles from the median luminosity and canopy height model tiles
# granite_06.R: generates a text file of granite tile file paths
# granite_07_granite_polys.R: converts each granite tile to polygons
# granite_08_combine_polygons.R: combines the granite polygons from the previous script
