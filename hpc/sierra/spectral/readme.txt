# Readme file

# This directory generates raster data products from Landsat surface reflectance data and NEON hyperspectral data
# used in our analysis.

# Files spectral_05*.R through spectral_07*.R depend on output from /tree_mortality/hpc/sierra/granite

# Note that numbered files are typically (but not always) dependent on the previous files with the same prefix. For example, 
# canopy_cover_02.R (which is run by canopy_cover_02.sub) is dependent on the output of canopy_cover_01.R.

# Check the input line numbers for any task array files. For example, in the above directory, you can use
# wc -l cluster_02_inputs.txt
# to check the number of lines that need to be fed into the array file for cluster_02_metrics.sub.

# 1) Generate a simplified set of wavelength bands from the NEON hyperspectral data and
# merge granite and grount tiles generated in /tree_mortality/hpc/sierra/granite/
# spectral_00*.R: generates a text file of sites and years to feed into a later script
# spectral_01*.R: generates a text file of NEON hyperspectral file paths
# spectral_02*.R: creates simplified raster tiles for each hdf5 file downloaded from NEON
# spectral_03*.R: calculates vegetation indices from each raster tile output from the previous script
# spectral_04*.R: creates merged canopy height model raster files from the canopy height model tiles
# spectral_05*.R: creates merged granite raster files from the granite tiles
# spectral_06*.R: creates a tree mask from the canopy height and luminosity tiles (e.g. to cut out snow)
# spectral_07.R: creates a merged ground raster file from the merged canopy height model raster file

# 2) Create rasters to show the change in NDMI between 2013 and 2017 from Landsat surface reflectance data
# dNDMI01.R: generates 2013 and 2017 Landsat median surface reflectance for the tiles temporally nearest September of the
# corresponding year 
# dNMDI02.R: create late summer 2013 and 2017 normalized difference moisture index raster files from Landsat





