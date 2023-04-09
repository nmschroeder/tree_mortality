# Estimating Individual Tree Mortality in the Sierra Nevada Using Lidar and Multispectral Reflectance Data
Supporting scripts and datasets for Hemming-Schroeder et al. (2023), *Journal of Geophysical Research - Biogeosciences*

## analysis directory

Scripts for figures and analysis discussed in the paper

## data directory

### data/deliverables

Data created using the methods in our study 

* data/deliverables/vector
    + tree_locations_las_intersection.shp (note that each shapefile has corresponding .prj, .dbf, and .shx files to help describe the geometries of the .shp dataset)
        - These files describe the locations of the treetop in the dataset that were created by combining the lidar data for all years within the intersection of all the lidar data files. In other words, a tree is only included here if there is lidar (las) data for all years available at the time of this study (2013, 2017, 2018, 2019, and 2021). 
        - treeID is the unique treeID assigned to that tree on the landscape and is consistent across the combined shapefiles and CSV files in this project
        - z is the elevation (meters)
        - ca2013 is the 2013 crown area (sq. meters)
        - zmax2013 is the 2013 tree height (meters)
        - the geometry is a point
        - the coordinate reference system is European Petroleum Survey Group (EPSG) 32611.

    + trees_YYYY_ndvi.shp
        - Not all the columns below are present in each data set
        - treeID is the unique tree ID for each tree in the study
        - x is the Easting value in meters 
        - y is the Northing value in meters
        - xlasXXXX is the Easting value in meters from the lidar file of year XXXX
        - ylasXXXX is the Northing value in meters from the lidar file of year XXXX
        - zmaxXXXX is the height of the tree in meters for the year XXXX
        - mort_area notes whether or not the tree is dead in a given year as classified by change in area where 1 indicates dead by area and 0 indicates not dead by area (may not be the first year the tree is classified as dead)
        - area2013 is the crown area in 2013 in square meters
        - sites is the site label (SOAP or TEAK)
        - poly_area is the crown area in square meters for the year given by YYYY in the filename
        - r_ndvi is the fraction of pixels under the NDVI threshold for the year YYYY
        - mean_ndvi is the mean NDVI of the pixels within the crown perimeter for the year YYYY
        - live is the live (1) or dead (0) label estimate given by the algorithm using NDVI for the year YYYY
        - geometry is the crown perimeter polygon for the tree for the year YYYY

    + trees_YYYY_rgreen.shp
        - These variables have the same meaning as for trees_YYYY_ndvi.shp with the following exceptions:
        - r_green is the fraction of pixels under the relative greenness threshold for the year YYYY
        - mean_green is the mean relative greenness of the pixels within the crown perimeter for the year YYYY
        - live is the live (1) or dead (0) label estimate given by the algorithm using relative greenness for the year YYYY

* data/deliverables/raster
    + Raster files to describe fractional tree mortality for each year aggregated to the 30-meter Landsat grid

### data/intermediate

* data/study_region
    + Raster and shapefiles to describe the outlines of the Soaproot Saddle and Lower Teakettle lidar data intersection for the years 2013, 2017, 2018, 2019, and 2021
    
* data/index_parameters
    + Results from the grid parameter searches to find the best vegetation indices using the training and validation datasets

### data/training

Shapefiles of tree crown perimeters that were labeled by hand as dead or alive in 2017 in QGIS using RGB imagery derived from NEON hyperspectral data

## figures directory

Figures generated in R are stored here

## hpc/sierra directory

Files for this project created for use on a high performance computing cluster to create the data sets in data/deliverables
