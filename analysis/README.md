# Estimating Individual Tree Mortality in the Sierra Nevada Using Lidar and Multispectral Reflectance Data
Supporting figure scripts and analyses for Hemming-Schroeder et al. manuscript 

* cumulative_mortality_stovall.R
    + This script reads in the dataset described in Stovall et al. (2019), *Nature Communications*, which can be downloaded on figshare: https://doi.org/10.6084/m9.figshare.7609193.v2
    + This dataset tracks tree mortality over time between 2009 and 2016
    + Then we compute the cumulative tree mortality for the dataset in 2016

* Figure_XX*.R scripts
    + These files generate the corresponding figures in R from Hemming-Schroeder et al. (2023), in revision at *JGR Biogeosciences*
    + Figures not referenced here are created in QGIS or draw.io

* mortality_map_comparison.R
    + This script converts tree mortality point data from Stovall et al. (2019) and our study to a 30-meter grid of tree mortality fraction
    + This data is plotted in Figures 6, 9, S4, and S5 in Hemming-Schroeder et al. (2023)
    
* study_region_outlines.R
    + This script creates the outlines for the study regions shown in Figure 1 from the data masks 

* Tables_1-2_index_accuracies.R
    + Note that the data for Table 3 is calculated within the Figure 10 script
    
* Table_S3_stovall_match_analysis.R
    + The calculations for Table S3 are made here
    + Note that the data for Table S2 is calculated within the Figure 8 scripts

* training_labels_analysis.R
    + This script generates a brief analysis of the training labels including the total number of trees labeled and the numbers of live and dead labels




