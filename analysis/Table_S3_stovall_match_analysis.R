# This script generates the values for Table S3 which is an analysis of the tree labels in
# Stovall et al. (2019) and our study over the region in which the two studies overlap

library(dplyr)
library(sf)
library(concaveman)
library(ggplot2)

vec_dir <- "data/deliverables/vector/"
mask_dir <- "data/intermediate/study_region/"

# Read in the matched dataset
stovall_matches <- read.table("../data/stovall_matches.csv")
str(stovall_matches)

# How many trees are in the region of interest? Use the polygon from the mask used to make
# the initial study site figure
soap <- st_read(paste0(mask_dir, "soap_mask_polygon.shp"))
teak <- st_read(paste0(mask_dir, "teak_mask_polygon.shp"))

stovall_xy <- stovall_matches %>% mutate(xcoord = x, ycoord = y)
stovall_sf <- st_as_sf(x = stovall_xy, coords = c("xcoord", "ycoord"), crs = 32611) 

N_total <- dim(stovall_matches)[1]

# Find which stovall trees intersect our study area
idx <- st_intersects(stovall_sf, soap, sparse = FALSE) | st_intersects(stovall_sf, teak, sparse = FALSE)
N_intersect <- sum(idx)

stovall_las_intersection <- stovall_sf[idx,]

# Label each entry by site (Soaproot Saddle or Lower Teakettle)
site <- ifelse(stovall_las_intersection$x < 310000, "SOAP", "TEAK")

# Add the site label to the data frame
stovall_las_intersection <- mutate(stovall_las_intersection, site = site)

# Find which Stovall trees were matched to one of our trees
idx <- stovall_las_intersection$contain == 1
sum(idx)

# How many trees were unmatched? Note: Stovall et al. (2019) generally found more trees with
# their method than we did.
idx2 <- stovall_las_intersection$contain == 0
N_unmatched_stovall <- sum(idx2)
N_unmatched_stovall

# Let's draw a tighter polygon around the Stovall trees to see which of our trees are in 
# the same region
stovall_df <- stovall_las_intersection %>% st_as_sf()

# We'll draw a polygon for each of the sites using the concaveman package
overlap_soap <- dplyr::filter(stovall_df, site == "SOAP") %>% concaveman(concavity = 4) 
overlap_teak <- dplyr::filter(stovall_df, site == "TEAK") %>% concaveman(concavity = 4)

# Let's look at the resulting Soaproot Saddle and Lower Teakettle sites for the region of 
# overlap
ggplot() + geom_sf(data = overlap_soap) + geom_sf(data = overlap_teak)

# Let's read in our tree location data
shps <- read_sf("data/deliverables/vector/tree_locations_las_intersection.shp")

# Find which of our trees intersect the region of overlap
idx1 <- st_intersects(shps, overlap_soap, sparse = FALSE) 
idx2 <- st_intersects(shps, overlap_teak, sparse = FALSE)

# Our trees in the Soaproot Saddle overlap region
N_soap_overlap <- sum(idx1)

# and the Lower Teakettle overlap region
N_teak_overlap <- sum(idx2)

# Total for both sites
N_soap_overlap+N_teak_overlap

# Next, let's compute the same for the Stovall trees
idx1_stovall <- stovall_las_intersection$site=="SOAP"
idx2_stovall <- stovall_las_intersection$site=="TEAK"

# How many are in the overlap region at Soaproot Saddle?
N_soap_overlap_stov <- sum(idx1_stovall)

# Lower Teakettle?
N_teak_overlap_stov <- sum(idx2_stovall)

## Mortality fractions in the region of overlap
# Soaproot Saddle
stovall_las_intersection$live_stov <- stovall_las_intersection$dead==0 %>% as.numeric()
1-mean(stovall_las_intersection[idx1_stovall,]$live_stov)

# Lower Teakettle
1-mean(stovall_las_intersection[idx2_stovall,]$live_stov)

# Not every tree in the region of overlap was matched, so let's look for any potential bias
# in the trees that were matched

# Find the indices of the matched trees
idx1_stovall_matched <- stovall_las_intersection$site=="SOAP" & stovall_las_intersection$contain == 1 & !is.na(stovall_las_intersection$label)

idx2_stovall_matched <- stovall_las_intersection$site=="TEAK" & stovall_las_intersection$contain == 1 & !is.na(stovall_las_intersection$label)

# Soaproot Saddle mortality fraction for matched trees
1-mean(stovall_las_intersection[idx1_stovall_matched,]$live_stov)

# Lower Teakettle mortality fraction for matched trees 
1-mean(stovall_las_intersection[idx2_stovall_matched,]$live_stov)

# Find the indices of unmatched trees
idx1_stovall_unmatched <- stovall_las_intersection$site=="SOAP" & stovall_las_intersection$contain == 0 | is.na(stovall_las_intersection$label)

idx2_stovall_unmatched <- stovall_las_intersection$site=="TEAK" & stovall_las_intersection$contain == 0  | is.na(stovall_las_intersection$label)

# Soaproot Saddle mortality fraction for unmatched trees
1-mean(stovall_las_intersection[idx1_stovall_unmatched,]$live_stov)

# Lower Teakettle mortality fraction for unmatched trees
1-mean(stovall_las_intersection[idx2_stovall_unmatched,]$live_stov)

### Our study

## Total mortality fractions by site

# Soaproot Saddle

# Collect the tree identifiers for trees at Sooproot Saddle in the region of overlap
treeIDs_soap <- shps$treeID[idx1]

# Collect the tree identifiers for trees at Lower Teakettle in the region of overlap
treeIDs_teak <- shps$treeID[idx2]

# Convert the live label from our study to the numeric class
stovall_las_intersection$label <- as.numeric(stovall_las_intersection$label)

# Compute the mortality fraction of our trees at Soaproot Saddle that were matched to Stovall trees
1-mean(stovall_las_intersection[idx1_stovall_matched,]$label, na.rm = TRUE)

# Compute the mortality fraction of our trees at Lower Teakettle that were matched to Stovall trees
1-mean(stovall_las_intersection[idx2_stovall_matched,]$label, na.rm = TRUE)

# Notice in the above data frames, there is no column named "live". Our live label
# is described as "label" instead. This still comes from our "live" variable when it is
# assigned in the file hpc/sierra/align_trees/tree_match_02*.R If you accidentally use
# live in the data frame stovall_las_intersection (which is not a column name here), 
# R defaults to live_stov which is the name of the live variable from the Stovall data 
# set (and not our data set).

# In the data frame below, we use the live variable to describe our data set.

# Read in our shape file for trees in 2017 with labels from relative greenness
shps2017 <- read_sf("data/deliverables/vector/trees_2017_rgreen.shp")

# Filter the shape file data frame for trees at the Soaproot Saddle site
shps_soap <- dplyr::filter(shps2017, treeID %in% treeIDs_soap)

# Filter the shape file data frame for trees at the Lower Teakettle site
shps_teak <- dplyr::filter(shps2017, treeID %in% treeIDs_teak)

# Compute the overall mortality fraction of all our trees at Soaproot Saddle
1-mean(shps_soap$live, na.rm = TRUE)

# Compute the overall mortality fraction of all our trees at Lower Teakettle
1-mean(shps_teak$live, na.rm = TRUE)

# Collect the tree identifiers for our trees that were matched to trees from the Stovall study at Soaproot Saddle
treeID_match_soap <- dplyr::filter(stovall_las_intersection, site == "SOAP", contain == 1)$treeID

# Collect the tree identifiers for our trees that were matched to trees from the Stovall study at Lower Teakettle
treeID_match_teak <- dplyr::filter(stovall_las_intersection, site == "TEAK", contain == 1)$treeID

# Filter for trees that were not matched at Soaproot Saddle in the region of overlap between the two studies
shps_soap_unmatched <- dplyr::filter(shps_soap, !(treeID %in% treeID_match_soap))

# Filter for trees that were not matched at Lower Teakettle in the region of overlap between the two studies
shps_teak_unmatched <- dplyr::filter(shps_teak, !(treeID %in% treeID_match_teak))

# Compute the mortality fraction of unmatched trees at Soaproot Saddle for our study in the region of overlap
# between the two studies
1-mean(shps_soap_unmatched$live, na.rm = TRUE)

# Compute the mortality fraction of unmatched trees at Lower Teakettle for our study in the region of overlap
# between the two studies
1-mean(shps_teak_unmatched$live, na.rm = TRUE)

# Compute the total number of matched trees at Soaproot Saddle
N_matched_soap <- sum(idx1_stovall_matched)
N_matched_soap

# Compute the total number of matched trees at Lower Teakettle
N_matched_teak <- sum(idx2_stovall_matched)
N_matched_teak

# Compute the fraction of matched trees in the overlap for our study at Soaproot Saddle
N_matched_soap/N_soap_overlap

# Compute the fraction of matched trees in the overlap for the Stovall et al. (2019) study at Soaproot Saddle
N_matched_soap/N_soap_overlap_stov

# Compute the fraction of matched trees in the overlap for our study at Lower Teakettle
N_matched_teak/N_teak_overlap

# Compute the fraction of matched trees in the overlap for the Stovall et al. (2019) study at Lower Teakettle
N_matched_teak/N_teak_overlap_stov
