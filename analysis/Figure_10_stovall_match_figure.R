# This script compares the mean NDVI of trees we labeled live that were labeled dead in Stovall et al. (2019)
# to the distributions of trees both studies labeled as live and both studies labeled as dead. 

library(dplyr)
library(ggplot2)
library(sf)
library(cvms)
library(gridExtra)
library(grid)
library(lattice)
library(cowplot)

# This first data directory is for storing stovall_matches.csv which is the dataset from Stovall et al. 
# appended with each treeID from our study it is matched to and whether or not the tree is contained by our
# matched crown perimeter or not. Because it contains data directly from another study, we do not include it here.
# However, it can be recreated by the following method:

# Download the ALLtrees_v2.csv from https://doi.org/10.6084/m9.figshare.7609193.v2

# Run the scripts on https://github.com/nmschroeder/tree_mortality/hpc/sierra/align_trees entitled
# tree_match_01.sub through tree_match_03.sub on a high performance computer with their corresponding
# R scripts (tree_match_01*.R through tree_match_03*.R). 

# Change the variable stovall_dir to reflect
# where you stored the ALLtrees_v2.csv file from figshare. Change the variable data_dir to reflect where
# you stored trees_2013_rgreen.shp.

# If you prefer to run the script locally, you can try running only tree_match_02*.R and comment out lines 21-22
# line 21:  # stovall_trees <- stovall_trees[idx,] 
# line 22:  # n <- length(idx)

# and add
# n <- dim(stovall_trees)[1]

# to try running the matching algorithm all at once.

vec_dir <- "data/deliverables/vector/"
mask_dir <- "data/intermediate/study_region/"

theme_set(theme_bw(base_size = 10))
stovall_matches <- read.table("../data/stovall_matches.csv")
str(stovall_matches)

ndvi_vals <- read_sf(paste0(vec_dir, "trees_2017_ndvi.shp")) %>% st_drop_geometry() %>% dplyr::select(treeID, mean_ndvi) %>% dplyr::filter(treeID %in% stovall_matches$treeID)

# For the matched dataset, filter for only trees from Stovall (2019) that are contained by one of our trees
stovall_filter <- dplyr::filter(stovall_matches, contain == 1, !is.na(label))
str(stovall_filter)

# How many trees are in the region of interest? Use the polygon from the mask used to make
# the initial study site figure

soap <- st_read(paste0(mask_dir, "soap_mask_polygon.shp"))
teak <- st_read(paste0(mask_dir, "teak_mask_polygon.shp"))

idx <- which(!is.na(stovall_filter$x) & !is.na(stovall_filter$y))
stovall_xy <- stovall_filter[idx,] %>% mutate(xcoord = x, ycoord = y)
stovall_sf <- st_as_sf(x = stovall_xy, coords = c("xcoord", "ycoord")) 

st_crs(stovall_sf) <- st_crs(soap)

# Find which stovall trees intersect our study area
idx <- st_intersects(stovall_sf, soap, sparse = FALSE) | st_intersects(stovall_sf, teak, sparse = FALSE)

stovall_las_intersection <- stovall_sf[idx,]

str(stovall_las_intersection)
unique(stovall_las_intersection$contain)

N_total <- dim(stovall_las_intersection)[1]

matches <- sum(stovall_las_intersection$contain==1)

match_and_label <- sum(stovall_las_intersection$contain==1 & !is.na(stovall_las_intersection$label))

# Remove the ones that are not matched
stovall_las_intersection <- dplyr::filter(stovall_las_intersection, contain == 1)

# Create confusion matrix from these matches
live_stov <- as.integer(!as.logical(stovall_las_intersection$dead))
stovall_las_intersection <- stovall_las_intersection %>% dplyr::mutate(live_stov=live_stov) %>% dplyr::select(-X.1, -X) %>% st_drop_geometry()

cm_prep <- tibble("this_study" = stovall_las_intersection$label, "stovall" = stovall_las_intersection$live_stov)
test_cm <- cm_prep %>% table()

cfm <- as_tibble(test_cm)
plot_confusion_matrix(cfm, target_col = "this_study", prediction_col = "stovall", counts_col = "n")

ratio_incompat <- test_cm[2,1]/(test_cm[2,1] + test_cm[1,1])

check <- sum(sum(test_cm))

# Make a stacked figure with both labeled live and both labeled dead in the first panel, and the mismatches in the second panel.
stovall_las_intersection$live <- as.factor(stovall_las_intersection$label)
stovall_las_intersection$live_stov <- as.factor(stovall_las_intersection$live_stov)
stovall_las_intersection <- right_join(stovall_las_intersection, ndvi_vals, by = "treeID")

# Create the figure
str(stovall_las_intersection)

# Find the indices where Stovall et al. (2019) labeled a tree as live (live_stov = 1)
# in 2016 and we labeled the tree as dead in 2017 (live = 0)
idx <- stovall_las_intersection$live==0 & stovall_las_intersection$live_stov==1

# The above cases are possible (a tree is live in 2016 and dies in 2017), so let's
# remove these from the analysis
stovall_intersection <- stovall_las_intersection[!idx,]

# Next, let's look for a contradictory case: the tree is labeled as dead in 2016
# by Stovall et al. (2019) (live_stov = 0) and live in 2017 by our study (live = 1)
idx2 <- stovall_intersection$live_stov == 0 & stovall_intersection$live == 1

# Let's find the quantiles of the mean NDVI for this contradictory category for 
# 0.5% through 99.5%
xx <- quantile(stovall_intersection$mean_ndvi[idx2], c(0.005, 0.995), na.rm = TRUE)

# Let's find the middle mean NDVI value (for plotting only)
xm <- mean(xx)

# Let's plot the contradictory category in this panel with the quantile information
p01 <- ggplot() + 
  geom_vline(xintercept = xx[1]) +
  geom_vline(xintercept = xx[2]) +
  geom_text(data = data.frame(x=xm, y = 5.5, label = c("99%")), mapping = aes(x = x, y = y, label = label)) +
  geom_segment(aes(x = xm-0.05, y = 5.5, xend = xx[1], yend = 5.5), arrow = arrow(length = unit(0.2, "cm"))) +
  geom_segment(aes(x = xm+0.05, y = 5.5, xend = xx[2], yend = 5.5), arrow = arrow(length = unit(0.2, "cm"))) +
  geom_density(data = stovall_intersection[idx2,], mapping = aes(x = mean_ndvi), fill = "lightgray") + xlim(c(0.1, 1)) + ylim(c(0, 5.8)) + labs(x = "Mean NDVI", y = "Relative Frequency Distribution") + 
  ggtitle("a. Classified as live in this study but dead in Stovall et al.") +
  theme(legend.position="none", plot.title = element_text(size = 10))

# We plot the data where both studies agree (as in not the indices for the contradictory data) as reference data in this panel 
p02 <- ggplot() +
  geom_density(data = stovall_intersection[!idx2,], mapping = aes(x = mean_ndvi, fill = live), alpha = 0.5) + xlim(c(0.1, 1)) + ylim(c(0, 5.8)) + 
  geom_vline(xintercept = xx[1]) +
  geom_vline(xintercept = xx[2]) +
  labs(x = "Mean NDVI", y = "Relative Frequency Distribution") +
  scale_fill_manual(name = "", labels = c("0"="Dead", "1"="Live"), values = c("0" = "khaki1", "1" = "Dodgerblue4")) +
  ggtitle("b. Classified the same in both studies") +
  geom_text(data = data.frame(x=c(0.4, 0.75), y=c(1.5, 1.5), label = c("Dead", "Live")), mapping = aes(x, y, label = label)) +
  theme(legend.position="none", plot.title = element_text(size = 10))

g2 <- plot_grid(p01, p02, ncol = 1, rel_heights = c(1,1))

g2

ggsave("figures/Figure_10_stovall_match.pdf", plot = g2, width = 4, height = 6, unit = "in", dpi = 600)


str(stovall_intersection)
write.csv(stovall_intersection, "../data/stovall_intersection.csv")
