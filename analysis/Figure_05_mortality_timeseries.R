# This script creates Figure 5 which displays the cumulative tree mortality for Soaproot Saddle, 
# Lower Teakettle, and the combined dataset for all years of available NEON data between 2013 and 2021.
# We also calculate summaries for the total tree mortality at each site and the percent mortality within
# 2020-2021 wildfire perimeters in the final portion of the script.

library(dplyr)
library(sf)
library(ggplot2)
library(cowplot)
theme_set(theme_bw(base_size = 10))

# To run this script, please download the National USFS Final Fire Perimeter dataset from 
# https://data.fs.usda.gov/geodata/edw/datasets.php, and store the directory name where you have the dataset
# saved in the fire_perim variable below
fire_perim <- "~/Data/S_USA/"

data_dir <- "data/deliverables/vector/"

# Read in wildfire perimeters from USFS
wildfires_all <- read_sf(paste0(fire_perim, "S_USA.FinalFirePerimeter.shp"))
wildfires <- dplyr::filter(wildfires_all, UNIQFIREID %in% c("2021-CASNF-000717", "2020-CASNF-001391"))
st_write(wildfires, "../data/wildfires.shp", delete_dsn = TRUE)

# Check
ggplot() + geom_sf(data = wildfires)

# Read in mortality data by year
shps_neon_2013 <- read_sf(paste0(data_dir, "trees_2013_rgreen.shp")) %>% st_as_sf(crs = 32611)
shps_neon_2017 <- read_sf(paste0(data_dir, "trees_2017_rgreen.shp")) %>% st_as_sf(crs = 32611)
shps_neon_2018 <- read_sf(paste0(data_dir, "trees_2018_rgreen.shp")) %>% st_as_sf(crs = 32611)
shps_neon_2019 <- read_sf(paste0(data_dir, "trees_2019_rgreen.shp")) %>% st_as_sf(crs = 32611)
shps_neon_2021 <- read_sf(paste0(data_dir, "trees_2021_rgreen.shp")) %>% st_as_sf(crs = 32611)
shps_locs <- read_sf(paste0(data_dir, "tree_locations_las_intersection.shp")) %>% st_as_sf(crs = 32611)
neon_years <- c(2013, 2017, 2018, 2019, 2021)

# Check to make sure all trees with an area less than 1 sq. meter in 2013 have been removed
area2013 <- st_area(shps_neon_2013) %>% as.numeric()
check <- sum(area2013<1)
check

# Here, we are filtering for crown area greater than 1 sq. meter in 2013, height in 2013 greater than 5 meters, and to ensure that
# there is spectral data for 2017, 2018, 2019, and 2021 (many missing strips in 2013)
idx <- which(shps_neon_2017$zmax2013>=5 & !is.na(shps_neon_2017$live) & !is.na(shps_neon_2018$live) & !is.na(shps_neon_2019$live) & !is.na(shps_neon_2021$live))
treeIDs <- shps_neon_2017$treeID[idx]

# Total number of trees that we have to work with
N_total <- length(treeIDs)

# Filter for only these trees
shps_neon_2013 <- dplyr::filter(shps_neon_2013, treeID %in% treeIDs)
shps_neon_2017 <- dplyr::filter(shps_neon_2017, treeID %in% treeIDs)
shps_neon_2018 <- dplyr::filter(shps_neon_2018, treeID %in% treeIDs)
shps_neon_2019 <- dplyr::filter(shps_neon_2019, treeID %in% treeIDs)
shps_neon_2021 <- dplyr::filter(shps_neon_2021, treeID %in% treeIDs)

# Check: are any trees that are labeled as dead in 2019 labeled as alive in 2021?
treeIDs2019 <- dplyr::filter(shps_neon_2019, live==0)$treeID

test <- dplyr::filter(shps_neon_2021, treeID %in% treeIDs2019) %>% dplyr::filter(live == 1)
# yes; there are some errors

# Check to make sure the dimension did not change
check <- dim(shps_neon_2017)[1] == N_total
check

# Initialize arrays for the mortality plot
years <- seq(2013, 2021)
ratio_dead <- years*NA
total_dead <- years*NA
dead_by_area <- years*NA
ratio_dead_soap <- years*NA
total_dead_soap <- years*NA
dead_by_area_soap <- years*NA
ratio_dead_teak <- years*NA
total_dead_teak <- years*NA
dead_by_area_teak <- years*NA

# 2013
# How many trees are we able to consider in 2013? 
N_total_2013 <- sum(!is.na(shps_neon_2013$live))
N_total_2013
ratio_dead[years == 2013] <- mean(shps_neon_2013$live==0, na.rm = TRUE)
total_dead[years == 2013] <- sum(shps_neon_2013$live == 0, na.rm = TRUE)

ratio_dead_soap[years == 2013] <- mean(dplyr::filter(shps_neon_2013, sites == "SOAP")$live==0, na.rm = TRUE)
total_dead_soap[years == 2013] <- sum(dplyr::filter(shps_neon_2013, sites == "SOAP")$live==0, na.rm = TRUE)

ratio_dead_teak[years == 2013] <- mean(dplyr::filter(shps_neon_2013, sites == "TEAK")$live==0, na.rm = TRUE)
total_dead_teak[years == 2013] <- sum(dplyr::filter(shps_neon_2013, sites == "TEAK")$live==0, na.rm = TRUE)

# 2017

# Store the fraction of dead trees, the sum of dead trees and the portion labeled dead by area
ratio_dead[years == 2017] <- mean(shps_neon_2017$live==0)
total_dead[years == 2017] <- sum(shps_neon_2017$live==0)
dead_by_area[years == 2017] <- sum(shps_neon_2017$mort_area==1)

ratio_dead_soap[years == 2017] <- mean(dplyr::filter(shps_neon_2017, sites == "SOAP")$live==0)
total_dead_soap[years == 2017] <- sum(dplyr::filter(shps_neon_2017, sites == "SOAP")$live==0)
dead_by_area_soap[years == 2017] <- sum(dplyr::filter(shps_neon_2017, sites == "SOAP")$mort_area==1)

ratio_dead_teak[years == 2017] <- mean(dplyr::filter(shps_neon_2017, sites == "TEAK")$live==0)
total_dead_teak[years == 2017] <- sum(dplyr::filter(shps_neon_2017, sites == "TEAK")$live==0)
dead_by_area_teak[years == 2017] <- sum(dplyr::filter(shps_neon_2017, sites == "TEAK")$mort_area==1)


# 2018
ratio_dead[years == 2018] <- mean(shps_neon_2018$live==0)
total_dead[years == 2018] <- sum(shps_neon_2018$live==0)
dead_by_area[years == 2018] <- sum(shps_neon_2018$mort_area==1)

ratio_dead_soap[years == 2018] <- mean(dplyr::filter(shps_neon_2018, sites == "SOAP")$live==0)
total_dead_soap[years == 2018] <- sum(dplyr::filter(shps_neon_2018, sites == "SOAP")$live==0)
dead_by_area_soap[years == 2018] <- sum(dplyr::filter(shps_neon_2018, sites == "SOAP")$mort_area==1)

ratio_dead_teak[years == 2018] <- mean(dplyr::filter(shps_neon_2018, sites == "TEAK")$live==0)
total_dead_teak[years == 2018] <- sum(dplyr::filter(shps_neon_2018, sites == "TEAK")$live==0)
dead_by_area_teak[years == 2018] <- sum(dplyr::filter(shps_neon_2018, sites == "TEAK")$mort_area==1)

# 2019
ratio_dead[years == 2019] <- mean(shps_neon_2019$live==0)
total_dead[years == 2019] <- sum(shps_neon_2019$live==0)
dead_by_area[years == 2019] <- sum(shps_neon_2019$mort_area==1)

ratio_dead_soap[years == 2019] <- mean(dplyr::filter(shps_neon_2019, sites == "SOAP")$live==0)
total_dead_soap[years == 2019] <- sum(dplyr::filter(shps_neon_2019, sites == "SOAP")$live==0)
dead_by_area_soap[years == 2019] <- sum(dplyr::filter(shps_neon_2019, sites == "SOAP")$mort_area==1)

ratio_dead_teak[years == 2019] <- mean(dplyr::filter(shps_neon_2019, sites == "TEAK")$live==0)
total_dead_teak[years == 2019] <- sum(dplyr::filter(shps_neon_2019, sites == "TEAK")$live==0)
dead_by_area_teak[years == 2019] <- sum(dplyr::filter(shps_neon_2019, sites == "TEAK")$mort_area==1)

# 2021

# Find trees that were alive in 2019 and died within the wildfire perimeters by 2021

# Transform the wildfire polygons to the same CRS as the tree locations
wildfires <- st_transform(wildfires, crs = 32611)

# Collect the tree IDs of trees that died by 2019
treeIDs2019 <- dplyr::filter(shps_neon_2019, live==0)$treeID

# Filter for trees which died between 2019 and 2021 (i.e. dead in 2021 and not already dead in 2019)
treeIDs_2019_2021 <- dplyr::filter(shps_neon_2021, live==0 & !(treeID %in% treeIDs2019))

# Check to make sure none of the trees were labeling as dead between 2019 and 2021 were already labeled
# dead in 2019
any(treeIDs_2019_2021$treeID %in% treeIDs2019)

# Filter the locations of trees that died between 2019 and 2021
trees_dead_2019_2021 <- filter(shps_locs, treeID %in% treeIDs_2019_2021$treeID)

# Each element of this matrix indicates whether a tree (rows) is located in a given wildfire (columns)
tf_matrix <- st_intersects(trees_dead_2019_2021, wildfires, sparse = FALSE)

# Number of trees
n <- dim(tf_matrix)[1]

# Initialize vector to note if a tree is located within any of the wildfires of interest (Creek and Blue here)
tf_wildfire <- vector(length = n)
# For each tree
for (i in 1:n){
  # Is the tree top located within any wildfire perimeter?
  tf_wildfire[i] <- any(tf_matrix[i,])
}

# Note the tree IDs which fall within a wildfire of interest
treeIDsWildfire <- trees_dead_2019_2021$treeID[tf_wildfire]

# Each element of this matrix indicates whether a tree (rows) is located in a given wildfire (columns)
tf_matrix_all <- st_intersects(shps_locs, wildfires, sparse = FALSE)
treeIDsBlue <- shps_locs$treeID[tf_matrix_all[,1]]
treeIDsCreek <- shps_locs$treeID[tf_matrix_all[,2]]

# How many trees died between 2019 and 2021 in the Creek Fire and Blue Fire?

# a) How many were already dead in 2019?
treeIDs_already_dead <- dplyr::filter(shps_neon_2019, live == 0)$treeID
N_already_dead_blue <- sum(treeIDs_already_dead %in% treeIDsBlue)
N_already_dead_blue

# b) Total trees within Blue Fire perimeter
length(treeIDsBlue)

# c) Total trees which died in Blue Fire
N_died_blue_fire <- (treeIDsWildfire %in% treeIDsBlue) %>% sum()
N_died_blue_fire

(N_died_blue_fire + N_already_dead_blue)/length(treeIDsBlue)

# d) Total trees within Creek Fire perimeter
length(treeIDsCreek)
N_died_creek_fire <- (treeIDsWildfire %in% treeIDsCreek) %>% sum()
N_died_creek_fire
N_already_dead_creek <- sum(treeIDs_already_dead %in% treeIDsCreek)
N_already_dead_creek
(N_died_creek_fire + N_already_dead_creek)/length(treeIDsCreek)

# e) How many already dead before wildfires?
N_total_dead_before_wildfire <- N_already_dead_creek + N_already_dead_blue
N_total_dead_before_wildfire

# f) How many dead between 2019 and 2021 within wildfire perimeters?
N_wildfire <- N_died_creek_fire + N_died_blue_fire
N_wildfire

# g) By what percent did the total mortality increase?
N_wildfire/N_total_dead_before_wildfire

# Compute the fraction dead, total dead, and number of trees noted dead by change in area (cumulative)
ratio_dead[years == 2021] <- mean(shps_neon_2021$live==0)
total_dead[years == 2021] <- sum(shps_neon_2021$live==0)
dead_by_area[years == 2021] <- sum(shps_neon_2021$mort_area==1)

ratio_dead_soap[years == 2021] <- mean(dplyr::filter(shps_neon_2021, sites == "SOAP")$live==0)
total_dead_soap[years == 2021] <- sum(dplyr::filter(shps_neon_2021, sites == "SOAP")$live==0)
dead_by_area_soap[years == 2021] <- sum(dplyr::filter(shps_neon_2021, sites == "SOAP")$mort_area==1)

ratio_dead_teak[years == 2021] <- mean(dplyr::filter(shps_neon_2021, sites == "TEAK")$live==0)
total_dead_teak[years == 2021] <- sum(dplyr::filter(shps_neon_2021, sites == "TEAK")$live==0)
dead_by_area_teak[years == 2021] <- sum(dplyr::filter(shps_neon_2021, sites == "TEAK")$mort_area==1)

tree_data <- data.frame(years = years, ratio_dead = ratio_dead)
ggplot(data = tree_data) + geom_col(mapping = aes(x = years, y = ratio_dead)) + 
  scale_x_discrete(limits = years) +
  ylab("Cumulative mortality fraction") +
  xlab("Year")
#ggsave("figures/cumulative_mortality_ratio_neon.eps", width = 6, height = 4)
ratio_dead_wildfire <- rep(NA, length(years))
ratio_dead_wildfire[length(years)] <- mean(shps_neon_2021$treeID %in% treeIDsWildfire)

# Add a new category to represent unburned trees
tree_data <- mutate(tree_data, other = 1)
idx <- which(is.na(tree_data$ratio_dead))
tree_data_wildfire <- data.frame(years = years, ratio_dead = ratio_dead_wildfire, other = 0)
tree_data$ratio_dead <- rowSums(cbind(tree_data$ratio_dead, -1*tree_data_wildfire$ratio_dead), na.rm = TRUE)
tree_data$ratio_dead[idx] <- NA

# Combine the non-wildfire and wildfire datasets together
tree_data_wildfire <- rbind.data.frame(tree_data, tree_data_wildfire)
tree_data_wildfire$other <- as.factor(tree_data_wildfire$other)

# Create percent labels for the proportion of trees that died inside and outside wildfire perimeters
tree_data_wildfire$perc <- round(tree_data_wildfire$ratio_dead*100,1) 
idx <- which(!is.na(tree_data_wildfire$perc))
tree_data_wildfire$perc[idx] <- tree_data_wildfire$perc[idx] %>% format(nsmall = 1) %>% as.character() %>% paste0("%")

# These next few lines help to format the percent labels on the figure
dy <- 0.02
tree_data_wildfire$yloc <- rep(dy, dim(tree_data_wildfire)[1])
locval <- filter(tree_data_wildfire, years == 2021 & other == 1)$ratio_dead
tree_data_wildfire$yloc[length(tree_data_wildfire$yloc)] <- locval+dy

# Choose a custom color palette
mycolors <- c('khaki3','lightblue4')
labelcolors <- c('black', 'white')    
p1 <- ggplot(data = tree_data_wildfire) + geom_col(mapping = aes(x = years, y = ratio_dead, fill = other), position = "stack", show.legend = FALSE) + 
  geom_text(data = tree_data_wildfire, mapping = aes(x = years, y = yloc, label = perc, color = other), size = 2.5, show.legend = FALSE) +
  scale_x_discrete(limits = as.integer(years)) +
  scale_fill_manual(values = mycolors) +
  scale_color_manual(values = labelcolors) +
  ylim(c(-0.02, 0.65)) +
  ylab("Cumulative mortality fraction") +
  xlab("Year") +
  ggtitle("c. Combined")
#ggsave("figures/cumulative_mortality_ratio.eps", width = 6.5, height = 4)


ratio_dead_wildfire <- rep(0, length(years))
ratio_dead_wildfire[length(years)] <- mean(shps_neon_2021$treeID %in% treeIDsWildfire)

# Plot the fraction of tree mortality by site to show elevation differences
tree_data_soap <- data.frame(years = years, ratio_dead = ratio_dead_soap, site = "Soaproot Saddle")
tree_data_teak <- data.frame(years = years, ratio_dead = ratio_dead_teak, site = "Lower Teakettle")
tree_data_combined <- data.frame(years = years, ratio_dead = ratio_dead, site = "Combined")
tree_data_panel <- rbind.data.frame(tree_data_soap, tree_data_teak, tree_data_combined)

# Choose a custom color palette
mypanelcolors <- c('black', 'lightblue4', 'khaki2')
ggplot(data = tree_data_panel) + geom_col(mapping = aes(x = years, y = ratio_dead, fill = site), 
                                          color = "black", position = 'dodge') +
  scale_x_discrete(limits = as.integer(years)) +
  scale_fill_manual(name = "", values = mypanelcolors) +
  ylab("Cumulative mortality fraction") +
  xlab("Year") 
  theme(legend.position="bottom")

# Save to the figure directory
#ggsave("figures/Figure_5_v2_cumulative_mortality_ratio_by_site.eps", width = 6.5, height = 4)
  
## Adding Soaproot Saddle and Lower Teakettle Panels to original stacked figure instead of dodged figure

# Soaproot Saddle
ratio_dead_wildfire_soap <- rep(NA, length(years))
ratio_dead_wildfire_soap[length(years)] <- mean(dplyr::filter(shps_neon_2021, sites == "SOAP")$treeID %in% treeIDsWildfire)

# Add a new category to represent unburned trees
tree_data_soap <- mutate(tree_data_soap, other = 1) %>% dplyr::select(-site)
idx <- which(is.na(tree_data_soap$ratio_dead))
tree_data_wildfire_soap <- data.frame(years = years, ratio_dead = ratio_dead_wildfire_soap, other = 0)
tree_data_soap$ratio_dead <- rowSums(cbind(tree_data_soap$ratio_dead, -1*tree_data_wildfire_soap$ratio_dead), na.rm = TRUE)
tree_data_soap$ratio_dead[idx] <- NA

# Combine the non-wildfire and wildfire datasets together
tree_data_wildfire_soap <- rbind.data.frame(tree_data_soap, tree_data_wildfire_soap)
tree_data_wildfire_soap$other <- as.factor(tree_data_wildfire_soap$other)

# Create percent labels for the proportion of trees that died inside and outside wildfire perimeters
tree_data_wildfire_soap$perc <- round(tree_data_wildfire_soap$ratio_dead*100,1) 
idx <- which(!is.na(tree_data_wildfire_soap$perc))
tree_data_wildfire_soap$perc[idx] <- tree_data_wildfire_soap$perc[idx] %>% format(nsmall = 1) %>% as.character() %>% paste0("%")

# These next few lines help to format the percent labels on the figure
dy <- 0.02
tree_data_wildfire_soap$yloc <- rep(dy, dim(tree_data_wildfire_soap)[1])
locval <- filter(tree_data_wildfire_soap, years == 2021 & other == 1)$ratio_dead
tree_data_wildfire_soap$yloc[length(tree_data_wildfire_soap$yloc)] <- locval+dy

# Choose a custom color palette
mycolors <- c('khaki3','lightblue4')
labelcolors <- c('black', 'white')    
p2 <- ggplot(data = tree_data_wildfire_soap) + geom_col(mapping = aes(x = years, y = ratio_dead, fill = other), position = "stack", show.legend = FALSE) + 
  geom_text(data = tree_data_wildfire_soap, mapping = aes(x = years, y = yloc, label = perc, color = other), size = 2.5, show.legend = FALSE) +
  scale_x_discrete(limits = as.integer(years)) +
  #ylim(c(-0.02, 0.65)) +
  scale_fill_manual(values = mycolors) +
  scale_color_manual(values = labelcolors) +
  ylab("Cumulative mortality fraction") +
  xlab("Year") + 
  ggtitle("a. Soaproot Saddle")
p2

# Lower Teakettle 
ratio_dead_wildfire_teak <- rep(NA, length(years))

# Add a new category to represent unburned trees
tree_data_teak <- mutate(tree_data_teak, other = 1) %>% dplyr::select(-site)
idx <- which(is.na(tree_data_teak$ratio_dead))
tree_data_wildfire_teak <- data.frame(years = years, ratio_dead = ratio_dead_wildfire_teak, other = 0)
tree_data_teak$ratio_dead <- rowSums(cbind(tree_data_teak$ratio_dead, -1*tree_data_wildfire_teak$ratio_dead), na.rm = TRUE)
tree_data_teak$ratio_dead[idx] <- NA

# Combine the non-wildfire and wildfire datasets together
tree_data_wildfire_teak <- rbind.data.frame(tree_data_teak, tree_data_wildfire_teak)
tree_data_wildfire_teak$other <- as.factor(tree_data_wildfire_teak$other)

# Create percent labels for the proportion of trees that died inside and outside wildfire perimeters
tree_data_wildfire_teak$perc <- round(tree_data_wildfire_teak$ratio_dead*100,1) 
idx <- which(!is.na(tree_data_wildfire_teak$perc))
tree_data_wildfire_teak$perc[idx] <- tree_data_wildfire_teak$perc[idx] %>% format(nsmall = 1) %>% as.character() %>% paste0("%")

# These next few lines help to format the percent labels on the figure
dy <- 0.02
tree_data_wildfire_teak$yloc <- rep(dy, dim(tree_data_wildfire_teak)[1])
locval <- filter(tree_data_wildfire_teak, years == 2021 & other == 1)$ratio_dead
tree_data_wildfire_teak$yloc[length(tree_data_wildfire_teak$yloc)] <- locval+dy

# Choose a custom color palette
mycolors2 <- c('lightblue4', 'khaki3')
labelcolors2 <- c('black', 'white')    
p3 <- ggplot(data = tree_data_wildfire_teak) + geom_col(mapping = aes(x = years, y = ratio_dead, fill = other), position = "stack", show.legend = FALSE) + 
  geom_text(data = tree_data_wildfire_teak, mapping = aes(x = years, y = yloc, label = perc, color = other), size = 2.5, show.legend = FALSE) +
  scale_x_discrete(limits = as.integer(years)) +
  scale_fill_manual(values = mycolors2) +
  ylim(c(-0.02, 0.65)) +
  scale_color_manual(values = labelcolors2) +
  ylab("Cumulative mortality fraction") +
  xlab("Year") +
  ggtitle("b. Lower Teakettle")
p3

panel_plot <- plot_grid(p2, p3, p1, nrow = 3)
panel_plot
ggsave("figures/Figure_5_panel.pdf", width = 4, height = 8, unit = "in")

# Tree mortality summary:

# How many trees were dead in 2017?
total_dead[years==2017]

# What proportion of trees were dead in 2017 overall?
total_dead[years==2017]/N_total

# How many trees died by 2017 at each site?
N_dead_teak <- total_dead_teak[years==2017]
N_dead_soap <- total_dead_soap[years==2017]

# What percentage of trees were dead in 2017 at Soaproot Saddle?
N_dead_soap/N_soap

# What percentage of trees were dead in 2017 at Lower Teakettle
N_dead_teak/N_teak

# Let's double-check our wildfire calculations from above (Lines 167-200) by calculating
# some of these totals a second way
N_wildfire <- length(treeIDsWildfire)
N_soap <- length(dplyr::filter(shps_neon_2021, sites == "SOAP")$treeID)
N_teak <- length(dplyr::filter(shps_neon_2021, sites == "TEAK")$treeID)
check <- N_soap + N_teak == N_total
check

# How many trees died within wildfire perimeters between 2019 and 2021?
N_wildfire

# What is the fraction of trees that died in wildfire burn perimeters between 2019 and 2021?
wildfire_fraction <- N_wildfire/N_total
wildfire_fraction

# The two wildfires were at Soaproot Saddle, so we can calculate the proportion of trees
# that died within the wildfire perimeters as follows:
wildfire_fraction_soap <- N_wildfire/N_soap
wildfire_fraction_soap

# How many total trees are located within the Blue Fire perimeter?
N_blue <- sum(tf_matrix_all[,1])

# The Creek Fire?
N_creek <- sum(tf_matrix_all[,2])

# Determine the number of dead trees within the Blue Fire perimeter in 2019 (before the Blue Fire)
N_dead_blue_2019 <- length(dplyr::filter(shps_neon_2019, treeID %in% treeIDsBlue, live==0)$treeID)

# Calculate the fraction of dead trees within the Blue Fire perimeter in 2019 (before the Blue Fire)
N_dead_blue_2019/N_blue

# Determine the number of dead trees within the Blue Fire perimeter in 2021 after the 2021 Blue Fire
N_dead_blue_2021 <- length(dplyr::filter(shps_neon_2021, treeID %in% treeIDsBlue, live==0)$treeID)

# Calculate the fraction of dead trees after the 2021 Blue Fire within the fire perimeter
N_dead_blue_2021/N_blue

# Calculate the increase in mortality between 2019 and 2021 within the Blue Fire perimeter
N_dead_blue_2021/N_dead_blue_2019

# Repeat for the Creek Fire:

# Determine the number of dead trees within the Creek Fire perimeter in 2019 (before the Creek Fire)
N_dead_creek_2019 <- length(dplyr::filter(shps_neon_2019, treeID %in% treeIDsCreek, live==0)$treeID)

# Calculate the fraction of dead trees in the Creek Fire perimeter in 2019 (before the Creek Fire)
N_dead_creek_2019/N_creek

# Determine the number of dead trees within the Creek Fire perimeter in 2021 (after the Creek Fire)
N_dead_creek_2021 <- length(dplyr::filter(shps_neon_2021, treeID %in% treeIDsCreek, live==0)$treeID)

# Determine the fraction of dead trees within the Creek Fire perimeter in 2021 (after the Creek Fire)
N_dead_creek_2021/N_creek

# Calculate the increase in mortality between 2019 and 2021 within the Creek Fire perimeter
N_dead_creek_2021/N_dead_creek_2019


