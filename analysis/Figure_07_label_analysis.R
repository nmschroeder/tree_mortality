# This script analyzes the mean relative greenness and NDVI distributions of the tree mortality
# dataset created in Hemming-Schroeder et al. (2023). 

library(dplyr)
library(ggplot2)
library(cowplot)
library(sf)
library(units)
library(gridExtra)

theme_set(theme_bw(base_size = 12))

# Read in the labeled data frame
fdata <- read.csv("data/deliverables/vector/feature_vars_labels.csv")
str(fdata)

# Remove any trees that could not be labeled
idx <- !is.na(fdata$live)
fdata <- fdata[idx,]
str(fdata)

# Make the live column a factor variable class for plotting
fdata$live <- as.factor(fdata$live)

# Collect the mean NDVI variable as well
anc_data <- read_sf("data/deliverables/vector/trees_2017_ndvi.shp")
str(anc_data)

# Remove the geometries from the data frame
anc_data <- anc_data %>% st_drop_geometry()

# Select the unique tree identifier (treeID) and the mean NDVI inside the crown perimeter
add_data <- dplyr::select(anc_data, treeID, mean_ndvi)

# Filter out trees that are not in the feature data frame (e.g. because they have no live or dead variable)
add_data <- dplyr::filter(add_data, treeID %in% fdata$treeID)

# Join the feature data frame with the data frame which includes the mean NDVI
fdata <- right_join(fdata, add_data, by = "treeID")

# Plot the distributions of mean relative greenness for live and dead trees
p1 <- ggplot(data = fdata) + geom_density(mapping = aes(x = mean_green, fill = live), alpha = 0.5) + xlim(c(0.25, 0.55)) + labs(x = "Mean relative greenness", y = "Probability Density Function") + 
  scale_fill_manual(name = "", labels = c("0"="Dead", "1"="Live"), values = c("0" = "khaki1", "1" = "Dodgerblue4")) +
  ggtitle("a. Relative Greenness") +
  theme(legend.direction="horizontal")

# Collect the live/dead legend
legend <- get_legend(p1)

# Remove the legend from panel 1
p1 <- p1 + theme(legend.position = "none")

# Plot the distributions of mean NDVI for live and dead trees
p2 <- ggplot(data = fdata) + geom_density(mapping = aes(x = mean_ndvi, fill = live), alpha = 0.5) + xlim(c(0, 1)) +
  labs(x = "Mean NDVI", y = "Probability Density Function") + 
  scale_fill_manual(name = "", labels = c("0"="Dead", "1"="Live"), values = c("0" = "khaki1", "1" = "Dodgerblue4"))+
  ggtitle("b. NDVI") +
  theme(legend.position="none")

# Arrange the figures in a plot with the legend
g1 <- grid.arrange(p1, p2, legend, ncol=1, nrow = 3, 
                  layout_matrix = rbind(c(1,1,1), c(2,2,2), c(3,3,3)),
                  heights = c(2.7, 2.7, 0.5))
g1

# Save the figure
ggsave("figures/Figure_7_label_analysis.pdf", plot = g1, width = 3.5, height = 7, unit = "in", dpi = 600)

# Calculate the vegetation index value where the distribution reaches a maximum for

# 1) Mean relative greenness for dead trees
mx <- which.max(density(filter(fdata, live ==0, !is.na(mean_green))$mean_green)$y)
xval_rgreen_dead <- density(filter(fdata, live ==0, !is.na(mean_green))$mean_green)$x[mx]
xval_rgreen_dead

# 2) Mean relative greenness for live trees
mx <- which.max(density(filter(fdata, live ==1)$mean_green)$y)
xval_rgreen_live <- density(filter(fdata, live ==1)$mean_green)$x[mx]
xval_rgreen_live

# 3) Mean NDVI for dead trees
mx <- which.max(density(filter(fdata, live ==0, !is.na(mean_ndvi))$mean_ndvi)$y)
xval_ndvi_dead <- density(filter(fdata, live ==0, !is.na(mean_green))$mean_ndvi)$x[mx]
xval_ndvi_dead

# 4) Mean NDVI for live trees
mx <- which.max(density(filter(fdata, live ==1)$mean_ndvi)$y)
xval_ndvi_live <- density(filter(fdata, live ==1)$mean_ndvi)$x[mx]
xval_ndvi_live

