#!/usr/bin/env Rscript

# Compute and plot mortality fraction accuracy for a grid search of crown fraction and vegetation index thresholds for
# Figure 3 in the manuscript

# We also plot the mortality fraction for the same grid search to explore the sensitivity of the mortality fraction to
# the parameters

require(sp)
require(Rcpp)
library(raster)
library(sf)
library(dplyr)
library(exactextractr)
library(tidyr)
library(ggplot2)
library(pracma)
library(cowplot)
library(gridExtra)

theme_set(theme_bw(base_size = 12))

# Set working directory

# Relative greenness

# Read in NDVI indices
rgreen_indices <- read.csv("data/intermediate/index_parameters/rgreen_indices_labels_train.csv")

# Determine the highest accuracy reading
idx <- which.max(rgreen_indices$acc)
optim_params_rgreen <- data.frame(rgreen = rgreen_indices$rgreen[idx], prop = rgreen_indices$prop[idx])

print("Statistics, training dataset:")
rgreen_indices[idx,]

print("Statistics, testing dataset:")
rgreen_indices_valid <- read.csv("data/intermediate/index_parameters/rgreen_indices_labels_valid.csv")
rgreen_indices_valid

total <- rgreen_indices$total[idx] + rgreen_indices_valid$total
print("Total trees")
total

# Create plot to analyze the proportion of dead trees using different Parameters
rgreen_plot <- ggplot(data = rgreen_indices) + geom_tile(mapping = aes(x = rgreen, y = prop, fill = rdead)) +
  xlim(c(-0.1, 1.1)) + ylim(c(-0.1, 1.1)) +
  labs(y = "Crown fraction", x = "Relative greenness") +
  scale_fill_viridis_c(option = "C", name = "Proportion of dead trees") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_fixed()
rgreen_plot
# ggsave("rgreen_parameter_labels.png", dpi = 600, width = 3, height = 3, unit = "in")

# Plot the accuracy for different proposed crown fractions and relative greenness thresholds
rgreen_plot2 <- ggplot(data = rgreen_indices) + 
  geom_raster(mapping = aes(x = rgreen, y = prop, fill = acc)) + 
  geom_point(data = optim_params_rgreen, mapping = aes(x = rgreen, y = prop), shape = 3, size = 2) +
  #xlim(c(-0.1, 1.1)) + ylim(c(-0.1, 1.1)) +
  labs(y = "Crown fraction", x = "Relative Greenness") +
  scale_fill_viridis_c(option = "D", name = "Accuracy", begin = 0, end = 1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  coord_fixed()
rgreen_plot2

# Read in NDVI indices
ndvi_indices <- read.csv("data/intermediate/index_parameters/ndvi_indices_labels_train.csv")

# Determine the highest accuracy reading
idx <- which.max(ndvi_indices$acc)
optim_params_ndvi <- data.frame(ndvi = ndvi_indices$ndvi[idx], prop = ndvi_indices$prop[idx])

print("Statistics, training dataset:")
ndvi_indices[idx,]

print("Statistics, testing dataset:")
ndvi_indices_valid <- read.csv("data/intermediate/index_parameters/ndvi_indices_labels_valid.csv")
ndvi_indices_valid

total <- ndvi_indices$total[idx] + ndvi_indices_valid$total
print("Total trees")
total

# Create plot to display the mortality fraction for different NDVI-based thresholds
ndvi_plot <- ggplot(data = ndvi_indices) + geom_tile(mapping = aes(x = ndvi, y = prop, fill = rdead)) +
  xlim(c(-0.1, 1.1)) + ylim(c(-0.1, 1.1)) +
  labs(y = "Crown fraction", x = "NDVI") +
  scale_fill_viridis_c(option = "C", name = "Proportion of dead trees") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_fixed()
ndvi_plot

# Plot the accuracy for different proposed crown fractions and NDVI thresholds
ndvi_plot2 <- ggplot(data = ndvi_indices) + 
  geom_raster(mapping = aes(x = ndvi, y = prop, fill = acc)) + 
  geom_point(data = optim_params_ndvi, mapping = aes(x = ndvi, y = prop), shape = 3, size = 2) +
  #xlim(c(-0.1, 1.1)) + ylim(c(-0.1, 1.1)) +
  labs(y = "Crown fraction", x = "NDVI") +
  scale_fill_viridis_c(option = "D", name = "Accuracy", begin = 0, end = 1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_fixed() +
  theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ndvi_plot2

# Plot the two grid searches and the mortality accuracy for relative greenness and NDVI (Figure 3 in the manuscript)
plot_grid(rgreen_plot2, ndvi_plot2, leg, labels = c("a", "b", ""), axis = "tl", rel_widths = c(1, 1, 0.3), label_fontface = "plain", align = "h", nrow = 1)
ggsave("figures/Figure_3_rgreen_ndvi_panel.eps", width = 6.5, height = 2.5, unit = "in")

