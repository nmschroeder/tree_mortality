# This script creates the Soaproot Saddle portion of Figure 8. The file Figure_08_part2*.R creates
# the Lower Teakettle plots and puts the two figure objects together to create and save Figure 8.

# For each driver variable explored in the paper, we set a domain for the variable wherein we would expect to 
# see a related impact on mortality. For example, for distance from rivers, we limit the domain from 0 to 500 meters.

# We generate 20 histogram bins for the domain and filter out any bins with fewer than 50 trees. Then, we fit
# a line to the data to estimate the slope and y-intercept. However, we'll see that some of the variables do not
# have a simple linear relationship.

library(sf)
library(dplyr)
library(ggplot2)
library(cowplot)
library(colorspace)
library(reshape2)
library(tidyr)
hcl_palettes(plot = TRUE)

options(scipen=999)
theme_set(theme_bw(base_size = 10))

# Read in the feature data
treedata <- read.csv("data/deliverables/vector/feature_vars_labels.csv") 

# Read in a data set which has the site associated with each tree ID; select the site; and drop the geometry
shps <- st_read("data/deliverables/vector/trees_2017_rgreen.shp") %>% 
  dplyr::select(treeID, sites) %>% 
  st_drop_geometry()

# Join the data tables together and drop NA values
treedata <- right_join(treedata, shps, by = "treeID") %>% drop_na()
str(treedata)

# Determine which trees are at Soaproot Saddle and filter for only those
idx <- which(treedata$site == "SOAP")
treedata <- treedata[idx,]

# Convert the aspect (radians) to degrees
treedata$aspect <- treedata$aspect*180/pi
N <- dim(treedata)[1]

# Use the following names and domains for our feature variables of interest

vars <- c("live", "mean_green", "zmax2013", "meank", "rgranite", "tpa", "cover", "slope", "d_rivers")
varnames <- c("Live Label", "Mean Relative Greenness", "2013 Height (m)", "Mean distance 10 nearest trees", "Fractional granite", "Trees per hectare", "Canopy cover fraction", "Slope", 
                          "Distance from rivers")

lb <- c(0, -1, 5, 0, 0, 0, 0, 0, 0)
ub <- c(1, 1, 65, 40, 1, 300, 1, 60, 500)

# Initialize a list for the linear regression information for each variable
slope_table <- list()

# Tree height
temp <- dplyr::filter(treedata, zmax2013<=65)
breaks <- seq(5, 65, (65-5)/20)
idx_live <- which(temp$live==1)
idx_dead <- which(temp$live==0)
h_live <- hist(temp$zmax2013[idx_live], breaks = breaks)
h_dead <- hist(temp$zmax2013[idx_dead], breaks = breaks)

df_zmax <- data.frame(x = h_live$mids, total = h_live$counts + h_dead$counts, y = h_dead$counts/(h_live$counts + h_dead$counts))
df_zmax_plot <- dplyr::filter(df_zmax, total >= 50)

m <- lm(y ~ x + 1, data = df_zmax_plot) 
summary(m)

# y-intercept
m1 <- m$coefficients[1]

# slope
m2 <- m$coefficients[2]

# minimum and maximum bin values
ymn <- min(df_zmax_plot$y, na.rm = TRUE)
ymx <- max(df_zmax_plot$y, na.rm = TRUE)

r <- abs(ymx - ymn)

slope_table[[1]] <- data.frame(feat_var = "zmax2013", yint = m1, slope = m2, r = r)

plot_breaks <- rep(10, 5)^seq(2, 6, by = 1)

p1 <- ggplot(data = df_zmax_plot, aes(x = x, y = y, size = total, color = total)) + 
  geom_point(shape = 7) + 
  geom_abline(slope = m$coefficients[2], intercept = m$coefficients[1]) +
  scale_color_continuous_sequential(limits=c(0, 1000000), palette = "Burg", rev = FALSE, breaks=plot_breaks, labels = c("100" = "100", "1000" = "1,000", "10000" = "10,000", "100000" = "100,000", "1000000" = "1,000,000")) +
  scale_size_continuous(limits = c(0, 1000000), breaks=plot_breaks, labels = c("100" = "100", "1000" = "1,000", "10000" = "10,000", "100000" = "100,000", "1000000" = "1,000,000")) +
  xlim(c(0,65)) + ylim(c(0, 0.85)) +
  labs(x = "Height (m)", y = "Mortality Fraction", size = "Number of trees", color = "Number of trees") +
  guides(color= guide_legend(), size=guide_legend()) +
  theme(legend.direction = 'horizontal', legend.position = 'bottom')
p1

legend <- get_legend(p1)
p1 <- p1 + theme(legend.position="none")

str(treedata)

## Distance from granite

# Explore domain

hist(treedata$meank)

temp <- dplyr::filter(treedata, meank<=40)

breaks <- seq(0, 40, 40/20)
idx_live <- which(temp$live==1)
idx_dead <- which(temp$live==0)
h_live <- hist(temp$meank[idx_live], breaks = breaks)
h_dead <- hist(temp$meank[idx_dead], breaks = breaks)

df_feat <- data.frame(x = h_live$mids, total = h_live$counts + h_dead$counts, y = h_dead$counts/(h_live$counts + h_dead$counts))
df_feat_plot <- dplyr::filter(df_feat, total >= 50)

m <- lm(y ~ x + 1, data = df_feat_plot)
summary(m)
# y-intercept
m1 <- m$coefficients[1]

# slope
m2 <- m$coefficients[2]

# minimum and maximum bin values
ymn <- min(df_feat_plot$y, na.rm = TRUE)
ymx <- max(df_feat_plot$y, na.rm = TRUE)

r <- abs(ymx - ymn)

slope_table[[2]] <- data.frame(feat_var = "meank", yint = m1, slope = m2, r = r)

p2 <- ggplot(data = df_feat_plot, aes(x = x, y = y, size = total, color = total)) + 
  geom_point(shape=7) + 
  geom_abline(slope = m$coefficients[2], intercept = m$coefficients[1]) +
  scale_color_continuous_sequential(limits=c(0, 1000000), palette = "Burg", rev = FALSE, breaks=plot_breaks, labels = c("100" = "100", "1000" = "1,000", "10000" = "10,000", "100000" = "100,000", "1000000" = "1,000,000")) +
  scale_size_continuous(limits=c(0, 1000000), breaks=plot_breaks, labels = c("100" = "100", "1000" = "1,000", "10000" = "10,000", "100000" = "100,000", "1000000" = "1,000,000")) +
  xlim(c(0,40))+ylim(c(0, 0.85)) +
  labs(x = "Mean distance 10 nearest trees (m)", y = "Mortality Fraction", size = "Number of trees", color = "Number of trees") +
  guides(color= guide_legend(), size=guide_legend()) +
  theme(legend.position="none")
p2

## Distance from rivers

# Explore domain

hist(treedata$d_rivers)

temp <- dplyr::filter(treedata, d_rivers <= 500)

breaks <- seq(0, 500, 500/20)

idx_live <- which(temp$live==1)
idx_dead <- which(temp$live==0)
h_live <- hist(temp$d_rivers[idx_live], breaks = breaks)
h_dead <- hist(temp$d_rivers[idx_dead], breaks = breaks)

df_feat <- data.frame(x = h_live$mids, total = h_live$counts + h_dead$counts, y = h_dead$counts/(h_live$counts + h_dead$counts))
df_feat_plot <- dplyr::filter(df_feat, total >= 50)

m <- lm(y ~ x + 1, data = df_feat_plot)
summary(m)
# y-intercept
m1 <- m$coefficients[1]

# slope
m2 <- m$coefficients[2]

# minimum and maximum bin values
ymn <- min(df_feat_plot$y, na.rm = TRUE)
ymx <- max(df_feat_plot$y, na.rm = TRUE)

r <- abs(ymx - ymn)

slope_table[[3]] <- data.frame(feat_var = "d_rivers", yint = m1, slope = m2, r = r)

p3 <- ggplot(data = df_feat_plot, aes(x = x, y = y, size = total, color = total)) + 
  geom_point(shape=7) + 
  geom_abline(slope = m$coefficients[2], intercept = m$coefficients[1]) +
  scale_color_continuous_sequential(limits=c(0, 1000000), palette = "Burg", rev = FALSE, breaks=plot_breaks, labels = c("100" = "100", "1000" = "1,000", "10000" = "10,000", "100000" = "100,000", "1000000" = "1,000,000")) +
  scale_size_continuous(limits = c(0, 1000000), breaks=plot_breaks, labels = c("100" = "100", "1000" = "1,000", "10000" = "10,000", "100000" = "100,000", "1000000" = "1,000,000")) +
  labs(x = "Distance rivers (m)", y = "Mortality Fraction", size = "Number of trees", color = "Number of trees") +
  xlim(c(0,500)) + ylim(c(0, 0.85)) +
  guides(color= guide_legend(), size=guide_legend()) +
  theme(legend.position="none")
p3

# Explore domain

hist(treedata$cover)

rm(h_live, h_dead, df_feat, df_feat_plot, m)

breaks <- c(seq(0, 1, 1/20))

idx_live <- which(treedata$live==1)
idx_dead <- which(treedata$live==0)
h_live <- hist(treedata$cover[idx_live], breaks = breaks)
h_dead <- hist(treedata$cover[idx_dead], breaks = breaks)

df_feat <- data.frame(x = h_live$mids, total = h_live$counts + h_dead$counts, y = h_dead$counts/(h_live$counts + h_dead$counts))
df_feat_plot <- dplyr::filter(df_feat, total >= 50)

m <- lm(y ~ x + 1, data = df_feat_plot) 
summary(m)
# y-intercept
m1 <- m$coefficients[1]

# slope
m2 <- m$coefficients[2]

# minimum and maximum bin values
ymn <- min(df_feat_plot$y, na.rm = TRUE)
ymx <- max(df_feat_plot$y, na.rm = TRUE)

r <- abs(ymx - ymn)

slope_table[[4]] <- data.frame(feat_var = "cover", yint = m1, slope = m2, r = r)
p4 <- ggplot(data = df_feat_plot, aes(x = x, y = y, size = total, color = total)) + 
  geom_point(shape=7) + 
  geom_abline(slope = m$coefficients[2], intercept = m$coefficients[1]) +
  scale_color_continuous_sequential(limits=c(0, 1000000), palette = "Burg", rev = FALSE, breaks=plot_breaks, labels = c("100" = "100", "1000" = "1,000", "10000" = "10,000", "100000" = "100,000", "1000000" = "1,000,000")) +
  scale_size_continuous(limits=c(0, 1000000), breaks=plot_breaks, labels = c("100" = "100", "1000" = "1,000", "10000" = "10,000", "100000" = "100,000", "1000000" = "1,000,000")) +
  xlim(c(0,1))+ylim(c(0, 0.85)) +
  labs(x = "Canopy cover fraction", y = "Mortality Fraction", size = "Number of trees", color = "Number of trees") +
  guides(color= guide_legend(), size=guide_legend()) +
  theme(legend.position="none")
p4

##

str(treedata)

# Explore domain

hist(treedata$tpa)

rm(h_live, h_dead, df_feat, df_feat_plot, m)

temp <- filter(treedata, tpa<=300)

breaks <- c(seq(0, 300, 300/20))

idx_live <- which(temp$live==1)
idx_dead <- which(temp$live==0)

h_live <- hist(temp$tpa[idx_live], breaks = breaks)
h_dead <- hist(temp$tpa[idx_dead], breaks = breaks)

df_feat <- data.frame(x = h_live$mids, total = h_live$counts + h_dead$counts, y = h_dead$counts/(h_live$counts + h_dead$counts))
df_feat_plot <- dplyr::filter(df_feat, total >= 50)

m <- lm(y ~ x + 1, data = df_feat_plot)
summary(m)
# y-intercept
m1 <- m$coefficients[1]

# slope
m2 <- m$coefficients[2]

# minimum and maximum bin values
ymn <- min(df_feat_plot$y, na.rm = TRUE)
ymx <- max(df_feat_plot$y, na.rm = TRUE)

r <- abs(ymx - ymn)

slope_table[[5]] <- data.frame(feat_var = "tpa", yint = m1, slope = m2, r = r)

p5 <- ggplot(data = df_feat_plot, aes(x = x, y = y, size = total, color = total)) + 
  geom_point(shape=7) + 
  geom_abline(slope = m2, intercept = m1) +
  scale_color_continuous_sequential(limits=c(0, 1000000), palette = "Burg", rev = FALSE, breaks=plot_breaks, labels = c("100" = "100", "1000" = "1,000", "10000" = "10,000", "100000" = "100,000", "1000000" = "1,000,000")) +
  scale_size_continuous(limits=c(0, 1000000), breaks=plot_breaks, labels = c("100" = "100", "1000" = "1,000", "10000" = "10,000", "100000" = "100,000", "1000000" = "1,000,000")) +
  xlim(c(0,300))+ylim(c(0, 0.85)) +
  labs(x = "Trees per hectare", y = "Mortality Fraction", size = "Number of trees", color = "Number of trees") +
  guides(color= guide_legend(), size=guide_legend()) +
  theme(legend.position="none")
p5

# Explore domain
str(treedata)
hist(treedata$aspect)

# Create aspect categories
acats <- vector(length = length(treedata$aspect))
acats[treedata$aspect<=22.5 & treedata$aspect>(360-22.5)] <- "N"
acats[treedata$aspect<=67.5 & treedata$aspect>22.5] <- "NE"
acats[treedata$aspect<=112.5 & treedata$aspect>67.5] <- "E"
acats[treedata$aspect<=157.5 & treedata$aspect>112.5] <- "SE"
acats[treedata$aspect<=202.5 & treedata$aspect>157.5] <- "S"
acats[treedata$aspect<=247.5 & treedata$aspect>202.5] <- "SW"
acats[treedata$aspect<=292.5 & treedata$aspect>247.5] <- "W"
acats[treedata$aspect<=337.5 & treedata$aspect>292.5] <- "NW"
acats <- ordered(acats, levels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"))

breaks <- levels(acats)

#treedata <- dplyr::select(treedata, -acats)
treedata <- dplyr::mutate(treedata, acats = acats)

rm(h_live, h_dead, df_feat, df_feat_plot, m)

test <- table(dplyr::select(treedata, acats, live))

df_feat_plot <- data.frame(x = ordered(levels(acats)), total = test[,1]+test[,2], y = test[,1]/(test[,1]+test[,2]))

# y-intercept
m1 <- NA

# slope
m2 <- NA

# minimum and maximum bin values
ymn <- min(df_feat_plot$y, na.rm = TRUE)
ymx <- max(df_feat_plot$y, na.rm = TRUE)

r <- abs(ymx - ymn)

temp_df <- data.frame(feat_var = "aspect", yint = m1, slope = m2, r = r)
rownames(temp_df) <- rownames(slope_table[[5]])

slope_table[[6]] <- temp_df

p6 <- ggplot(data = df_feat_plot, aes(x = x, y = y, size = total, color = total)) + 
  geom_point(shape=7) + 
  scale_x_discrete(limits = breaks) +
  scale_color_continuous_sequential(limits=c(0, 1000000), palette = "Burg", rev = FALSE, breaks=plot_breaks, labels = c("100" = "100", "1000" = "1,000", "10000" = "10,000", "100000" = "100,000", "1000000" = "1,000,000")) +
  scale_size_continuous(limits=c(0, 1000000), breaks=plot_breaks, labels = c("100" = "100", "1000" = "1,000", "10000" = "10,000", "100000" = "100,000", "1000000" = "1,000,000")) +
  ylim(c(0, 0.85)) +
  labs(x = "Aspect", y = "Mortality Fraction", size = "Number of trees", color = "Number of trees") +
  guides(color= guide_legend(), size=guide_legend()) +
  theme(legend.position="none")
p6


# Explore domain
str(treedata)
hist(treedata$rgranite)

rm(h_live, h_dead, df_feat, df_feat_plot, m)

breaks <- c(seq(0, 1, 1/20))
idx_live <- which(treedata$live==1)
idx_dead <- which(treedata$live==0)

h_live <- hist(treedata$rgranite[idx_live], breaks = breaks)
h_dead <- hist(treedata$rgranite[idx_dead], breaks = breaks)

df_feat <- data.frame(x = h_live$mids, total = h_live$counts + h_dead$counts, y = h_dead$counts/(h_live$counts + h_dead$counts))
df_feat_plot <- dplyr::filter(df_feat, total >= 50)

m <- lm(y ~ x + 1, data = df_feat_plot)
summary(m)
# y-intercept
m1 <- m$coefficients[1]

# slope
m2 <- m$coefficients[2]

ymn <- min(df_feat_plot$y, na.rm = TRUE)
ymx <- max(df_feat_plot$y, na.rm = TRUE)

r <- abs(ymx - ymn)

slope_table[[7]] <- data.frame(feat_var = "rgranite", yint = m1, slope = m2, r = r)
p7 <- ggplot(data = df_feat_plot, aes(x = x, y = y, size = total, color = total)) + 
  geom_point(shape=7) + 
  geom_abline(slope = m$coefficients[2], intercept = m$coefficients[1]) +
  scale_color_continuous_sequential(limits=c(0, 1000000), palette = "Burg", rev = FALSE, breaks=plot_breaks, labels = c("100" = "100", "1000" = "1,000", "10000" = "10,000", "100000" = "100,000", "1000000" = "1,000,000")) +
  scale_size_continuous(limits=c(0, 1000000), breaks=plot_breaks, labels = c("100" = "100", "1000" = "1,000", "10000" = "10,000", "100000" = "100,000", "1000000" = "1,000,000")) +
  xlim(c(0,1))+ylim(c(0, 0.85)) +
  labs(x = "Granite fraction", y = "Mortality Fraction", size = "Number of trees", color = "Number of trees") +
  guides(color= guide_legend(), size=guide_legend()) +
  theme(legend.position="none")
p7


# Explore domain
str(treedata)
hist(treedata$slope)

rm(h_live, h_dead, df_feat, df_feat_plot, m)

breaks <- c(seq(0, 90, 90/20))

idx_live <- which(treedata$live==1)
idx_dead <- which(treedata$live==0)
h_live <- hist(treedata$slope[idx_live], breaks = breaks)
h_dead <- hist(treedata$slope[idx_dead], breaks = breaks)

df_feat <- data.frame(x = h_live$mids, total = h_live$counts + h_dead$counts, y = h_dead$counts/(h_live$counts + h_dead$counts))
df_feat_plot <- dplyr::filter(df_feat, total >= 50)

m <- lm(y ~ x + 1, data = df_feat_plot)
summary(m)
# y-intercept
m1 <- m$coefficients[1]

# slope
m2 <- m$coefficients[2]

# minimum and maximum bin values
ymn <- min(df_feat_plot$y, na.rm = TRUE)
ymx <- max(df_feat_plot$y, na.rm = TRUE)

r <- abs(ymx - ymn)

slope_table[[8]] <- data.frame(feat_var = "slope", yint = m1, slope = m2, r = r)
p8 <- ggplot(data = df_feat_plot, aes(x = x, y = y, size = total, color = total)) + 
  geom_point(shape=7) + 
  geom_abline(slope = m$coefficients[2], intercept = m$coefficients[1]) +
  scale_color_continuous_sequential(limits=c(0, 1000000), palette = "Burg", rev = FALSE, breaks=plot_breaks, labels = c("100" = "100", "1000" = "1,000", "10000" = "10,000", "100000" = "100,000", "1000000" = "1,000,000")) +
  scale_size_continuous(limits=c(0, 1000000), breaks=plot_breaks, labels = c("100" = "100", "1000" = "1,000", "10000" = "10,000", "100000" = "100,000", "1000000" = "1,000,000")) +
  xlim(c(0,90))+ylim(c(0, 0.85)) +
  labs(x = "Slope (degrees)", y = "Mortality Fraction", size = "Number of trees", color = "Number of trees") +
  guides(color= guide_legend(), size=guide_legend()) +
  theme(legend.position="none")
p8



gsoap <- plot_grid(p1, p3, p5, p2, p4, p7, p8, p6, nrow = 4, align = "hv", axis = 'tr',
                rel_heights = c(1,1,1,1), rel_widths = c(1,1),
                labels = c("a.", "e.", "b.", "f.", "c.", "g.", "d.", "h."), label_fontface = "plain", label_size = 12)
gsoap

# Create a table of the slopes and y-intercepts
slope_table_df <- do.call(rbind.data.frame, slope_table)
slope_table_df <- dplyr::arrange(slope_table_df, desc(r))
slope_table_df
