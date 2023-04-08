library(sf)
library(dplyr)
library(ggplot2)
library(cowplot)
library(colorspace)
library(reshape2)
library(gridExtra)
library(grid)
hcl_palettes(plot = TRUE)
theme_set(theme_bw(base_size = 10))

vars <- c("d_mean_green", "zmax2013", "cover", "rgranite", "tpa", "meank", "slope", "d_rivers")
varnames <- c("Difference in Mean Relative Greenness", "Height (m)", "Canopy cover fraction", "Fractional granite", "Trees per hectare", "Mean Distance 10 Nearest Trees (m)", "Slope", 
              "Distance from rivers")

# Restricted domain correlations
lb <- c(-1, 5, 0, 0, 0, 0, 0, 0)
ub <- c(1, 65, 1, 1, 300, 40, 60, 500)

options(scipen=999)
treedata <- read.csv("data/deliverables/vector/feature_vars_labels.csv")
theme_set(theme_bw(base_size = 10))

shps_start <- st_read("data/deliverables/vector/trees_2013_rgreen.shp")
treedata_start <- shps_start %>% st_drop_geometry() %>% dplyr::select(treeID, mean_green) %>% rename(mean_green_2013 = mean_green)

shps <- st_read("data/deliverables/vector/trees_2017_rgreen.shp")

shps <- dplyr::select(shps, treeID, sites) %>% st_drop_geometry()
treedata <- treedata %>% right_join(shps, by = "treeID") %>% right_join(treedata_start, by = "treeID") %>% mutate(d_mean_green = (mean_green - mean_green_2013))

idx <- which(!is.na(treedata$d_mean_green))
treedata <- treedata[idx,]

N <- dim(treedata)[1]

## Combined sites

idx <- which(!is.na(treedata$live))
treedata_cor <- treedata[idx,]
var_cor <- matrix(data = NA, nrow = length(vars), ncol = length(vars), dimnames = list(vars, vars))
cor_n <- var_cor

for (i in 1:length(vars)){
  for (j in i:length(vars)){
    x <- treedata_cor[,vars[i]]
    y <- treedata_cor[,vars[j]]
    idx <- !is.na(x) & !is.na(y) & x>=lb[i] & x<=ub[i] & y>=lb[j] & y<=ub[j] #& tf
    cor_n[i,j] <- sum(idx)
    var_cor[i,j] <- cor(x[idx], y[idx])
  }
}

rownames(var_cor) <- varnames
colnames(var_cor) <- varnames

var_cor_tri <- var_cor
var_cor[lower.tri(var_cor)] <- 0
var_cor <- var_cor + t(var_cor) - diag(x=1, nrow = dim(var_cor)[1], ncol = dim(var_cor)[1])

# Methods for plotting the correlation heat map: http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization
var_cor <- var_cor %>% round(digits = 2)

# Melt the matrix into a data frame
var_melt <- melt(var_cor_tri, na.rm = TRUE)

# Reorder the variables by magnitude of correlation
reorder_vars_df <- dplyr::filter(var_melt, Var1 == 'Difference in Mean Relative Greenness') %>% arrange(desc(abs(value)))
reorder_vars <- reorder_vars_df$Var2
test <- var_cor[reorder_vars, reorder_vars]

var_cor_tri <- test
var_cor_tri[lower.tri(var_cor_tri)] <- NA
var_melt <- melt(var_cor_tri, na.rm = TRUE)

# Create a ggheatmap
ggheatmap <- ggplot(var_melt, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "darkred", high = "steelblue", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   hjust = 1))+
  coord_fixed()

plot_combined <- ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 2.5) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.direction = "vertical")


leg <- get_legend(plot_combined)

plot_combined <- plot_combined + theme(legend.position = "none")


## Soaproot Saddle

idx <- which(!is.na(treedata$live) & treedata$site == "SOAP")
treedata_cor <- treedata[idx,]
var_cor <- matrix(data = NA, nrow = length(vars), ncol = length(vars), dimnames = list(vars, vars))
cor_n <- var_cor

for (i in 1:length(vars)){
  for (j in i:length(vars)){
    x <- treedata_cor[,vars[i]]
    y <- treedata_cor[,vars[j]]
    idx <- !is.na(x) & !is.na(y) & x>=lb[i] & x<=ub[i] & y>=lb[j] & y<=ub[j] #& tf
    cor_n[i,j] <- sum(idx)
    var_cor[i,j] <- cor(x[idx], y[idx])
  }
}


rownames(var_cor) <- varnames
colnames(var_cor) <- varnames

var_cor_tri <- var_cor
var_cor[lower.tri(var_cor)] <- 0
var_cor <- var_cor + t(var_cor) - diag(x=1, nrow = dim(var_cor)[1], ncol = dim(var_cor)[1])
var_cor <- var_cor %>% round(digits = 2)

# Melt the matrix into a data frame
var_melt <- melt(var_cor_tri, na.rm = TRUE)

# Reorder the variables by magnitude of correlation
test <- var_cor[reorder_vars, reorder_vars]

var_cor_tri <- test
var_cor_tri[lower.tri(var_cor_tri)] <- NA
var_melt <- melt(var_cor_tri, na.rm = TRUE)

# Create a ggheatmap
ggheatmap <- ggplot(var_melt, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "darkred", high = "steelblue", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   hjust = 1))+
  coord_fixed()

plot_soap <- ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 2.5) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none")

plot_soap

# Lower Teakettle
idx <- which(!is.na(treedata$live) & treedata$site == "TEAK")
treedata_cor <- treedata[idx,]

var_cor <- matrix(data = NA, nrow = length(vars), ncol = length(vars), dimnames = list(vars, vars))
cor_n <- var_cor

for (i in 1:length(vars)){
  for (j in i:length(vars)){
    x <- treedata_cor[,vars[i]]
    y <- treedata_cor[,vars[j]]
    idx <- !is.na(x) & !is.na(y) & x>=lb[i] & x<=ub[i] & y>=lb[j] & y<=ub[j] #& tf
    cor_n[i,j] <- sum(idx)
    var_cor[i,j] <- cor(x[idx], y[idx])
  }
}

rownames(var_cor) <- varnames
colnames(var_cor) <- varnames


var_cor_tri <- var_cor
var_cor[lower.tri(var_cor)] <- 0
var_cor <- var_cor + t(var_cor) - diag(x=1, nrow = dim(var_cor)[1], ncol = dim(var_cor)[1])
var_cor <- var_cor %>% round(digits = 2)

# Melt the matrix into a data frame
var_melt <- melt(var_cor_tri, na.rm = TRUE)

# Reorder the variables by magnitude of correlation
test <- var_cor[reorder_vars, reorder_vars]

var_cor_tri <- test
var_cor_tri[lower.tri(var_cor_tri)] <- NA
var_melt <- melt(var_cor_tri, na.rm = TRUE)

# Create a ggheatmap
ggheatmap <- ggplot(var_melt, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "darkred", high = "steelblue", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   hjust = 1))+
  coord_fixed()

plot_teak <- ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 2.5) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none")

plot_cor <- plot_grid(plot_soap, plot_teak, plot_combined, leg, 
                      labels = c("a. Soaproot Saddle", "b. Lower Teakettle", "c. Combined", NA), nrow = 2, align = "hv", axis = 'tlrb',
                      label_fontface = "plain", label_size = 12)
plot_cor

ggsave("figures/Figure_S9_corr_panel.pdf", plot = plot_cor, width = 9, height = 9.2, units = "in")

