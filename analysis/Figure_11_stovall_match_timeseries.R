# This script creates a time series for the mean and standard deviation of the mean NDVI for three categories
# of trees we matched to trees in the Stovall (2019) data set
# 1) trees labeled live in both data sets,
# 2) trees labeled dead in both data sets, and
# 3) trees labeled as live in our dataset and dead in Stovall (2019).

# This goes with Figure 11 from Hemming-Schroeder et al. (2023)

library(dplyr)
library(sf)
library(tidyr)
library(cowplot)

theme_set(theme_bw(base_size = 10))

# The data set read in on this line is created at the end of the Figure 10 script. Because it includes data
# from another study, we do not include it in this repository. 
stovall_intersection <- read.csv("../data/stovall_intersection.csv")
match <- (stovall_intersection$live == stovall_intersection$live_stov)*1.0
stovall <- dplyr::select(stovall_intersection, treeID, live_stov) %>% mutate(match = match)

# Find which Stovall trees are matched to trees from our study
treeIDs <- dplyr::filter(stovall_intersection, contain == 1)$treeID

# Load in the shapefiles with mean NDVI, filter for only the treeIDs that are matched
# to Stovall trees, and select only columns of interest (treeID, live, mean_ndvi, and poly_area)
trees2013 <- read_sf("data/deliverables/vector/trees_2013_ndvi.shp") %>% dplyr::filter(treeID %in% treeIDs) %>% dplyr::select(treeID, mean_ndvi, live)
poly_area <- st_area(trees2013) %>% as.numeric()
trees2013 <- trees2013 %>% st_drop_geometry() %>% mutate(poly_area)
treeIDs <- dplyr::filter(trees2013, !is.na(live))$treeID

trees2017 <- read_sf("data/deliverables/vector/trees_2017_ndvi.shp") %>% dplyr::filter(treeID %in% treeIDs) %>% dplyr::select(treeID, mean_ndvi, live)
poly_area <- st_area(trees2017) %>% as.numeric()
trees2017 <- trees2017 %>% st_drop_geometry() %>% mutate(poly_area)
treeIDs <- dplyr::filter(trees2017, !is.na(live))$treeID

trees2018 <- read_sf("data/deliverables/vector/trees_2018_ndvi.shp") %>% dplyr::filter(treeID %in% treeIDs) %>% dplyr::select(treeID, mean_ndvi, live)
poly_area <- st_area(trees2018) %>% as.numeric()
trees2018 <- trees2018 %>% st_drop_geometry() %>% mutate(poly_area)
treeIDs <- dplyr::filter(trees2018, !is.na(live))$treeID

trees2019 <- read_sf("data/deliverables/vector/trees_2019_ndvi.shp") %>% dplyr::filter(treeID %in% treeIDs) %>% dplyr::select(treeID, mean_ndvi, live)
poly_area <- st_area(trees2019) %>% as.numeric()
trees2019 <- trees2019 %>% st_drop_geometry() %>% mutate(poly_area)
treeIDs <- dplyr::filter(trees2019, !is.na(live))$treeID

trees2021 <- read_sf("data/deliverables/vector/trees_2021_ndvi.shp") %>% dplyr::filter(treeID %in% treeIDs) %>% dplyr::select(treeID, mean_ndvi, live)
poly_area <- st_area(trees2021) %>% as.numeric()
trees2021 <- trees2021 %>% st_drop_geometry() %>% mutate(poly_area)
treeIDs <- dplyr::filter(trees2021, !is.na(live))$treeID

trees2013 <- dplyr::filter(trees2013, treeID %in% treeIDs)
trees2017 <- dplyr::filter(trees2017, treeID %in% treeIDs)
trees2018 <- dplyr::filter(trees2018, treeID %in% treeIDs)
trees2019 <- dplyr::filter(trees2019, treeID %in% treeIDs)
trees2021 <- dplyr::filter(trees2021, treeID %in% treeIDs)

str(trees2013)

dfs <- list(trees2013, trees2017, trees2018, trees2019, trees2021)
years <- c(2013, 2017, 2018, 2019, 2021)
df_match <- list()
for (i in 1:length(dfs)){
  df_match[[i]] <- dfs[[i]] %>% left_join(stovall, by = "treeID") %>% 
    group_by(match, live) %>% 
    summarize(ndvi_mean = mean(mean_ndvi, na.rm = TRUE), 
              ndvi_sd = sd(mean_ndvi, na.rm = TRUE),
              ndvi_se = sd(mean_ndvi, na.rm = TRUE)/sqrt(length(mean_ndvi)), 
              ca_mean = mean(poly_area, na.rm = TRUE), 
              ca_sd = sd(poly_area, na.rm = TRUE),
              ca_se = sd(poly_area, na.rm = TRUE)/sqrt(length(poly_area)), 
              year = years[i], 
              count = length(live))
}

df_combined <- do.call(rbind.data.frame, df_match)
df_combined <- as.data.frame(df_combined)

test <- dplyr::filter(df_combined, match == 1 & live == 1)
test

## Version 1: separated

p1 <- ggplot(data = dplyr::filter(df_combined, match == 1 & live == 1)) + 
  geom_point(mapping = aes(x = year, y = ndvi_mean)) +
  geom_errorbar(mapping = aes(x = year, ymin=ndvi_mean-ndvi_se, ymax=ndvi_mean+ndvi_se), width=.2,
                position=position_dodge(.9)) +
  ylim(c(0.25, 0.85)) +
  xlab("Year")+
  ylab("NDVI")+
  ggtitle("Classified as Live in Both Studies")+
  scale_x_continuous(breaks = seq(2013, 2021))
p1

p2 <- ggplot(data = dplyr::filter(df_combined, match == 1 & live == 0)) + 
  geom_point(mapping = aes(x = year, y = ndvi_mean)) +
  geom_errorbar(mapping = aes(x = year, ymin=ndvi_mean-ndvi_se, ymax=ndvi_mean+ndvi_se), width=.2,
                position=position_dodge(.9)) +
  ylim(c(0.25, 0.85)) +
  xlab("Year")+
  ylab("NDVI")+
  ggtitle("Classified as Dead in Both Studies")+
  scale_x_continuous(breaks = seq(2013, 2021))
p2

p3 <- ggplot(data = dplyr::filter(df_combined, match == 0 & live == 1)) + 
  geom_point(mapping = aes(x = year, y = ndvi_mean)) +
  geom_errorbar(mapping = aes(x = year, ymin=ndvi_mean-ndvi_se, ymax=ndvi_mean+ndvi_se), width=.2,
                position=position_dodge(.9)) +
  ylim(c(0.25, 0.85)) +
  xlab("Year")+
  ylab("NDVI")+
  ggtitle("Classified as Live in this Study but Dead in Stovall et al.")+
  scale_x_continuous(breaks = seq(2013, 2021))
p3


pgrid <- plot_grid(p1, p2, p3, nrow = 3, align = "hv", axis = 'tr')
pgrid
#ggsave("ndvi_timeseries_panel.png", plot=pgrid)

###
p1_ca <- ggplot(data = dplyr::filter(df_combined, match == 1 & live == 1)) + 
  geom_point(mapping = aes(x = year, y = ca_mean)) +
  geom_errorbar(mapping = aes(x = year, ymin=ca_mean-ca_se, ymax=ca_mean+ca_se), width=.2,
                position=position_dodge(.9)) +
  geom_hline(yintercept=0)+
  xlab("Year")+
  ylab(bquote("Crown Area "~(m^2)))+
  ylim(c(-10, 80)) +
  ggtitle("Classified as Live in Both Studies") +
  scale_x_continuous(breaks = seq(2013, 2021))
p1_ca

p2_ca <- ggplot(data = dplyr::filter(df_combined, match == 1 & live == 0)) + 
  geom_point(mapping = aes(x = year, y = ca_mean)) +
  geom_errorbar(mapping = aes(x = year, ymin=ca_mean-ca_se, ymax=ca_mean+ca_se), width=.2,
                position=position_dodge(.9)) +
  geom_hline(yintercept=0)+
  xlab("Year")+
  ylab(bquote("Crown Area "~(m^2)))+
  ggtitle("Classified as Dead in Both Studies") +
  ylim(c(-10, 80)) +
  scale_x_continuous(breaks = seq(2013, 2021))
p2_ca

p3_ca <- ggplot(data = dplyr::filter(df_combined, match == 0 & live == 1)) + 
  geom_point(mapping = aes(x = year, y = ca_mean)) +
  geom_errorbar(mapping = aes(x = year, ymin=ca_mean-ca_se, ymax=ca_mean+ca_se), width=.2,
                position=position_dodge(.9)) +
  geom_hline(yintercept=0)+
  xlab("Year")+
  ylab(bquote("Crown Area "~(m^2)))+
  ggtitle("Classified as Live in this Study but Dead in Stovall et al.") +
  ylim(c(-10, 80)) +
  scale_x_continuous(breaks = seq(2013, 2021))
p3_ca


pgrid2 <- plot_grid(p1_ca, p2_ca, p3_ca, nrow = 3, align = "hv", axis = 'tr')
pgrid2

#ggsave("crown_area_timeseries_panel.png", plot=pgrid2)

## Version 2, combined into one plot

dataset <- rep("1", times = dim(df_combined)[1])
dataset[df_combined$match == 1 & df_combined$live == 0] <- "2"
dataset[df_combined$match == 0 & df_combined$live == 1] <- "3"
dataset[df_combined$match == 0 & df_combined$live == 0] <- "4"
df_combined$dataset = dataset

p1 <- ggplot() + 
  geom_point(data = dplyr::filter(df_combined, dataset == "1"), mapping = aes(x = year, y = ndvi_mean), pch = 17, color = "steelblue") +
  geom_line(data = dplyr::filter(df_combined, dataset == "1"), mapping = aes(x = year, y =  ndvi_mean), color = "steelblue") +
  geom_errorbar(data = dplyr::filter(df_combined, dataset == "1"), mapping = aes(x = year, ymin=ndvi_mean-ndvi_se, ymax=ndvi_mean+ndvi_se), color = "steelblue", width=.2,
                position=position_dodge(.9)) +
  geom_point(data = dplyr::filter(df_combined, dataset == "2"), mapping = aes(x = year, y = ndvi_mean), pch = 4, color = "goldenrod3") +
  geom_line(data = dplyr::filter(df_combined, dataset == "2"), mapping = aes(x = year, y =  ndvi_mean), color = "goldenrod3") +
  geom_errorbar(data = dplyr::filter(df_combined, dataset == "2"), mapping = aes(x = year, ymin=ndvi_mean-ndvi_se, ymax=ndvi_mean+ndvi_se), color = "goldenrod3", width=.2,
                position=position_dodge(.9)) +
  geom_point(data = dplyr::filter(df_combined, dataset == "3"), mapping = aes(x = year, y = ndvi_mean)) +
  geom_line(data = dplyr::filter(df_combined, dataset == "3"), mapping = aes(x = year, y =  ndvi_mean)) +
  geom_errorbar(data = dplyr::filter(df_combined, dataset == "3"), mapping = aes(x = year, ymin=ndvi_mean-ndvi_se, ymax=ndvi_mean+ndvi_se), width=.2,
                position=position_dodge(.9)) +
  
  ylim(c(0.25, 0.85)) +
  xlab("Year")+
  ylab("NDVI")+
  scale_x_continuous(breaks = seq(2013, 2021))
p1

p2 <- ggplot() + 
  geom_point(data = dplyr::filter(df_combined, dataset == "1"), mapping = aes(x = year, y = ca_mean), pch = 17, color = "steelblue") +
  geom_line(data = dplyr::filter(df_combined, dataset == "1"), mapping = aes(x = year, y =  ca_mean), color = "steelblue") +
  geom_errorbar(data = dplyr::filter(df_combined, dataset == "1"), mapping = aes(x = year, ymin=ca_mean-ca_se, ymax=ca_mean+ca_se), color = "steelblue", width=.2,
                position=position_dodge(.9)) +
  geom_point(data = dplyr::filter(df_combined, dataset == "2"), mapping = aes(x = year, y = ca_mean), pch = 4, color = "goldenrod3") +
  geom_line(data = dplyr::filter(df_combined, dataset == "2"), mapping = aes(x = year, y =  ca_mean), color = "goldenrod3") +
  geom_errorbar(data = dplyr::filter(df_combined, dataset == "2"), mapping = aes(x = year, ymin=ca_mean-ca_se, ymax=ca_mean+ca_se), color = "goldenrod3", width=.2,
                position=position_dodge(.9)) +
  geom_point(data = dplyr::filter(df_combined, dataset == "3"), mapping = aes(x = year, y = ca_mean)) +
  geom_line(data = dplyr::filter(df_combined, dataset == "3"), mapping = aes(x = year, y =  ca_mean)) +
  geom_errorbar(data = dplyr::filter(df_combined, dataset == "3"), mapping = aes(x = year, ymin=ca_mean-ca_se, ymax=ca_mean+ca_se), width=.2,
                position=position_dodge(.9)) +
  scale_color_manual(name = "Labels", 
                     values = c("1"="steelblue", 
                                "2" = "goldenrod3",
                                "3" = "gray50"),
                     labels = c("1"="Live in both studies", 
                                "2" = "Dead in both studies",
                                "3" = "Live in this study but dead in Stovall et al.")) +
  theme(legend.position = "bottom") +
  
  ylim(c(-10, 80)) +
  xlab("Year")+
  ylab(bquote("Crown Area "~(m^2)))+
  scale_x_continuous(breaks = seq(2013, 2021))
p2

df_combined_v2 <- filter(df_combined, dataset != 4)

p3 <- ggplot() + 
  geom_point(data = df_combined_v2, mapping = aes(x = year, y = ndvi_mean, color = dataset, pch = dataset)) +
  geom_line(data = df_combined_v2, mapping = aes(x = year, y =  ndvi_mean, color = dataset)) +
  geom_errorbar(data = df_combined_v2, mapping = aes(x = year, ymin=ndvi_mean-ndvi_se, ymax=ndvi_mean+ndvi_se, color = dataset), width=.2) +
  scale_color_manual(name = "Labels", 
                     values = c("1"="steelblue", 
                                "2" = "goldenrod3",
                                "3" = "gray50"),
                     labels = c("1"="Live in both studies", 
                                "2" = "Dead in both studies",
                                "3" = "Live in this study but dead in Stovall et al.")) +
  scale_shape_manual(name = "Labels", 
                     values = c("1" = 17, "2" = 4, "3" = 19),
                     labels = c("1"="Live in both studies", 
                                "2" = "Dead in both studies",
                                "3" = "Live in this study but dead in Stovall et al."))+
  ylim(c(0.25, 0.85)) +
  xlab("Year")+
  ylab("NDVI")+
  scale_x_continuous(breaks = seq(2013, 2021)) +
  theme(panel.grid = element_blank())
p3
#ggsave("ndvi_ts.png", plot=p3, height = 3.5, width = 6, unit = "in")



p4 <- ggplot() + 
  geom_point(data = df_combined_v2, mapping = aes(x = year, y = ca_mean, color = dataset, pch = dataset)) +
  geom_line(data = df_combined_v2, mapping = aes(x = year, y =  ca_mean, color = dataset)) +
  geom_errorbar(data = df_combined_v2, mapping = aes(x = year, ymin=ca_mean-ca_se, ymax=ca_mean+ca_se, color = dataset), width=.2) +
  scale_color_manual(name = "Labels", 
                     values = c("1"="steelblue", 
                                "2" = "goldenrod3",
                                "3" = "gray50"),
                     labels = c("1"="Live in both studies", 
                                "2" = "Dead in both studies",
                                "3" = "Live in this study but dead in Stovall et al.")) +
  scale_shape_manual(name = "Labels", 
                     values = c("1" = 17, "2" = 4, "3" = 19),
                     labels = c("1"="Live in both studies", 
                                "2" = "Dead in both studies",
                                "3" = "Live in this study but dead in Stovall et al."))+
  ylim(c(-10, 80)) +
  xlab("Year")+
  ylab(bquote("Crown Area "~(m^2)))+
  scale_x_continuous(breaks = seq(2013, 2021)) +
  theme(panel.grid = element_blank())
p4
#ggsave("crownarea_ts.png", plot=p4, height = 3.5, width = 6, unit = "in")

##

p_1 <- ggplot() + 
  geom_point(data = df_combined_v2, mapping = aes(x = year, y = ndvi_mean, color = dataset, pch = dataset)) +
  geom_line(data = df_combined_v2, mapping = aes(x = year, y =  ndvi_mean, color = dataset)) +
  geom_errorbar(data = df_combined_v2, mapping = aes(x = year, ymin=ndvi_mean-ndvi_sd, ymax=ndvi_mean+ndvi_sd, color = dataset), width=.2) +
  scale_color_manual(name = "", 
                     values = c("1"="steelblue", 
                                "2" = "goldenrod3",
                                "3" = "gray50"),
                     labels = c("1"="Live in both studies", 
                                "2" = "Dead in both studies",
                                "3" = "Live in this study but dead in Stovall et al.")) +
  scale_shape_manual(name = "", 
                     values = c("1" = 17, "2" = 4, "3" = 19),
                     labels = c("1"="Live in both studies", 
                                "2" = "Dead in both studies",
                                "3" = "Live in this study but dead in Stovall et al."))+
  ylim(c(0.25, 0.85)) +
  xlab("Year")+
  ylab("NDVI")+
  scale_x_continuous(breaks = seq(2013, 2021)) +
  theme(panel.grid = element_blank(), legend.direction = 'horizontal', legend.position = 'bottom') +
  guides(colour = guide_legend(nrow = 3))


leg <- get_legend(p_1)
p_1 <- p_1 + theme(legend.position = 'none')
p_1


p_2 <- ggplot() + 
  geom_point(data = df_combined_v2, mapping = aes(x = year, y = ca_mean, color = dataset, pch = dataset)) +
  geom_line(data = df_combined_v2, mapping = aes(x = year, y =  ca_mean, color = dataset)) +
  geom_errorbar(data = df_combined_v2, mapping = aes(x = year, ymin=ca_mean-ca_sd, ymax=ca_mean+ca_sd, color = dataset), width=.2) +
  scale_color_manual(name = "", 
                     values = c("1"="steelblue", 
                                "2" = "goldenrod3",
                                "3" = "gray50"),
                     labels = c("1"="Live in both studies", 
                                "2" = "Dead in both studies",
                                "3" = "Live in this study but dead in Stovall et al.")) +
  scale_shape_manual(name = "", 
                     values = c("1" = 17, "2" = 4, "3" = 19),
                     labels = c("1"="Live in both studies", 
                                "2" = "Dead in both studies",
                                "3" = "Live in this study but dead in Stovall et al."))+
  ylim(c(-10, 80)) +
  xlab("Year")+
  ylab(bquote("Crown Area "~(m^2)))+
  scale_x_continuous(breaks = seq(2013, 2021)) +
  theme(panel.grid = element_blank(), legend.position = 'none')

p_col <- plot_grid(p_1, p_2, leg, nrow = 3, align = "hv", axis = 'brlt', rel_heights = c(1, 1, 0.4), labels = c("a.", "b.", NA), label_fontface = "plain", label_size = 12)

p_col

ggsave("figures/Figure_11_ts_panel.pdf", plot=p_col, height = 8, width = 4, unit = "in")





