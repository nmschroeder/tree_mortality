#!/usr/bin/env Rscript

# Create data frame of the area of each crown perimeter for each year

# HPC
data_dir <- "/path/to/your/data/directory/"
home_dir <- paste0(data_dir, "sierra")

library(dplyr)
library(sf)

setwd(home_dir)

area_df <- read.csv("tree_crown_area_by_year.csv") %>% dplyr::filter(area2013>1)

mort2017 <- ifelse(area_df$area2017<1, 1, 0)
mort2018 <- ifelse(mort2017==1 | area_df$area2018 < 1, 1, 0)
mort2019 <- ifelse(mort2018==1 | area_df$area2019 < 1, 1, 0)
mort2021 <- ifelse(mort2019==1 | area_df$area2021 < 1, 1, 0)

mort_df <- mutate(area_df, mort2017, mort2018, mort2019, mort2021)

write.csv(mort_df, "trees_dead_by_area.csv")



