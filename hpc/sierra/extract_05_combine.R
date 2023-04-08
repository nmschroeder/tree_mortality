#!/usr/bin/env Rscript

library(dplyr)
library(sf)
library(raster)
library(exactextractr)

data_dir <- "/path/to/your/data/directory/"
data_dir <- paste0(data_dir, "sierra")
setwd(data_dir)

env1 <- read.csv("env_data_las_intersection_part1.csv")
str(env1)
env2 <- read.table("env_data_las_intersection_part2.csv")
str(env2)

env3 <- read.table("align_trees/canopy_cover.csv")

env_data <- right_join(env1, env2, by = "treeID") %>% right_join(env3, by = "treeID")

write.csv(env_data, "env_data_las_intersection.csv")
