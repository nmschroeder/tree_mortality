#!/usr/bin/env Rscript

library(dplyr)
library(stringr)

# HPC
data_dir <- "/path/to/your/data/directory/"
data_dir <- paste0(data_dir, "sierra")
fnames <- list.files(path = data_dir, 
                     pattern = glob2rx("env_data_part2_*.csv"), 
                     full.names = TRUE)

setwd(data_dir)
dlist <- lapply(fnames, read.csv)
d <- do.call(rbind.data.frame, dlist)

print("combine complete")

print("data frame structure:")
str(d)

write.table(d, "env_data_las_intersection_part2.csv")
