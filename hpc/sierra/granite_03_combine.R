#!/usr/bin/env Rscript

library(dplyr)

# HPC3
data_dir <- "/dfs5/bio/hemmingn/sierra/align_trees"

fnames <- list.files(path = data_dir, 
                     pattern = glob2rx("granite_distance_*.csv"), 
                     full.names = TRUE)

dlist <- lapply(fnames, read.csv)

d <- do.call(rbind.data.frame, dlist)

write.table(d, paste0(data_dir,"/granite_distances.csv"))
