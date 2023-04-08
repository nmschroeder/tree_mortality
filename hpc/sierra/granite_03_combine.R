#!/usr/bin/env Rscript

library(dplyr)

# HPC
data_dir <- "/path/to/your/data/directory/"
data_dir <- paste0(data_dir, "sierra/")

fnames <- list.files(path = data_dir, 
                     pattern = glob2rx("granite_distance_*.csv"), 
                     full.names = TRUE)

dlist <- lapply(fnames, read.csv)

d <- do.call(rbind.data.frame, dlist)

write.table(d, paste0(data_dir,"/granite_distances.csv"))
