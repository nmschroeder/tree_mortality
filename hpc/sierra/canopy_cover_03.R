#!/usr/bin/env Rscript

library(dplyr)
library(stringr)

# HPC
data_dir <- "/path/to/your/data/directory/"
data_dir <- paste0(data_dir, "sierra")
fnames <- list.files(path = data_dir, 
                     pattern = glob2rx("canopy_cover_*.csv"), 
                     full.names = TRUE)

setwd(data_dir)

data_csv <- lapply(fnames, read.csv)


d <- do.call(rbind.data.frame, data_csv)

print("combine complete")

print("data frame structure:")
str(d)

write.table(d, "canopy_cover.csv")
