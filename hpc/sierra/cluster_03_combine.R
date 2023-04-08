#!/usr/bin/env Rscript

library(dplyr)
library(stringr)

# HPC
data_dir <- "/path/to/your/data/directory/"
data_dir <- paste0(data_dir, "sierra/")
fnames <- list.files(path = data_dir, 
                     pattern = glob2rx("clustering_metrics_*.csv"), 
                     full.names = TRUE)

index_list <- read.table("cluster_02_inputs.txt")
index_list <- gsub(pattern = ",", replacement = "_", x = index_list$V1)

setwd(data_dir)

dlist <- list()

for (i in 1:length(index_list)){
  idx <- which(str_detect(fnames, pattern = index_list[i]))
  print(idx)
  dlist[[i]] <- read.csv(fnames[idx])
}

d <- do.call(rbind.data.frame, dlist)

print("combine complete")

print("data frame structure:")
str(d)

write.table(d, "clustering_metrics.csv")
