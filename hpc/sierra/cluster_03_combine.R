#!/usr/bin/env Rscript

library(dplyr)
library(stringr)

# HPC3
home_dir <- "/data/homezvol0/hemmingn/sierra"
data_dir <- "/dfs5/bio/hemmingn/sierra/align_trees"
fnames <- list.files(path = data_dir, 
                     pattern = glob2rx("clustering_metrics_*.csv"), 
                     full.names = TRUE)

setwd(data_dir)


index_list <- read.table(paste0(home_dir, "/cluster_02_inputs.txt"))
index_list <- gsub(pattern = ",", replacement = "_", x = index_list$V1)

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
