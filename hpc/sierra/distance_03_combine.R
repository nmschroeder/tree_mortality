#!/usr/bin/env Rscript

library(dplyr)
library(stringr)

# HPC3
data_dir <- "/dfs5/bio/hemmingn/sierra/align_trees"
fnames <- list.files(path = data_dir, 
                     pattern = glob2rx("landscape_distance_*.csv"), 
                     full.names = TRUE)
setwd(data_dir)
print("troubleshooting")
print(fnames[120])
print(fnames[121])
print(fnames[122])
print(fnames[123])

dlist <- list()

for (i in 1:length(fnames)){
    dtemp <- read.csv(fnames[i])
    print('index')
    print(as.character(i))
    print(str(dtemp))
    dlist[[i]] <- dtemp
}

d <- do.call(rbind.data.frame, dlist)

write.table(d, "landscape_distances.csv")
