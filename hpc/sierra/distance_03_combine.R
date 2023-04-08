#!/usr/bin/env Rscript

library(dplyr)
library(stringr)

# HPC
data_dir <- "/path/to/your/data/directory/"
data_dir <- paste0(data_dir, "sierra/landscape")
fnames <- list.files(path = data_dir, 
                     pattern = glob2rx("landscape_distance_*.csv"), 
                     full.names = TRUE)
setwd(data_dir)

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
