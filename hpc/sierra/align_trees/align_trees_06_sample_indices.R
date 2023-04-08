#!/usr/bin/env Rscript
library(sf)
library(dplyr)
data_dir <- "/path/to/your/data/directory/"
out_dir <- paste0(data_dir, "sierra")

shps <- st_read(paste0(out_dir, "/tree_objects_2017_las_intersection.shp"))
n <- dim(shps)[1]
print(n)

a <- seq(1, n, by = 10000)
b <- a[2:length(a)]

a <- a + 1
a[1] <- 1
b <- c(b, n)

idx <- cbind(a,b)

write.table(idx, "sample_tree_inputs.txt", row.names = FALSE, col.names = FALSE, quote = FALSE, sep = ",")
