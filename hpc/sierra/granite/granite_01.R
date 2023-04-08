#!/usr/bin/env Rscript
library(dplyr)
data_dir <- "/path/to/your/data/directory/"
data_dir <- paste0(data_dir, "NEON/lidar/chm")

fnames <- list.files(path = data_dir, pattern = ".tif$", full.names = TRUE, recursive = TRUE)

fname_df <- data.frame(fnames = fnames)

write.table(fname_df, "neon_chm_files.txt", col.names = FALSE, quote = FALSE, row.names = FALSE)
