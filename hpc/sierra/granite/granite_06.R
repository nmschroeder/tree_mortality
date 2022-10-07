#!/usr/bin/env Rscript
library(dplyr)

data_dir <- "/dfs4/jranders_lab/users/hemmingn/sierra"

fnames <- list.files(path = data_dir, pattern = glob2rx("granite*.tif"), full.names = TRUE, recursive = TRUE)

fname_df <- data.frame(fnames = fnames)

write.table(fname_df, "granite_files.txt", col.names = FALSE, quote = FALSE, row.names = FALSE)
