#!/usr/bin/env Rscript
library(dplyr)

data_dir <- "/dfs4/jranders_lab/users/hemmingn/NEON/spectral"

fnames <- list.files(path = data_dir, pattern = ".h5$", full.names = TRUE, recursive = TRUE)

fname_df <- data.frame(fnames = fnames)

write.table(fname_df, "neon_spectral_h5files.txt", col.names = FALSE, quote = FALSE, row.names = FALSE)
