#!/usr/bin/env Rscript

library(dplyr)

# Look for all the hdf5 files downloaded from the NEON data portal for TEAK
fnames <- list.files("/dfs4/jranders_lab/users/hemmingn/NEON/spectral/2017/TEAK", pattern=glob2rx("*.h5"), recursive = TRUE)

# Create a dataframe to write to a text file
df <- data.frame(fnames=fnames)

# Write textt file with a file name per line to feed into another script
write.table(df, "teak_2017_fnames.txt", quote=FALSE,row.names=FALSE, col.names=FALSE)
