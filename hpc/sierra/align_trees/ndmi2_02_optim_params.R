#!/usr/bin/env Rscript

library(stringr)

spectral_idx <- "ndmi2"
data_dir <- "/path/to/your/data/directory/"
data_dir <- paste0(data_dir, "sierra/")

params <- read.table(paste0(data_dir,spectral_idx,"_optim_params.txt"))
print(params)

yrs <- c(2013, 2017, 2018, 2019, 2021)

txt <- list()

for (i in 1:length(yrs)){
  txt[[i]] <- c(params$V1, yrs[i])
}

txt <- do.call(rbind, txt)

write.table(txt, paste0(spectral_idx,"_optim_params_years.txt"), quote = FALSE, row.names = FALSE, col.names = FALSE)
