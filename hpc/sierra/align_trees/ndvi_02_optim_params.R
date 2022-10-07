#!/usr/bin/env Rscript

library(stringr)

params <- read.table("/dfs5/bio/hemmingn/sierra/align_trees/optim_params.txt")
print(params)

yrs <- c(2013, 2017, 2018, 2019, 2021)

txt <- list()

for (i in 1:length(yrs)){
  txt[[i]] <- c(params$V1, yrs[i])
}

txt <- do.call(rbind, txt)

write.table(txt, "optim_params_years.txt", quote = FALSE, row.names = FALSE, col.names = FALSE)
