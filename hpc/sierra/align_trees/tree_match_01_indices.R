#!/usr/bin/env Rscript

library(dplyr)
library(sf)

stovall_trees <- read.csv("/dfs4/jranders_lab/users/hemmingn/sierra/stovall/ALLtrees_v2.csv")

n <- dim(stovall_trees)[1]

a <- seq(1, n, by = round(n/250))
b <- a[2:length(a)]

a <- a + 1
a[1] <- 1
b <- c(b, n)

idx <- cbind(a,b)

write.table(idx, "tree_match_02_inputs.txt", row.names = FALSE, col.names = FALSE, quote = FALSE, sep = ",")
