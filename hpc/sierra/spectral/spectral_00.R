#!/usr/bin/env Rscript
library(dplyr)

s <- c("SOAP", "TEAK")
y <- c("2013", "2017", "2018", "2019", "2021")

sites <- c()
years <- c()

for (i in s){
  for (j in y){
    sites <- c(sites, i)
    years <- c(years, j)
  }
}

d <- data.frame(sites = sites, years = years)

write.table(d, "neon_sites_years.txt", sep = ",", quote = FALSE, col.names = FALSE, row.names = FALSE)
