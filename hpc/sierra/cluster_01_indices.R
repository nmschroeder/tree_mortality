library(sf)
library(dplyr)
out_dir <- "/dfs5/bio/hemmingn/sierra"
shps <- st_read("/dfs5/bio/hemmingn/sierra/align_trees/tree_locations_las_intersection.shp")
n <- dim(shps)[1]

a <- seq(1, n, by = round(n/30))
b <- a[2:length(a)]

a <- a + 1
a[1] <- 1
b <- c(b, n)

idx <- cbind(a,b)

write.table(idx, "cluster_02_inputs.txt", row.names = FALSE, col.names = FALSE, quote = FALSE, sep = ",")
