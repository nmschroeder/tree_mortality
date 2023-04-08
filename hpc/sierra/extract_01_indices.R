library(sf)
library(dplyr)
data_dir <- "/path/to/your/data/directory/"
shps <- st_read(paste0(data_dir, "sierra/tree_locations_las_intersection.shp"))
n <- dim(shps)[1]

a <- seq(1, n, by = round(n/250))
b <- a[2:length(a)]

a <- a + 1
a[1] <- 1
b <- c(b, n)

idx <- cbind(a,b)

write.table(idx, "extract_03_inputs.txt", row.names = FALSE, col.names = FALSE, quote = FALSE, sep = ",")
