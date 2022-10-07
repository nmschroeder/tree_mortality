library(dplyr)
library(sf)
library(raster)
library(concaveman)
library(tidyr)

data_dir <- "/Volumes/LaCie/sierra/"
data_dir <- "/dfs4/jranders_lab/users/hemmingn/sierra/align_trees/"

setwd(data_dir)
trees2017 <- read_sf(paste0(data_dir,"tree_objects_2017_las_intersection.shp"))

str(trees2017)

site <- ifelse(trees2017$xlas2017>303000, "TEAK", "SOAP")
x <- trees2017$xlas2017
y <- trees2017$ylas2017

trees2017 <- mutate(trees2017, site=site, x=x, y=y)

xy <- dplyr::select(trees2017, x, y, xlas2017, ylas2017, site) %>% st_drop_geometry() %>% drop_na() %>% as.data.frame()

xy_sf <- st_as_sf(x=xy, coords=c('xlas2017', 'ylas2017'), dim = "XY", crs = 32611)

# Here, let's loop through sets of indices of values to save on memory
xy_soap <- dplyr::filter(xy_sf, site == "SOAP", !is.na(x))
ext_soap <- extent(xy_soap)
xy_teak <- dplyr::filter(xy_sf, site == "TEAK", !is.na(x))
ext_teak <- extent(xy_teak)
rm(trees2017, xy, xy_sf)
N_gridx <- 100
N_gridy <- N_gridx

# Create boundaries
x_lb <- seq(floor(ext_soap@xmin), ceiling(ext_soap@xmax),length.out=N_gridx)
y_lb <- seq(floor(ext_soap@ymin), ceiling(ext_soap@ymax),length.out=N_gridy)

# Upper bounds
x_ub <- x_lb[2:length(x_lb)]
y_ub <- y_lb[2:length(y_lb)]

# Lower bounds
x_lb <- x_lb[1:length(x_ub)]
y_lb <- y_lb[1:length(y_ub)]

regions <- list()
count <- 1

for (i in 1:length(x_lb)){
  for (j in 1:length(y_lb)){
    idx <- which(as.numeric(xy_soap$x) >= x_lb[i] & as.numeric(xy_soap$x) < x_ub[i] & as.numeric(xy_soap$y) >= y_lb[j] & as.numeric(xy_soap$y) < y_ub[j])
    if (length(idx)>4){
      pts <- xy_soap[idx,]
      regions[[count]] <- concaveman(pts, concavity = 10)
      count <- count+1
    }
  }
}

region <- do.call(rbind.data.frame, regions)
st_write(region, "soap_region.shp", delete_dsn = TRUE)

# Lower Teakettle

# Create boundaries
x_lb <- seq(floor(ext_teak@xmin), ceiling(ext_teak@xmax),length.out=N_gridx)
y_lb <- seq(floor(ext_teak@ymin), ceiling(ext_teak@ymax),length.out=N_gridy)

# Upper bounds
x_ub <- x_lb[2:length(x_lb)]
y_ub <- y_lb[2:length(y_lb)]

# Lower bounds
x_lb <- x_lb[1:length(x_ub)]
y_lb <- y_lb[1:length(y_ub)]

regions <- list()
count <- 1

for (i in 1:length(x_lb)){
  for (j in 1:length(y_lb)){
    idx <- which(as.numeric(xy_teak$x) >= x_lb[i] & as.numeric(xy_teak$x) < x_ub[i] & as.numeric(xy_teak$y) >= y_lb[j] & as.numeric(xy_teak$y) < y_ub[j])
    if (length(idx)>4){
      pts <- xy_teak[idx,]
      regions[[count]] <- concaveman(pts, concavity = 10)
      count <- count+1
    }
  }
}

region <- do.call(rbind.data.frame, regions)
st_write(region, "teak_region.shp", delete_dsn = TRUE)



