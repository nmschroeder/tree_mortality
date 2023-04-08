#!/usr/bin/env Rscript
prism_inputs <- commandArgs(trailingOnly = TRUE)
print(prism_inputs)

library(raster)
library(dplyr)
library(sf)
library(sp)
library(ggplot2)
library(stringr)
library(pracma)

theme_set(theme_bw(base_size = 12))

# Read in and split the task array index string
prism_inputs <- paste0(prism_inputs, collapse = " ")
print(prism_inputs)
print(" ")
prism_inputs <- str_split(prism_inputs, pattern = ", ")
print(prism_inputs)
print(" ")
prism_inputs <- do.call(c, prism_inputs)
print(prism_inputs)

# This file creates a time series of PRISM precipitation for our study region
prism_var <- prism_inputs[1]
prism_ylab <- prism_inputs[2]
prism_title <- prism_inputs[3]

# Check output file
print("Variable")
print(prism_var)

print("y-label")
print(prism_ylab)

print("title")
print(prism_title)

# HPC
data_dir <- "/path/to/your/data/directory/"
landsat_dir <- paste0(data_dir, "Landsat")
prism_dir <- paste0(data_dir, "PRISM")
out_dir <- paste0(data_dir, "sierra/prism")
data_dir <- paste0(data_dir, "sierra")


# Set theme for images
theme_set(theme_bw(base_size = 12))

fnames <- list.files(prism_dir, pattern = glob2rx(paste0("*", prism_var, "*.bil")), full.names = TRUE, recursive = TRUE)
fnames <- fnames[10:(length(fnames)-1)]
print(fnames)

N_files <- length(fnames)

print("Read in first PRISM file")

prism <- raster(fnames[1])

#print("Write out PRISM CRS")
#prism_crs <- st_crs(prism)
#print(prism_crs)

lidar_bbox <- read_sf(paste0(data_dir, "/neon_las_extent.shp"))
lidar_bbox <- st_buffer(lidar_bbox, dist = 1000)
lidar_bbox_t <- st_transform(lidar_bbox, crs = 4269)

prism_roi <- crop(prism, lidar_bbox_t)

prism_list <- list()

ptm <- proc.time()
for (i in 1:N_files){
  prism_temp <- raster(fnames[i])
  prism_list[[i]] <- crop(prism_temp, lidar_bbox_t)
}

proc.time() - ptm

prism_ts <- lapply(prism_list, cellStats, stat = 'mean')
prism_ts <- do.call(c, prism_ts)

# Create a time series of the mean precipitation for each month from 1981 through 2021. 
# This way we can look at the timing of the precipitation, the strength of the amplitude, and the
# overall amount over time.

tseq <- linspace(1981 + 10.5/12, 2021 + 9.5/12, N_files)

time_data <- data.frame(tseq = tseq, prism_ts = prism_ts)

ggplot(data = time_data) + geom_point(aes(x = tseq, y = prism_ts))

N_years <- floor(N_files/12)

prism_ts_annual <- vector(length = N_years)

for (i in 1:N_years){
  if (prism_var == "ppt"){
    prism_ts_annual[i] <- sum(prism_ts[(12*i-11):(12*i)])
  } else{
    prism_ts_annual[i] <- mean(prism_ts[(12*i-11):(12*i)])
  }
}

tseq_annual <- seq(1982, 1982+N_years-1)
mean_tseq_annual <- mean(prism_ts_annual)
std_tseq_annual <- std(prism_ts_annual)
median_tseq_annual <- median(prism_ts_annual)
drought_years <- c(2002, 2008, 2009, 2012, 2013, 2014, 2015, 2016, 2021)
drought_tf <- tseq_annual %in% drought_years
tseq_drought <- tseq_annual[drought_tf]

time_data_drought <- data.frame(yrs = tseq_drought, drought = "Drought")

time_data_annual <- data.frame(yrs = tseq_annual, prism = prism_ts_annual)
str(time_data_annual)

#time_data_annual <- dplyr::filter(time_data_annual, yrs >= 2000)
#time_data_drought <- dplyr::filter(time_data_drought, yrs >= 2000)

write.csv(time_data_annual, paste0(out_dir,"/prism_time_data_annual_", prism_var, ".csv"))
write.csv(time_data_drought, paste0(out_dir,"/prism_time_data_drought_", prism_var, ".csv"))

ggplot(data = time_data_annual) + geom_point(aes(x = yrs, y = prism))

## Time Series
ggplot(data = time_data_annual) + 
  geom_rect(data = time_data_drought, 
            aes(xmin=yrs-0.5,xmax=yrs+0.5,ymin=-Inf,ymax=Inf,fill = drought), alpha = 0.2) + 
  geom_col(aes(x = yrs, y = prism)) + 
  geom_hline(aes(yintercept = mean_tseq_annual, linetype = "Mean"), color = "lightcoral", size = 0.8, show.legend = TRUE) +
  #geom_hline(aes(yintercept = median_tseq_annual, linetype = "Median"), color = "lightcoral", size = 0.8, show.legend = TRUE) +
  geom_hline(aes(yintercept = mean_tseq_annual + std_tseq_annual, linetype = "Standard deviation"), color = "lightcoral", size = 0.8, show.legend = TRUE) +
  geom_hline(aes(yintercept = mean_tseq_annual - std_tseq_annual, linetype = "Standard deviation"), color = "lightcoral", size = 0.8, show.legend = FALSE) +
  scale_linetype_manual(name="", values=c(1, 2),
                     guide = guide_legend(override.aes = list(color = c("lightcoral", "lightcoral"))))+
  scale_fill_manual(name = "", values = c("lightcoral", "white"), guide = FALSE) +
  scale_x_continuous(name="Water Years (Oct. 1 Previous Calendar Year - Sep. 30 Water Year)", breaks=seq(1980,2025,5)) + ylab(prism_ylab) +
  xlim(c(2000, 2021)) +
  theme(legend.position="bottom")
 
#  ggtitle(paste0("Soaproot Saddle and Teakettle Regional ", prism_title))
ggsave(paste0("prism_", prism_var, "_timeseries.png"), dpi = 600, width = 6.5, height = 4, unit = "in")

## Tif files of precipitation anomaly and precipitation

prism_ts <- do.call(stack, prism_list)
prism_mean <- calc(prism_ts, mean)*12

# Subtract the background mean from the 2011-2015 water-year mean to find the anomaly
idx <- seq(339, 399) # indices for 2011-2015 water-years

# Subset our raster stack to obtain the 2011-2015 low precipitation water-years
prism_drought <- subset(prism_ts, idx)

# Compute the drought
prism_drought_mean <- calc(prism_drought, mean)*12

# plot(prism_mean)
# plot(prism_drought_mean)
# 
# Compute the anomalies
prism_anomaly <- prism_drought_mean - prism_mean
#prism_ratio_anomaly <- prism_drought_mean/prism_mean
# plot(prism_anomaly)
# plot(prism_ratio_anomaly)

# Let's regrid these to the resolution of Landsat even though it will just be a ball-park estimate
# that does not capture detailed variations

ls8 <- list.files(landsat_dir, 
                  pattern = glob2rx("LC08*_042034_*_B2.TIF"),
                  recursive = TRUE,
                  full.names = TRUE)

# In case this file is located in more than one location, just pick the first one
ls8 <- ls8[1]
ls8_raster <- raster(ls8)
ls8_raster <- crop(ls8_raster, lidar_bbox)

# Project the 4-km rasters onto a 30-meter grid (very rough approximation!)
prism_drought_ngb_30m <- projectRaster(prism_drought_mean, ls8_raster, method = 'ngb')
prism_drought_bil_30m <- projectRaster(prism_drought_mean, ls8_raster, method = 'bilinear')
writeRaster(prism_drought_ngb_30m, paste0(out_dir, "/prism_", prism_var, "_ngb.tif"), overwrite = TRUE)
writeRaster(prism_drought_bil_30m, paste0(out_dir, "/prism_", prism_var, "_bil.tif"), overwrite = TRUE)

# Also project the anomalies
prism_drought_anom_ngb_30m <- projectRaster(prism_anomaly, ls8_raster, method = 'ngb')
prism_drought_anom_bil_30m <- projectRaster(prism_anomaly, ls8_raster, method = 'bilinear')
writeRaster(prism_drought_anom_ngb_30m, paste0(out_dir, "/prism_", prism_var, "_anom_ngb.tif"), overwrite = TRUE)
writeRaster(prism_drought_anom_bil_30m, paste0(out_dir, "/prism_", prism_var, "_anom_bil.tif"), overwrite = TRUE)

# Also save the original resolution versions of these variables
writeRaster(prism_drought_mean, paste0(out_dir,"/prism_", prism_var, ".tif"), overwrite = TRUE)
writeRaster(prism_anomaly, paste0(out_dir,"/prism_", prism_var, "_anom.tif"), overwrite = TRUE)

