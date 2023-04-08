# We downloaded Landsat Collection 2 Level 2 Tier 1 surface reflectance data from the USGS Earthdata Explorer
# website for path 42, row 34 for the years 2013 and 2017. Then, we ran the scripts dNDMI01.R and dNDMI02.R located
# in our tree_mortality directory at hpc/sierra/spectral to compute the landsat20130915.tif and landsat20170915.tif
# files referenced in this script. We stored these files in a directory called ../data relative to this directory.

library(dplyr)
library(rgdal)
library(raster)
library(ggplot2)
library(colorspace)
hcl_palettes(plot = TRUE)

theme_set(theme_bw(base_size=10))

landsat_dir <- "../data"

trees_per_pixel <- raster("data/deliverables/raster/trees_per_pixel.tif")
data_mask <- trees_per_pixel
data_mask[trees_per_pixel>=4]<-1
data_mask[trees_per_pixel<4] <- 0

ls_2013 <- raster("../data/landsat20130915.tif", band = 4)
ls_2017 <- raster("../data/landsat20170915.tif", band = 4)
ls_2017 <- ls_2017 - ls_2013

# Read in data for Soaproot Saddle
soap_2017 <- raster("data/deliverables/raster/soap_mortality_2017.tif")
soap_2013 <- raster("data/deliverables/raster/soap_mortality_2013.tif")
soap_2017 <- soap_2017-soap_2013
soap_ls_2017 <- crop(ls_2017, extent(soap_2017))
soap_mask_2017 <- data_mask %>% crop(extent(soap_2017))
trees_soap <- crop(trees_per_pixel, extent(soap_2017))

soap_2017_df <- soap_2017 %>% as.data.frame()
soap_ls_2017_df <- soap_ls_2017 %>% as.data.frame()
soap_mask_2017 <- soap_mask_2017 %>% as.data.frame()
trees_soap_df <- trees_soap %>% as.data.frame()

# Read in data for teakettle
teak_2017 <- raster("data/deliverables/raster/teak_mortality_2017.tif")
teak_2013 <- raster("data/deliverables/raster/teak_mortality_2013.tif")
teak_2017 <- teak_2017 - teak_2013
teak_ls_2017 <- crop(ls_2017, extent(teak_2017))
teak_mask_2017 <- data_mask %>% crop(extent(teak_2017))

teak_2017_df <- teak_2017 %>% as.data.frame()
teak_ls_2017_df <- teak_ls_2017 %>% as.data.frame()
teak_mask_2017 <- teak_mask_2017 %>% as.data.frame()
trees_teak <- crop(trees_per_pixel, extent(teak_2017))
trees_teak_df <- trees_teak %>% as.data.frame()

soapdf2017 <- cbind.data.frame(soap_2017_df, soap_ls_2017_df, soap_mask_2017, trees_soap_df)
colnames(soapdf2017) <- c("mortality", "dNDMI", "mask", "count")
teakdf2017 <- cbind.data.frame(teak_2017_df, teak_ls_2017_df, teak_mask_2017, trees_teak_df)
colnames(teakdf2017) <- c("mortality", "dNDMI", "mask", "count")
df2017 <- rbind.data.frame(soapdf2017, teakdf2017)
idx <- which(!is.na(df2017$mortality))
df2017 <- df2017[idx,]

idx <- which(df2017$mask==1)

m <- lm(dNDMI ~ mortality, data = df2017[idx,])

summary(m)

msg <- expression("R"^2*"=0.37")

ggplot(df2017[idx,]) + geom_point(mapping = aes(x = mortality, y = dNDMI)) + 
  geom_abline(slope = coef(m)[2], intercept = coef(m)[1], color = 'red') +
  annotate("text", x = 0.75, y = 0.13, label = msg, parse = TRUE) +
  ylab("Change in NDMI between 2011 and 2017") + xlab("Ratio of mortality")


xval_sort <- function(x){
  xvals <- seq(0, 1, by = 0.1)
  idx <- which.min(abs(x - xvals))
  return(xvals[idx])
}

xvals <- sapply(df2017$mortality, xval_sort) %>% as.factor()

# Write a for-loop to count the number of values in each box
xvals_unique <- sort(unique(xvals))
n <- length(xvals_unique)
boxcount <- vector(mode = "numeric", length = length(xvals))
for (i in 1:n){
  idx <- xvals == xvals_unique[i]
  boxcount_i <- sum(idx)
  boxcount[idx] <- boxcount_i
}

df2017 <- mutate(df2017, xvals = xvals, boxcount = boxcount)
idx <- which(df2017$mask==1)

# Note that the first categorical variable is 0.1 but is represented numerically as 1, so all
# the x-values need to be transformed so that instead of x -> y we have 10x -> y
ggplot(data = df2017[idx,]) + 
  geom_boxplot(mapping = aes(x = xvals, y = dNDMI, fill = boxcount)) + 
  #geom_point(mapping = aes(x = mortality*10, y = dNDMI)) + 
  
  geom_abline(slope = coef(m)[2]/10, intercept = coef(m)[1], color = 'darkred') +
  annotate("text", x = 9, y = 0.1, label = msg, parse = TRUE, color = "darkred") +
  scale_fill_continuous_sequential(name = "Count", palette = "Inferno", rev = TRUE) +
  xlab("Change in fractional tree mortality (2013-2017 for 4 trees or more)") + 
  ylab("Change in NDMI (2013-2017)") +
  theme(legend.position = "bottom", legend.key.width = unit(1.5, 'cm'), legend.key.height = unit(0.5, "cm"))
  
ggsave("figures/Figure_S12_dNDMI_trend.pdf", dpi=600, width = 7.5, height = 7.5, unit = "in") 

# Parameters

# The slope is divided by 10 for plotting due to x-value issues from boxplot
# (Each unit is treated as 1 instead of 0.1)
coef(m)[2]
coef(m)[1]
