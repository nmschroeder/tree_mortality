library(ggplot2)
library(dplyr)
library(raster)
library(sp)
library(sf)
library(gridExtra)
library(cowplot)
library(colorspace)
library(ggpattern)
hcl_palettes(plot = TRUE)

theme_set(theme_bw(base_size = 12))

# Replace with path to USFS Fire Perimeters
fire_perim <- "~/Data/S_USA/"

# Read in wildfire perimeters from USFS
wildfires_all <- read_sf(paste0(fire_perim, "S_USA.FinalFirePerimeter.shp"))
wildfires <- dplyr::filter(wildfires_all, UNIQFIREID %in% c("2021-CASNF-000717", "2020-CASNF-001391"))
wildfires <- st_transform(wildfires, crs = 32611)

p1 <- st_point(x = c(298500, 4099200))
p2 <- st_point(x = c(298500, 4101400))
p3 <- st_point(x = c(300000, 4101400))
p4 <- st_point(x = c(300000, 4099200))

pgon <- st_polygon(x = list(rbind(p1, p2, p3, p4, p1)))
pgon <- pgon %>% st_sfc(crs = 32611)
st_write(pgon, "data/intermediate/fire_edit_extent.shp")

soap_2013 <- raster("data/deliverables/raster/soap_mortality_2013.tif")
soap_2017 <- raster("data/deliverables/raster/soap_mortality_2017.tif")
soap_2018 <- raster("data/deliverables/raster/soap_mortality_2018.tif")
soap_2019 <- raster("data/deliverables/raster/soap_mortality_2019.tif")
soap_2021 <- raster("data/deliverables/raster/soap_mortality_2021.tif")

teak_2013 <- raster("data/deliverables/raster/teak_mortality_2013.tif")
teak_2017 <- raster("data/deliverables/raster/teak_mortality_2017.tif")
teak_2018 <- raster("data/deliverables/raster/teak_mortality_2018.tif")
teak_2019 <- raster("data/deliverables/raster/teak_mortality_2019.tif")
teak_2021 <- raster("data/deliverables/raster/teak_mortality_2021.tif")

soap_0 <- (soap_2017 - soap_2013) %>% rasterToPoints(spatial=TRUE) %>% data.frame()
soap_1 <- soap_2017 %>% rasterToPoints(spatial=TRUE) %>% data.frame()
soap_2 <- (soap_2018 - soap_2017) %>% rasterToPoints(spatial=TRUE) %>% data.frame()
soap_3 <- (soap_2019 - soap_2018) %>% rasterToPoints(spatial=TRUE) %>% data.frame()
soap_4 <- (soap_2021 - soap_2019) %>% rasterToPoints(spatial=TRUE) %>% data.frame()

teak_0 <- (teak_2017 - teak_2013) %>% rasterToPoints(spatial=TRUE) %>% data.frame()
teak_1 <- teak_2017%>%rasterToPoints(spatial=TRUE) %>% data.frame()
teak_2 <- (teak_2018 - teak_2017) %>% rasterToPoints(spatial=TRUE) %>% data.frame()
teak_3 <- (teak_2019 - teak_2018) %>% rasterToPoints(spatial=TRUE) %>% data.frame()
teak_4 <- (teak_2021 - teak_2019) %>% rasterToPoints(spatial=TRUE) %>% data.frame()

writeRaster(teak_2021-teak_2019, 'data/deliverables/raster/teak_mortality_2021-2019.tif', overwrite = TRUE)
writeRaster(soap_2021-soap_2019, 'data/deliverables/raster/soap_mortality_2021-2019.tif', overwrite = TRUE)

p0 <- ggplot()+geom_raster(data=soap_0, mapping=aes(x=x,y=y,fill=layer)) + scale_fill_continuous_diverging(palette = 'Blue-Red 3', limits = c(-1, 1)) +
  theme(legend.position = "top", legend.key.width = unit(2, 'cm'), legend.title = element_blank()) + 
  ylim(c(4098000, 4104500)) +
  xlim(c(295000, 302500))
legend <- get_legend(p0)

p1 <- ggplot()+geom_raster(data=soap_1, mapping=aes(x=x,y=y,fill=layer)) + scale_fill_continuous_diverging(palette = 'Blue-Red 3', limits = c(-1, 1)) + coord_sf(datum = st_crs(32611)) +
  labs(x = "", y = "Northing (m)", title = "a. 2017") +
  scale_x_continuous(breaks = c(295000, 297000, 299000, 301000),
                     labels = c("295000", "297000", "299000","301000"), limits = c(295000, 302500)) +
  scale_y_continuous(breaks = c(4098000, 4100000, 4102000, 4104000),
                     labels = c("4098000", "4100000", "4102000", "4104000"), limits = c(4098000, 4104500)) +
  theme(legend.position="none", axis.ticks.length=unit(-0.1, "cm"))

p2 <- ggplot()+geom_raster(data=soap_2, mapping=aes(x=x,y=y,fill=layer)) + scale_fill_continuous_diverging(palette = 'Blue-Red 3', limits = c(-1, 1)) + coord_sf(datum = st_crs(32611)) +
  labs(x = "", y = "", title = "b. 2017-2018") +
  scale_x_continuous(breaks = c(295000, 297000, 299000, 301000),
                     labels = c("295000", "297000", "299000","301000"), limits = c(295000, 302500)) +
  scale_y_continuous(breaks = c(4098000, 4100000, 4102000, 4104000),
                     labels = c("4098000", "4100000", "4102000", "4104000"), limits = c(4098000, 4104500)) +
  theme(legend.position="none", axis.ticks.length=unit(-0.1, "cm"))

p3 <- ggplot()+geom_raster(data=soap_3, mapping=aes(x=x,y=y,fill=layer)) + scale_fill_continuous_diverging(palette = 'Blue-Red 3', limits = c(-1, 1)) + coord_sf(datum = st_crs(32611)) +
  labs(x="Easting (m)", y = "Northing (m)", title = "c. 2018-2019") +
  scale_x_continuous(breaks = c(295000, 297000, 299000, 301000),
                     labels = c("295000", "297000", "299000","301000"), limits = c(295000, 302500)) +
  scale_y_continuous(breaks = c(4098000, 4100000, 4102000, 4104000),
                     labels = c("4098000", "4100000", "4102000", "4104000"), limits = c(4098000, 4104500)) +
  theme(legend.position="none", axis.ticks.length=unit(-0.1, "cm"))

p4 <- ggplot()+ 
  geom_raster(data=soap_4, mapping=aes(x=x,y=y,fill=layer)) +
  geom_sf(data = wildfires, fill = NA, color = "black", inherit.aes = FALSE) +
  scale_fill_continuous_diverging(palette = 'Blue-Red 3', limits = c(-1, 1)) +
  coord_sf(datum = st_crs(32611)) +
  labs(x="Easting (m)", y = "", title = "d. 2019-2021") +
  scale_x_continuous(breaks = c(295000, 297000, 299000, 301000),
                     labels = c("295000", "297000", "299000","301000"), limits = c(295000, 302500)) +
  scale_y_continuous(breaks = c(4098000, 4100000, 4102000, 4104000),
                     labels = c("4098000", "4100000", "4102000", "4104000"), limits = c(4098000, 4104500)) +
  theme(legend.position="none", axis.ticks.length=unit(-0.1, "cm"))


soap <- grid.arrange(p1, p2, p3, p4, legend, ncol=2, nrow = 3, 
             layout_matrix = rbind(c(1,2),c(3,4),c(5,5)),
             widths = c(2.7, 2.7), heights = c(2.5, 2.5, 0.4))
soap
ggsave("figures/Figure_S6_soap_grid.pdf", plot=soap, width = 7.5, height = 6, unit = "in", dpi = 600)

# Teakettle

p0 <- ggplot()+geom_raster(data=teak_0, mapping=aes(x=x,y=y,fill=layer)) + scale_fill_continuous_diverging(palette = 'Blue-Red 3', limits = c(-1, 1)) +
  theme(legend.position = "top", legend.key.width = unit(2, 'cm'), legend.title = element_blank()) +
  ylim(c(4091000, 4107000)) +
  xlim(c(314000, 326000)) +
  coord_sf(datum = st_crs(32611))
legend <- get_legend(p0)

p1 <- ggplot()+geom_raster(data=teak_1, mapping=aes(x=x,y=y,fill=layer)) + scale_fill_continuous_diverging(palette = 'Blue-Red 3', limits = c(-1, 1)) +
  scale_x_continuous(breaks = c(315000, 320000, 325000),
                     labels = c("315000", "320000","325000"), limits = c(314000, 326000)) +
  scale_y_continuous(breaks = c(4095000, 4100000, 4105000),
                     labels = c("4095000", "4100000", "4105000"), limits = c(4091000, 4107000)) +
  labs(x = "", y = "Northing (m)", title = "a. 2017") +
  theme(legend.position="none", axis.ticks.length=unit(-0.1, "cm")) +
  coord_sf(datum = st_crs(32611))

p2 <- ggplot()+geom_raster(data=teak_2, mapping=aes(x=x,y=y,fill=layer)) + scale_fill_continuous_diverging(palette = 'Blue-Red 3', limits = c(-1, 1)) +
  scale_x_continuous(breaks = c(315000, 320000, 325000),
                     labels = c("315000", "320000","325000"), limits = c(314000, 326000)) +
  scale_y_continuous(breaks = c(4095000, 4100000, 4105000),
                     labels = c("4095000", "4100000", "4105000"), limits = c(4091000, 4107000)) +
  labs(x = "", y = "", title = "b. 2017-2018") +
  theme(legend.position="none", axis.ticks.length=unit(-0.1, "cm")) +
  coord_sf(datum = st_crs(32611))

p3 <- ggplot()+geom_raster(data=teak_3, mapping=aes(x=x,y=y,fill=layer)) + scale_fill_continuous_diverging(palette = 'Blue-Red 3', limits = c(-1, 1)) +
  scale_x_continuous(breaks = c(315000, 320000, 325000),
                     labels = c("315000", "320000","325000"), limits = c(314000, 326000)) +
  scale_y_continuous(breaks = c(4095000, 4100000, 4105000),
                     labels = c("4095000", "4100000", "4105000"), limits = c(4091000, 4107000)) +
  labs(x="Easting (m)", y = "Northing (m)", title = "c. 2018-2019") +
  theme(legend.position="none", axis.ticks.length=unit(-0.1, "cm")) +
  coord_sf(datum = st_crs(32611))

p4 <- ggplot()+geom_raster(data=teak_4, mapping=aes(x=x,y=y,fill=layer)) + scale_fill_continuous_diverging(palette = 'Blue-Red 3', limits = c(-1, 1)) +
  scale_x_continuous(breaks = c(315000, 320000, 325000),
                     labels = c("315000", "320000","325000"), limits = c(314000, 326000)) +
  scale_y_continuous(breaks = c(4095000, 4100000, 4105000),
                     labels = c("4095000", "4100000", "4105000"), limits = c(4091000, 4107000)) +
  labs(x="Easting (m)", y = "", title = "d. 2019-2021") +
  theme(legend.position="none", axis.ticks.length=unit(-0.1, "cm")) +
  coord_sf(datum = st_crs(32611))


g <- grid.arrange(p1, p2, p3, p4, legend, ncol=2, nrow = 3, 
             layout_matrix = rbind(c(1,2),c(3,4),c(5,5)),
             widths = c(2.7, 2.7), heights = c(2.5, 2.5, 0.4))
g
ggsave("figures/Figure_S7_teak_grid.pdf", plot = g, width = 7.5, height = 9, unit = "in", dpi = 600)

