#==================================================================================================
# FISH 635 Final project: GLM map
# 4/26/2021
# Luke Henslee, College of Fisheries and Ocean Sciences, UAF
#
#==================================================================================================
#NOTES:

#==================================================================================================

# Load packages and set working directory ####
library(tidyverse)
library(ggplot2)
library(bbmle)
library(dplyr)
library(manipulate)
library(tidyverse)
library(dplyr)
library(bbmle)
library(visreg) 
library(ggthemes)
library(ggridges)
library(reshape2)
library(cowplot)
library(ggmap)
library(ggspatial)
library(sf)
library(spData)
library(ggsn)
library(extrafont)
loadfonts(device = "win")

# Set wd
setwd("C:/Users/lhhenslee/Desktop/Luke/School/Coursework/635/data")

# Import data
mcode <- read.csv("M_code_list.csv")

# Build GLM map ####

# Subset data to fish assigned to stocks
stocks <- filter(mcode, mcode$Spawning.group %in% c("4", "5", "6", "S"))

# Subset terminal stocks in Shaktoolik subdistrict
stocks5 <- filter(stocks, stocks$Tag.group == 2)

# Creat bionmial response column 
bi <- vector(length = nrow(stocks5))

for(i in 1:length(bi)) {
  if(stocks5[i,9] == stocks5[i,16]) {
    bi[i] <- 1
  } else {
    bi[i] <- 0
  }
}

stocks5$binomial <- bi

# Build model
bi_model <- glm(binomial ~ Lat + Long, data = stocks5,
                family = binomial(link = "logit"))

# Import reference coords for SD5 shoreline
coords <- c(-161.5146743978081,64.37618605961693,-161.4589981157694,
            64.4124414841602,-161.4059774154973,64.41332881589321,
            -161.2518127915397,64.37292679926715,-161.0004367969463,
            64.23805513721148,-160.963800104766,64.20268008786921,
            -160.9695000648917,64.1864550314435,-160.967901295941,
            64.13213181186877)

# Separate lat and long
lat <- coords[c(F, T)]
long <- coords[c(T, F)]

# Create sequence of lats along path
lat1  <- vector()
for(i in 1:6) {
  lat1 <- append(x = lat1, values = c(seq(lat[i], lat[i+1], length.out = 100)))
}

# Create sequence of longs along path
long1  <- vector()
for(i in 1:6) {
  long1 <- append(x = long1, values = c(seq(long[i], long[i+1], length.out = 100)))
}

# Predict model output values and create dataframe
pred <- predict(bi_model, data.frame(Lat = lat1, Long = long1),
                type = "response")

pred.data <- data.frame(cbind(lat1, long1, pred))

# Create basemap
SD5 <- get_map(location=c(-161.6, 64.1,-160.85, 64.5),
               maptype="toner", source='stamen', crop=TRUE) 

 # Add predictive values
ggmap(SD5) +
  geom_point(data = pred.data, aes(x=long1, 
                                   y=lat1, color = pred), size = 3) +
  scale_color_viridis(discrete = F, option = "C", name = "Likelihood",
                      ) +
  xlab(NULL) + ylab(NULL) +
  annotation_north_arrow(style = north_arrow_nautical()) + 
  coord_sf(crs = 4326) + 
  annotation_scale(style = "ticks", location = "br", height = unit(2, "mm"), 
                   tick_height = 1, text_cex = 1, line_width = 2) +
  theme(axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0), colour = "black"),
        axis.title.x = element_text(size = 14, margin = margin(t = 0, r = 0, b = 0, l = 0), colour = "black"),
        text = element_text(family = "Times New Roman")) +
  theme (panel.border = element_rect (fill = "transparent", color = "grey20", size = 2)) +
  theme (plot.margin = unit(c(0,0,.5,0), "cm"))

# Try some other options ####

# Use "rnaturalearth" to make map
library(rnaturalearth)
library(rnaturalearthdata)

world <- ne_countries(scale = "medium", returnclass = "sf")

theme_set(theme_bw())

usa <- subset(world, admin == "United States of America")

ggplot(data = usa) +
    geom_sf(fill = "cornsilk") +
    coord_sf(xlim = c(-161.6, -160.8), 
             ylim = c(64.1, 64.5))

# Not a lot of resolution in that map

# Try stamen maps again
alaska.box <- c(left = -161.6, bottom = 64.1, right = -160.8, top = 64.5)
alaska.base <- ggmap::get_stamenmap (bbox = alaska.box, maptype = "toner-lite", 
                                     where = "cache")

ggmap(alaska.base)
  # I like it

# See what 'toner-lite' looks like with predictive points
ggmap(alaska.base) +
  geom_point(data = pred.data, aes(x=long1, 
                                   y=lat1, color = pred), size = 4) +
  scale_color_viridis(discrete = F, option = "C", name = "Likelihood", 
                      breaks = c(0.1, 0.3, 0.5)) +
  scale_x_continuous(breaks = c(-161.4, -161), expand = c(0,0)) +
  scale_y_continuous(breaks = c(64.2, 64.3, 64.4), expand = c(0,0)) + 
  xlab(NULL) + ylab(NULL) +
  annotation_north_arrow(style = north_arrow_nautical()) + 
  coord_sf(crs = 4326) + 
  annotation_scale(style = "ticks", location = "br", height = unit(2, "mm"), 
                   tick_height = 1, text_cex = 1, line_width = 2) +
  theme(axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0), colour = "black"),
        axis.title.x = element_text(size = 14, margin = margin(t = 0, r = 0, b = 0, l = 0), colour = "black"),
        text = element_text(family = "Times New Roman")) +
  theme (panel.border = element_rect (fill = "transparent", color = "grey20", size = 2)) +
  theme (plot.margin = unit(c(0,0,.5,0), "cm")) + 
  theme(legend.position = c(0.8, 0.7), legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))

  # I like this one

# Trying some other stuff
library(maptools)
library(shapefiles)
library(rgdal)
library(ggplot2)
library(rgeos)

# Projection and datum setup
wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
GlobalCoastline <- readOGR("ne_50m_ocean.shp")

install.packages("tigris")
library(tigris)
?tigris

manhattan_roads <- roads("NY", "New York")

ggplot(manhattan_roads) + 
  geom_sf() + 
  theme_void()

alaskacoast <- coastline()

library(oce)
alaskashore <- read.coastline.shapefile(
  "ne_10m_admin_0_countries.zip",
  lonlim = c(-161.6, -160.8),
  latlim = c(64.1, 64.5)
)
plot(alaskashore, latitudelim = c(64.1, 64.5), longitudelim = c(-161.6, -160.8))


# Creat finalized map with 'toner-lite' ####

# Creat basemap
alaska.box <- c(left = -161.6, bottom = 64.1, right = -160.8, top = 64.5)
alaska.base <- ggmap::get_stamenmap (bbox = alaska.box, maptype = "toner-lite", 
                                     where = "cache")


# Create GLM map with predictive points
glm <- ggmap(alaska.base) +
  geom_point(data = pred.data, aes(x=long1, 
                                   y=lat1, color = pred), size = 2.5) +
  scale_color_viridis(discrete = F, option = "C", name = "Likelihood", 
                      breaks = c(0.1, 0.3, 0.5)) +
  scale_x_continuous(breaks = c(-161.4, -161), expand = c(0,0)) +
  scale_y_continuous(breaks = c(64.2, 64.3, 64.4), expand = c(0,0)) + 
  xlab(NULL) + ylab(NULL) +
  annotation_north_arrow(style = north_arrow_nautical()) + 
  coord_sf(crs = 4326) + 
  annotation_scale(style = "ticks", location = "br", height = unit(2, "mm"), 
                   tick_height = 1, text_cex = 1, line_width = 2) +
  theme(axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0), colour = "black"),
        axis.title.x = element_text(size = 14, margin = margin(t = 0, r = 0, b = 0, l = 0), colour = "black"),
        text = element_text(family = "Times New Roman")) +
  theme (panel.border = element_rect (fill = "transparent", color = "grey20", size = 2)) +
  theme (plot.margin = unit(c(0,0,.5,0), "cm")) + 
  theme(legend.position = c(0.8, 0.7), legend.text = element_text(size = 12),
        legend.title = element_text(size = 12), 
        legend.spacing.x = unit(0.5, 'cm'))
glm

# Create inset map
alaska.box2 <- c(left = -168.5, bottom = 62, right = -155, top = 69)
alaska.base2 <- ggmap::get_stamenmap (bbox = alaska.box2, zoom = 5, maptype = "toner-background", 
                                      where = "cache")
alaska <- ggmap(alaska.base2) +
  geom_rect(xmin = -161.8, xmax = -160.6, ymin = 63.85, ymax = 64.55, fill = "transparent", 
            color = "red", size = 1) + theme_void() + 
  theme (panel.border = element_rect (fill = "transparent", color = "grey20", size = 1)) +
  theme (plot.margin = unit(c(0,0,0,0), "cm"))
alaska

# Combine maps
GLM_map <- ggdraw() +
  draw_plot(glm) +
  draw_plot(alaska, x = 0.3, y = 0.1, width = 0.35, height = 0.35)

GLM_map

# Save image
dev.new (width = 3.53, height = 5.62, units = "in", noRStudioGD = T); last_plot()
ggsave ("GLM_map.tiff", width = dev.size()[1], height = dev.size()[2], dpi = 600); dev.off()
