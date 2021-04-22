#==================================================================================================
# FISH 635 Homework #10: Figure 1
# 4/21/2021
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

# Set working directory
setwd("C:/Users/lhhenslee/Desktop/Luke/School/Coursework/635/data")

# We are making a figure of our study area for the ice monitoring project in 2021
  # Fist, make a tibble of our sites

sites <- tibble (
  long = c(-165.736733, -165.398033),
  lat = c(64.516933, 64.47955), 
  names = c("West", "Subsistence zone")
)

# For reference, we'll make a map of Alaska

alaska.box <- c(left = -169.3, bottom = 61.5, right = -152.4, top = 69)
alaska.base <- ggmap::get_stamenmap (bbox = alaska.box, zoom = 7, maptype = "terrain-background", 
                                     where = "cache")
alaska <- ggmap(alaska.base) +
  geom_rect(xmin = -167, xmax = -163.5, ymin = 64, ymax = 65.5, fill = "transparent", 
            color = "grey20", size = 1) + theme_void() + 
  theme (panel.border = element_rect (fill = "transparent", color = "grey20", size = 2)) +
  theme (plot.margin = unit(c(0,0,0,0), "cm"))
alaska

# Zoom in a little more...

alaska.box2 <- c(left = -167, bottom = 64, right = -163, top = 65.5)
alaska.base2 <- ggmap::get_stamenmap (bbox = alaska.box2, zoom = 9, maptype = "terrain-background", 
                                     where = "cache")
alaska2 <- ggmap(alaska.base2) +
  geom_point (data = sites, aes (x = long, y = lat), size = 2, color = "grey20") + 
  geom_rect(xmin = -166.25, xmax = -165, ymin = 64.3, ymax = 64.7, fill = "transparent", 
            color = "grey20", size = 1) + 
  xlab(NULL) + ylab (NULL) + 
  scale_x_continuous (breaks = c(-166, -165, -164), 
                      labels = c("166°W", "165°W", "164°W"), 
                      expand = c(0,0)) + 
  scale_y_continuous (breaks = c(64.25, 64.75, 65.25), 
                      labels = c ("64.25°N", "64.75°N", "65.25°N"), 
                      expand = c(0,0)) + 
  theme (panel.border = element_rect (fill = "transparent", color = "grey20", size = 2)) +
  theme (plot.margin = unit(c(0,0,0,0), "cm"))
alaska2

# Now we will download the mapbase

sitebox.coords <- c(left = -166.25, bottom = 64.3, right = -165, top = 64.7)
sitebox <- ggmap::get_stamenmap(bbox = sitebox.coords, zoom = 11, maptype = "terrain-background", where = "cache")
sitebox.map <- ggmap(sitebox)
sitebox.map

boxmap <- ggmap(sitebox) +
  geom_point(data = sites, aes(x = long, y = lat), size = 2, color = "grey20") +
  theme_void() + theme(panel.border = element_rect (fill = "transparent", color = "grey20", size = 1.5)) +
  theme (plot.margin = unit(c (0,0,0,0), "cm")) + coord_sf(crs = 4326) + 
  annotation_scale(style = "ticks", location = "br", height = unit(2, "mm"), tick_height = 1, text_cex = 1, line_width = 2)
boxmap
