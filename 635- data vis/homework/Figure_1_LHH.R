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
library(sf)
library(spData)
library(ggsn)
library(extrafont)
loadfonts(device = "win")

# We are making a figure of our study area for the ice monitoring project in 2021
  # Fist, make a tibble of our sites
sites <- tibble (
  long = c(-165.736733, -165.398033),
  lat = c(64.516933, 64.47955), 
  names = c("West", "Subsistence zone")
)

# Main map
alaska.box <- c(left = -167, bottom = 64, right = -163, top = 65.5)
alaska.base <- ggmap::get_stamenmap (bbox = alaska.box, zoom = 9, maptype = "terrain-background", 
                                      where = "cache")

sites.map <- ggmap(alaska.base) +
  geom_point(data = sites, aes (x = long, y = lat), size = 2, color = "grey20") + 
  annotate("text", x = -166.3, y = 64.5, label = "West beach", size = 5) +
  annotate("text", x = -165.1, y = 64.35, label = "Subsistence\nzone", size = 5) +
  xlab(NULL) + ylab (NULL) + 
  scale_x_continuous (breaks = c(-166, -165, -164), 
                      labels = c("166\u00B0W", "165\u00B0W", "164\u00B0W"), 
                      expand = c(0,0)) + 
  scale_y_continuous (breaks = c(64.25, 64.75, 65.25), 
                      labels = c ("64.25\u00B0N", "64.75\u00B0N", "65.25\u00B0N"), 
                      expand = c(0,0)) + 
  annotation_north_arrow(style = north_arrow_nautical()) + 
  coord_sf(crs = 4326) + 
  annotation_scale(style = "ticks", location = "br", height = unit(2, "mm"), 
                   tick_height = 1, text_cex = 1, line_width = 2) +
  theme(axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0), colour = "black"),
        axis.title.x = element_text(size = 14, margin = margin(t = 0, r = 0, b = 0, l = 0), colour = "black"),
        text = element_text(family = "Times New Roman")) +
  theme (panel.border = element_rect (fill = "transparent", color = "grey20", size = 2)) +
  theme (plot.margin = unit(c(0,0,.5,0), "cm")) 

sites.map


# Make the subset map
alaska.box2 <- c(left = -169, bottom = 56, right = -141, top = 71.5)
alaska.base2 <- ggmap::get_stamenmap (bbox = alaska.box2, zoom = 5, maptype = "terrain-background", 
                                     where = "cache")
alaska <- ggmap(alaska.base2) +
  geom_rect(xmin = -167, xmax = -163, ymin = 64, ymax = 65.5, fill = "transparent", 
            color = "grey20", size = 1) + theme_void() + 
  theme (panel.border = element_rect (fill = "transparent", color = "grey20", size = 1)) +
  theme (plot.margin = unit(c(0,0,0,0), "cm"))

alaska
  
# Combine figures
fig1 <- ggdraw() +
  draw_plot(sites.map) +
  draw_plot(alaska, x = 0.5, y = 0.45, width = 0.5, height = 0.5)

fig1

dev.new (width = 5.62, height = 3.53, units = "in", noRStudioGD = T); last_plot()
ggsave ("LHH_Figure1.tiff", width = dev.size()[1], height = dev.size()[2], dpi = 600); dev.off()
