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
coords <- c(-161.5273018875508,64.37747333390276,-161.4699594151314,64.41695242913136,
            -161.4006001991064,64.41740707243177,-161.3205411884522,64.39860357934566,
            -161.2336829929258,64.37166138833511,-161.1766256570854,64.34163451352516,
            -161.0956679338751,64.29776771313855,-161.0164809519801,64.25666957567954,
            -160.9599921990417,64.2200052650285,-160.9510959985217,64.19937145390944,
            -160.9546669149806,64.16322695760471,-160.9572106312379,64.14900566586235,
            -160.9548838659418,64.13187753606377)

# Separate lat and long
lat <- coords[c(F, T)]
long <- coords[c(T, F)]

# Create sequence of lats along path
lat1  <- vector()
for(i in 1:11) {
  lat1 <- append(x = lat1, values = c(seq(lat[i], lat[i+1], length.out = 100)))
}

# Create sequence of longs along path
long1  <- vector()
for(i in 1:11) {
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
                                   y=lat1, color = pred), size = 1) +
  scale_color_viridis(discrete = F, option = "C", alpha = .5) +
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

