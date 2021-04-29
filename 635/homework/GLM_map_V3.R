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
coords <- c(-161.5287514135798,64.37835088320918,-161.5272618494996,64.37832789003939,-161.5261371665858,64.37864616614081,-161.5248218150066,64.3797894306184,-161.5231773125713,64.38046539295118,-161.5226828486227,64.38156899745968,-161.5214587415487,64.3822864992289,-161.5214803120477,64.38314658644371,-161.5210851834547,64.38414064380714,-161.5185780085142,64.38551786094061,-161.5176720984914,64.38628068266118,-161.5180335133874,64.38732156886275,-161.5165157920693,64.38883711719704,-161.5147960676525,64.38929584988072,-161.5133104677427,64.39029720031792,-161.5111191206144,64.39117342182206,-161.5106620960832,64.39195486503817,-161.5103735360071,64.39299233189028,-161.5118130907404,64.39367750272029,-161.510327098534,64.39465330215998,-161.5079642037982,64.39587127917906,-161.5086540682303,64.39713121208445,-161.5104404069499,64.39815098797405,-161.5081079465631,64.39935261327999,-161.5068746922458,64.39988579447841,-161.5049880453263,64.40112268744909,-161.5030951421961,64.40287800518043,-161.5010065564789,64.40414040328363,-161.4984915279489,64.405494340351,-161.4977031776341,64.40661008223067,-161.4959003878483,64.40697133855663,-161.4939066083333,64.4075934225329,-161.493109738685,64.40807123722443,-161.486622270085,64.40985523063488,-161.4856378078,64.40992805012817,-161.4830414733686,64.41168888502496,-161.4822877778488,64.41220759622301,-161.4816344480676,64.41368447294224,-161.4763793954136,64.41412173530593,-161.475319733821,64.4144195295187,-161.4738075906968,64.41479248520639,-161.4730708530292,64.41684040883388,-161.4720630432894,64.41820563325972,-161.4650363594596,64.41919431643359,-161.459723598473,64.41965781003532,-161.4532532234119,64.42023784375961,-161.4444397969773,64.420291192561,-161.4358353493787,64.42084239232898,-161.4281206341303,64.42116122830623,-161.4219042026614,64.42095622197462,-161.4144604974011,64.42123739459248,-161.4096696505112,64.42129356161804,-161.403997018244,64.4216381831433,-161.3993522624151,64.42272589955392,-161.3946349825618,64.42396298724805,-161.3922183216225,64.42107579018536,-161.3912419304123,64.41836926165132,-161.3912651033054,64.41719835663608,-161.3836019961352,64.41421621960538,-161.3755582809381,64.41201266640331,-161.3680170759483,64.41020343425663,-161.3663895708587,64.41029804988102,-161.3640962464258,64.40921045401885,-161.3575135576229,64.40779129336671,-161.35156870061,64.40652557724546,-161.3458184509419,64.4052815300673,-161.3414159841045,64.40426796378985,-161.3360086035975,64.40306697725455,-161.3307367017692,64.40203085716622,-161.3227469444216,64.40099848657391,-161.3129016588381,64.40003525532674,-161.3075845551875,64.39787097658891,-161.3030158518042,64.39677944285921,-161.2970396422253,64.39558318772991,-161.2921241609843,64.39469662296867,-161.2868581401219,64.39447544128387,-161.2826471030216,64.39499859728711,-161.2775674215106,64.39467050774334,-161.2749822202105,64.3928821639611,-161.2723690686246,64.39028600886749,-161.2673124110051,64.38845965189262,-161.2621811210406,64.38653299489263,-161.2576045938926,64.38480312424136,-161.2537787979994,64.3831664154611,-161.2464179999855,64.38170669692241,-161.2424489544452,64.38125506967965)
coords <- append(coords, c(-161.2390499118216,64.38061049724973,-161.2375911371597,64.37958050642334,-161.2376350332717,64.37696938737596,-161.2331995118422,64.37277281044818,-161.2285933250665,64.3700287226674,-161.2240006756351,64.36751454109928,-161.220608145123,64.36565359795917,-161.2162081389885,64.36344445093489,-161.2119579832319,64.36127195304186,-161.2070461329956,64.35898167400825,-161.1904259487058,64.35085388578976,-161.1822057072002,64.34644373310554,-161.1778943199586,64.34407954612998,-161.1734945364335,64.3415882793508,-161.1691085041344,64.33916278953515,-161.1654798938475,64.33724026428504,-161.1618514171435,64.33513912730884,-161.1582184932166,64.33300080801483,-161.1539072516179,64.33051795145758,-161.1503031684707,64.32837514296412,-161.1414706858101,64.32315336026289,-161.1335469361007,64.31861048111628,-161.1264191937318,64.31473750321521,-161.1184644451191,64.31029669031963,-161.1106214225244,64.30623163618502,-161.1028642823171,64.30217840511872,-161.0957598326851,64.29862851355291,-161.0892523070869,64.29536769581499,-161.0731718766267,64.28733855660968,-161.0632643773163,64.2825321948697,-161.0532681071648,64.27749583483585,-161.0431223706831,64.27230577385977,-161.0340145485281,64.26744209858825,-161.0220510220911,64.26105003718973,-161.0090309931391,64.25387995830451,-160.9999180783516,64.24880406023075,-160.9908483453287,64.24384248578819,-160.9839234017522,64.23930150082472,-160.9737982639049,64.23247253896062,-160.9686816899014,64.22802691220355,-160.9638461181771,64.22381584609856,-160.9592763147811,64.21900402756296,-160.9559237081083,64.21475999268124,-160.9528444944722,64.21107987795766,-160.9512197517678,64.2081332515602,-160.9499497975463,64.20490512774731,-160.9495683920227,64.20024215131164,-160.9507233249571,64.1998085761517,-160.9519262654913,64.19718452987328,-160.9527606226469,64.19448657345403,-160.954535483166,64.19199450228521,-160.954975262719,64.18959730180489,-160.9559656100055,64.18784501677291,-160.9557155846481,64.18449480161111,-160.9552743089798,64.18124308902598,-160.9545929939262,64.17910390205427,-160.9537666945073,64.17461585200462,-160.953580062382,64.1713864544912,-160.9540353202777,64.1693388264396,-160.9552407345972,64.1682072209458,-160.9546972901521,64.16670830603013,-160.9546908948532,64.16457331853566,-160.9549145219714,64.16330629404524,-160.9544974544718,64.16130498897172,-160.955436036912,64.16132591847015,-160.9549153395658,64.15964171427834,-160.9546100133367,64.15791175528197,-160.9539120904094,64.15591496512934,-160.9542213015893,64.15370408305874,-160.9559943221358,64.15235433719033,-160.956404311381,64.15105560202154,-160.9570721916948,64.14997305704989,-160.9556136943908,64.14756256266745,-160.9555710001884,64.14547785714592,-160.9547413507005,64.14352746741984,-160.9541545333754,64.14188514159619,-160.9549086175462,64.1409687297497,-160.9550007840916,64.1398023911601,-160.9548620800496,64.13829574469739,-160.9550525186099,64.13706798658224,-160.9549082669064,64.13575129946439,-160.9553344430567,64.13533410050108,-160.9538240368648,64.13425357163163,-160.9528645620438,64.13297070944114,-160.951925482194,64.13161971343806))

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
pred <- predict(bi_model, data.frame(Lat = lat, Long = long),
                type = "response")

pred.data <- data.frame(cbind(lat, long, pred))

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
  geom_point(data = pred.data, aes(x=long, 
                                   y=lat, fill = pred), color = "transparent", size = 5, shape = 21) +
  scale_fill_viridis(discrete = F, option = "C", name = "Likelihood", 
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
ggsave ("GLM_map_V2.tiff", width = dev.size()[1], height = dev.size()[2], dpi = 600); dev.off()

# Try one more time with predictive points right along the shore
pred.coords <- read.csv("635/data/pred_coords.csv")
