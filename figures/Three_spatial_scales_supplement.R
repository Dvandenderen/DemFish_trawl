
rm(list=ls())

### load libraries
library(rgdal)
library(raster)
library(sp)
library(rgeos)
library(maptools)
library(dplyr)
library(sf)
library(tidyverse)
library(ggplot2)
library(viridis)
library(gridExtra)  

##################
load("cleaned data/surveyed_grid.RData") # get grid information
load("cleaned data/211216_biomass_grid.RData") # get biomass per grid cell and year

nco <- sf::st_as_sf(grid_master)

# prepare map
ctrys <- rnaturalearth::ne_countries(scale = 50, returnclass = "sf")
minlong <- -180 #round(min(filedata$long)-1)
maxlong <- 51
minlat  <- 23
maxlat  <- 83
coordslim <- c(minlong,maxlong,minlat,maxlat)
coordxmap <- round(seq(minlong,maxlong,length.out = 4))
coordymap <- round(seq(minlat,maxlat,length.out = 4))

# plot map
figmap1 <- ggplot(nco) + 
  geom_sf( aes(fill=SST), fill=NA, colour="black") + 
    geom_sf(data = ctrys, fill="grey",colour=NA) 

figmap1 <-  figmap1 +  theme(plot.background=element_blank(),
                           panel.background=element_blank(),
                           axis.text.y   = element_text(size=9),
                           axis.text.x   = element_text(size=9),
                           axis.title.y  = element_blank(),
                           plot.title = element_text(size = 11),
                           axis.title.x  = element_blank(),
                           panel.border  = element_rect(colour = "grey", size=.5,fill=NA),
                           legend.text   = element_text(size=9),
                           legend.title  = element_text(size=9),
                           legend.key.size = unit(.4, 'cm'))+
  coord_sf(xlim = c(-180, 51), ylim = c(20, 84), expand = FALSE) +
  scale_x_continuous(breaks=coordxmap)  +
  scale_y_continuous(breaks=coordymap)

nco <- unionSpatialPolygons(grid_master,grid_master$subdivision)
nco <- sf::st_as_sf(nco)

col <-rep(c(viridis::viridis(5),rev(viridis::viridis(4)),viridis::plasma(6)),10)[1:nrow(nco)]

figmap2 <- ggplot(nco) + 
  geom_sf( colour="black",fill=col) + 
  geom_sf(data = ctrys, fill="grey",colour=NA)

figmap2 <-  figmap2 +  theme(plot.background=element_blank(),
                             panel.background=element_blank(),
                             axis.text.y   = element_text(size=9),
                             axis.text.x   = element_text(size=9),
                             axis.title.y  = element_blank(),
                             plot.title = element_text(size = 11),
                             axis.title.x  = element_blank(),
                             panel.border  = element_rect(colour = "grey", size=.5,fill=NA),
                             legend.text   = element_text(size=9),
                             legend.title  = element_text(size=9),
                             legend.key.size = unit(.4, 'cm'))+
  coord_sf(xlim = c(-180, 51), ylim = c(20, 84), expand = FALSE) +
  scale_x_continuous(breaks=coordxmap)  +
  scale_y_continuous(breaks=coordymap)

nco <- unionSpatialPolygons(grid_master,grid_master$ECO_REG)
nco <- sf::st_as_sf(nco)

col <-rep(c(viridis::viridis(5),viridis::plasma(6)),10)[1:nrow(nco)]

figmap3 <- ggplot(nco) + 
  geom_sf( colour="black",fill=col) + 
  geom_sf(data = ctrys, fill="grey",colour=NA)

figmap3 <-  figmap3 +  theme(plot.background=element_blank(),
                             panel.background=element_blank(),
                             axis.text.y   = element_text(size=9),
                             axis.text.x   = element_text(size=9),
                             axis.title.y  = element_blank(),
                             plot.title = element_text(size = 11),
                             axis.title.x  = element_blank(),
                             panel.border  = element_rect(colour = "grey", size=.5,fill=NA),
                             legend.text   = element_text(size=9),
                             legend.title  = element_text(size=9),
                             legend.key.size = unit(.4, 'cm'))+
  coord_sf(xlim = c(-180, 51), ylim = c(20, 84), expand = FALSE) +
  scale_x_continuous(breaks=coordxmap)  +
  scale_y_continuous(breaks=coordymap)

pdf("figures/Spatial scales.pdf",width=8,height=10) 
grid.arrange(figmap1,figmap2,figmap3, nrow = 3,
             left = "Latitude",bottom = "Longitude")
dev.off()
