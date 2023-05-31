
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
load("processed data/surveyed_grid.RData") # get grid information
load("processed data/Biomass_grid.RData") # get biomass per grid cell and year

grid_master <- subset(grid_master,grid_master@data$uni_cell %in% cpue_good$uni_cell)

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

  figmap1 <- ggplot() + 
    geom_sf( data= nco) + 
    geom_sf(data = ctrys, fill="grey",colour=NA) +
    coord_sf(crs = 5070,xlim = c(-4927853,3633046),ylim=c(307076,6029855))
  
  figmap1 <- figmap1 +  theme(plot.background=element_blank(),
                              panel.grid.major = element_line(colour = "grey"),
                              panel.background=element_blank(),
                              axis.text.y   = element_text(size=10),
                              axis.text.x   = element_text(size=10),
                              axis.title.y  = element_blank(),
                              plot.title = element_text(size = 11),
                              axis.title.x  = element_blank(),
                              panel.border  = element_rect(colour = "grey", size=.5,fill=NA),
                              legend.text   = element_text(size=10),
                              legend.title  = element_text(size=10),
                              legend.key.size = unit(.4, 'cm'),
                              legend.position = "bottom")
  
  figmap2 <- ggplot() + 
    geom_sf(data= nco ) + 
    geom_sf(data = ctrys, fill="grey",colour=NA) +
    coord_sf(crs =  "+proj=aea +lat_0=23 +lon_0=10 +lat_1=29.5 +lat_2=45.5 +x_0=0 
           +y_0=0 +datum=NAD83 +units=m +no_defs", 
             xlim = c(-2427853,2333046),ylim=c(807076,6029855))
  
  figmap2 <-  figmap2 +  theme(plot.background=element_blank(),
                               panel.grid.major = element_line(colour = "grey"),
                               panel.background=element_blank(),
                               axis.text.y   = element_text(size=10),
                               axis.text.x   = element_text(size=10),
                               axis.title.y  = element_blank(),
                               plot.title = element_text(size = 11),
                               axis.title.x  = element_blank(),
                               panel.border  = element_rect(colour = "grey", size=.5,fill=NA),
                               legend.text   = element_text(size=10),
                               legend.title  = element_text(size=9),
                               legend.key.size = unit(.4, 'cm'),
                               legend.position = "bottom")
  
  upper  <- cowplot::plot_grid(figmap1,figmap2,nrow = 1, rel_widths=c(0.60,0.40),labels = c('a)',''), label_size = 10,label_fontface="plain")
  
  nco <- unionSpatialPolygons(grid_master,grid_master$subdivision)
  nco <- sf::st_as_sf(nco)
  xy = st_coordinates(st_centroid(nco))
  nco = nco[order(xy[,"X"], xy[,"Y"]),]

  col <-rep(c(viridis::viridis(6),viridis::plasma(6)),4)[1:nrow(nco)]

figmap1 <- ggplot() + 
  geom_sf( data= nco,fill=col) + 
  geom_sf(data = ctrys, fill="grey",colour=NA) +
  coord_sf(crs = 5070,xlim = c(-4927853,3633046),ylim=c(307076,6029855))

figmap1 <- figmap1 +  theme(plot.background=element_blank(),
                            panel.grid.major = element_line(colour = "grey"),
                            panel.background=element_blank(),
                            axis.text.y   = element_text(size=10),
                            axis.text.x   = element_text(size=10),
                            axis.title.y  = element_blank(),
                            plot.title = element_text(size = 11),
                            axis.title.x  = element_blank(),
                            panel.border  = element_rect(colour = "grey", size=.5,fill=NA),
                            legend.text   = element_text(size=10),
                            legend.title  = element_text(size=10),
                            legend.key.size = unit(.4, 'cm'),
                            legend.position = "bottom")

figmap2 <- ggplot() + 
  geom_sf(data= nco,fill=col ) + 
  geom_sf(data = ctrys, fill="grey",colour=NA) +
  coord_sf(crs =  "+proj=aea +lat_0=23 +lon_0=10 +lat_1=29.5 +lat_2=45.5 +x_0=0 
           +y_0=0 +datum=NAD83 +units=m +no_defs", 
           xlim = c(-2427853,2333046),ylim=c(807076,6029855))

figmap2 <-  figmap2 +  theme(plot.background=element_blank(),
                             panel.grid.major = element_line(colour = "grey"),
                             panel.background=element_blank(),
                             axis.text.y   = element_text(size=10),
                             axis.text.x   = element_text(size=10),
                             axis.title.y  = element_blank(),
                             plot.title = element_text(size = 11),
                             axis.title.x  = element_blank(),
                             panel.border  = element_rect(colour = "grey", size=.5,fill=NA),
                             legend.text   = element_text(size=10),
                             legend.title  = element_text(size=9),
                             legend.key.size = unit(.4, 'cm'),
                             legend.position = "bottom")

middle  <- cowplot::plot_grid(figmap1,figmap2,nrow = 1, rel_widths=c(0.60,0.40),labels = c('b)',''), label_size = 10,label_fontface="plain")

nco <- unionSpatialPolygons(grid_master,grid_master$ECO_REG)
nco <- sf::st_as_sf(nco)
xy = st_coordinates(st_centroid(nco))
nco = nco[order(xy[,"X"], xy[,"Y"]),]

col <-rep(c(viridis::viridis(6),viridis::plasma(6)),4)[1:nrow(nco)]

figmap1 <- ggplot() + 
  geom_sf( data= nco,fill=col) + 
  geom_sf(data = ctrys, fill="grey",colour=NA) +
  coord_sf(crs = 5070,xlim = c(-4927853,3633046),ylim=c(307076,6029855))

figmap1 <- figmap1 +  theme(plot.background=element_blank(),
                            panel.grid.major = element_line(colour = "grey"),
                            panel.background=element_blank(),
                            axis.text.y   = element_text(size=10),
                            axis.text.x   = element_text(size=10),
                            axis.title.y  = element_blank(),
                            plot.title = element_text(size = 11),
                            axis.title.x  = element_blank(),
                            panel.border  = element_rect(colour = "grey", size=.5,fill=NA),
                            legend.text   = element_text(size=10),
                            legend.title  = element_text(size=10),
                            legend.key.size = unit(.4, 'cm'),
                            legend.position = "bottom")

figmap2 <- ggplot() + 
  geom_sf(data= nco,fill=col ) + 
  geom_sf(data = ctrys, fill="grey",colour=NA) +
  coord_sf(crs =  "+proj=aea +lat_0=23 +lon_0=10 +lat_1=29.5 +lat_2=45.5 +x_0=0 
           +y_0=0 +datum=NAD83 +units=m +no_defs", 
           xlim = c(-2427853,2333046),ylim=c(807076,6029855))

figmap2 <-  figmap2 +  theme(plot.background=element_blank(),
                             panel.grid.major = element_line(colour = "grey"),
                             panel.background=element_blank(),
                             axis.text.y   = element_text(size=10),
                             axis.text.x   = element_text(size=10),
                             axis.title.y  = element_blank(),
                             plot.title = element_text(size = 11),
                             axis.title.x  = element_blank(),
                             panel.border  = element_rect(colour = "grey", size=.5,fill=NA),
                             legend.text   = element_text(size=10),
                             legend.title  = element_text(size=9),
                             legend.key.size = unit(.4, 'cm'),
                             legend.position = "bottom")

lower  <- cowplot::plot_grid(figmap1,figmap2,nrow = 1, rel_widths=c(0.60,0.40),labels = c('c)',''), label_size = 10,label_fontface="plain")


png("figures/Spatial scales.png",width=8,height=10) 
grid.arrange(upper, middle, lower, nrow = 3,
             left = "Latitude",bottom = "Longitude")
dev.off()
