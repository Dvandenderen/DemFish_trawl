
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

###########
### load survey data and spatial object of the area surveyed
###########
load("cleaned data/surveyed_grid.RData")
source("scripts - data processing/source_combine_all_surveys_after_cleaning.R")

Richness <- subset(trawl,trawl$type == "dem")

# now get overlap between survey and grid_master
Richness$uniq <- paste(Richness$lon,Richness$lat,sep="_")
coords_uni <- unique(Richness$uniq)
t <- strsplit(coords_uni, "_")
coords<- matrix(unlist(t), ncol=2, byrow=TRUE)
coords <- as.data.frame(coords)
coords[,1] <- as.numeric(as.character(coords[,1]))
coords[,2] <- as.numeric(as.character(coords[,2]))
coords[,3] <- coords_uni

coord <-data.frame(Longitude = coords[,1], Latitude = coords[,2])
coordinates(coord)<- ~ Longitude + Latitude  
crs(coord) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
tr <- over(coord,grid_master)
coords$one_degrees <- tr$uni

Richness <- cbind(Richness,coords[match(Richness$uniq,coords[,3]),c("one_degrees")])
colnames(Richness)[ncol(Richness)] <- "one_degrees"

Richness <- cbind(Richness,grid_master@data[match(Richness$one_degrees,grid_master@data$uni),c("subdivision")])
colnames(Richness)[ncol(Richness)] <- "subdivision"


agg <- aggregate(data=Richness, spp ~ subdivision + year, function(x) length(unique(x)))

agg <- aggregate(data=agg, spp ~ subdivision , function(x) mean(x))

