
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

##############################################
# get all survey estimates per year on the grid - 

trawl$tlweight <- trawl$tl*trawl$wtcpue_q

# get types,sizes separate
trawl_sep <- trawl %>% 
  group_by(haulid,region,gear,year,month,lon,lat,depth,type) %>%
  summarize_at(.vars=c('wtcpue', 'wtcpue_q','tlweight'), .funs = function(x) sum(x,na.rm=T)) %>% 
  dplyr::select(haulid,region,gear,year,month,lon,lat,depth,type,wtcpue,wtcpue_q,tlweight) %>%
  as.data.frame()

# now get all stations and years with data
trawl <- trawl %>% 
  group_by(haulid,region,gear,year,month,lon,lat,depth) %>%
  summarize_at(.vars=c('wtcpue', 'wtcpue_q'), .funs = function(x) sum(x)) %>% 
  dplyr::select(haulid,region,gear,year,month,lon,lat,depth,wtcpue,wtcpue_q) %>%
  as.data.frame()

# add the type, size, species to estimate surplus production 
trawl_sep <- subset(trawl_sep,trawl_sep$type =="dem")
colnames(trawl_sep)[which(colnames(trawl_sep) %in% c("wtcpue","wtcpue_q","tlweight"))] <- c("wtcpue_Dem","wtcpue_q_Dem","tlw_Dem")
trawl <- cbind(trawl,trawl_sep[match(trawl$haulid,trawl_sep$haulid), c("wtcpue_Dem","wtcpue_q_Dem","tlw_Dem")])
trawl <- trawl[,c('haulid','region','year','lon','lat','depth',"wtcpue_q_Dem","tlw_Dem")]
colnames(trawl) <- c('haulid','region','year','lon','lat','depth',"biomass","tlw")
trawl$biomass <- ifelse(is.na(trawl$biomass),0,trawl$biomass)
trawl <- subset(trawl,trawl$biomass < 10^100)
trawl$tlw <- trawl$tlw/trawl$biomass

# remove lowest and highest 2% of biomass per year and survey
high <- trawl %>%
  group_by(region,year)  %>%
  slice_max(biomass, prop = 0.02) 

low <- trawl %>%
  group_by(region,year)  %>%
  slice_min(biomass, prop = 0.02) 

LH_ends <- c(unique(high$haulid),unique(low$haulid))

trawl <- trawl %>% 
  filter(!(haulid %in% LH_ends)) %>%
  as.data.frame()

# now get overlap between survey and grid_master
trawl$uniq <- paste(trawl$lon,trawl$lat,sep="_")
coords_uni <- unique(trawl$uniq)
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

trawl <- cbind(trawl,coords[match(trawl$uniq,coords[,3]),c("one_degrees")])
colnames(trawl)[ncol(trawl)] <- "one_degrees"

cpue <- trawl %>%
  group_by(one_degrees,year) %>%
  summarise_at (c("biomass","tlw"),mean, na.rm=T) %>%
  as.data.frame ()
cpue$uni <- paste(cpue$one_degrees,cpue$year)
cpue$year <- as.numeric(cpue$year)

cpue <- cbind(cpue,grid_master@data[match(cpue$one_degrees,grid_master@data$uni),c("ECO_REG")])
colnames(cpue)[ncol(cpue)] <- "ECO_REG" 
cpue <- subset(cpue,!(is.na(cpue$one_degrees)))

depth_grid <- trawl %>%
  group_by(one_degrees) %>%
  summarise_at (c("depth"),mean, na.rm=T) %>%
  as.data.frame ()

rm(list=setdiff(ls(), c("cpue","grid_master","depth_grid")))

##### now get the mean for all grids for 2000 -2010 (ignoring any variation in sampling)

# remove all NAs
cpue <- subset(cpue,!(is.na(cpue$biomass)))

cpue <- subset(cpue,cpue$year %in% c(2000:2010))

tt <-  aggregate(list(cpue$biomass,cpue$tlw),
                 by=list(cpue$one_degrees),FUN=mean,na.rm=T)
colnames(tt) <- c("one_degrees","biomass","tl")

####### get the mean catch of alll grids for 2000-2010

# load Regs fisheries database (v4.0)
C0004 <- readRDS("C:/Users/danie/Dropbox/Werk/Demersal fish and fisheries/Data analysis/Catch_all_2000_2004.rds")
C0509 <- readRDS("C:/Users/danie/Dropbox/Werk/Demersal fish and fisheries/Data analysis/Catch_all_2005_2009.rds")
C1014 <- readRDS("C:/Users/danie/Dropbox/Werk/Demersal fish and fisheries/Data analysis/Catch_all_2010_2014.rds")

### total catch for all demersal and pelagic groups
Catch <- rbind(C0004,C0509,C1014)
Catch <- subset(Catch,Catch$IYear %in% c(2000:2010))
Catchdem <- subset(Catch, Catch$Funcgroup %in% c(4,5,6,10:24))
Catchpel <- subset(Catch, Catch$Funcgroup %in% c(1:3))

Catchdem$Tot <- Catchdem$Reported + Catchdem$IUU + Catchdem$Discards
Catchpel$Tot <- Catchpel$Reported + Catchpel$IUU + Catchpel$Discards
Catch <- aggregate(Catchdem$Tot, by= list(Catchdem$Cell,Catchdem$IYear),FUN= sum,na.rm=T)
colnames(Catch) <- c("Cell","Year","catch")
Catch$uni <- paste(Catch$Cell,Catch$Year)
Catchpel <- aggregate(Catchpel$Tot, by= list(Catchpel$Cell,Catchpel$IYear),FUN= sum,na.rm=T)
colnames(Catchpel) <- c("Cell","Year","catch")
Catchpel$uni <- paste(Catchpel$Cell,Catchpel$Year)
Catch <- cbind(Catch,Catchpel[match(Catch$uni,Catchpel$uni),c("catch")])
colnames(Catch)[ncol(Catch)] <- "catch_pel"

### load Watson cells
cells <- read.csv(file="C:/Users/danie/Dropbox/Werk/Demersal fish and fisheries/Data analysis/World_watson.csv",sep=";",header=T)

## link to one degrees grid
catchcoord <-data.frame(Longitude = cells$LonCentre , Latitude = cells$LatCentre)
coordinates(catchcoord)<- ~ Longitude + Latitude  
crs(catchcoord) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
tr <- over(catchcoord,grid_master)
cells$one_degrees <- tr$uni
cells <- subset(cells,!(is.na(cells$one_degrees)))

# get all cells for all years
cells2 <- as.data.frame(lapply(cells, rep, length(min(Catch$Year):max(Catch$Year))))
cells <- data.frame(cells2,Year = rep(min(Catch$Year):max(Catch$Year),each=nrow(cells)))
cells$uni <- paste(cells$Cell,cells$Year)
cells <- cbind(cells, Catch[match(cells$uni,Catch$uni), c("catch","catch_pel")])                    
#colnames(cells)[ncol(cells)] <- "catch"
cells$catch[is.na(cells$catch)] <- 0
cells$catch_pel[is.na(cells$catch_pel)] <- 0

# now get the sum in each grid_master cell
Fisheries <- aggregate(list(cells$catch,cells$catch_pel,cells$OceanAreasqkm), by= list(cells$one_degrees,cells$Year),FUN= sum)
colnames(Fisheries) <-  c("one_degrees","Year","Catch","Catch_pel","Area_catch")
Fisheries$Catch <- Fisheries$Catch *1000  # tonnes per cell per year ---> kg per cell per year
Fisheries$Catch_sqkm <- Fisheries$Catch/Fisheries$Area_catch # kg per cell per year ---> kg per km2 per year
Fisheries$Catch_pel <- Fisheries$Catch_pel *1000  # tonnes per cell per year ---> kg per cell per year
Fisheries$Catch_pel_sqkm <- Fisheries$Catch_pel/Fisheries$Area_catch # kg per cell per year ---> kg per km2 per year
Fisheries$uni <- paste(Fisheries$one_degrees,Fisheries$Year)

Fish <- aggregate(list(Fisheries$Catch_sqkm,Fisheries$Catch_pel_sqkm),
                 by=list(Fisheries$one_degrees,Fisheries$Year),FUN=mean,na.rm=T)
colnames(Fish) <- c("one_degrees","Year","Catch","Catch_pel")

Fish <- aggregate(list(Fish$Catch,Fish$Catch_pel),
                  by=list(Fish$one_degrees),FUN=mean,na.rm=T)
colnames(Fish) <- c("one_degrees","Catch","Catch_pel")

tt <- cbind(tt,Fish[match(tt$one_degrees,Fish$one_degrees),c(2:3)])
tt <- cbind(tt,depth_grid[match(tt$one_degrees,depth_grid$one_degrees),c("depth")])
colnames(tt)[ncol(tt)] <- "depth"

ttnew  <-  cbind(tt,newsub[match(tt$one_degrees,newsub$uni),c(4,6,8:15,18)])
 
ttnew[,c(2:6,9:16)] <- ttnew[,c(2:6,9:16)] * ttnew$ocean_sqkm 

tt <-  aggregate(list(ttnew[,c(2:6,7,9:16)]),
                 by=list(ttnew$subdivision),FUN=sum,na.rm=T)

tt[,c(2:6,8:15)] <- tt[,c(2:6,8:15)] / tt$ocean_sqkm 
colnames(tt)[1] <- "Subdiv"
  
save(tt,file = "C:/Users/danie/Desktop/data_subdivision.Rdata")
