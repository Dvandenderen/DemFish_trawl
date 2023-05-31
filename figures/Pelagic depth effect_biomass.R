
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
load("processed data/surveyed_grid.RData")
source("scripts - data processing/source_combine_all_surveys_after_cleaning.R")

##############################################
# get all survey estimates per year on the grid 

trawl$tlweight <- trawl$tl*trawl$wtcpue_q

# get 95 percentile of dem trophic level
trawldem <- subset(trawl,trawl$type =="dem")
trawldem <- trawldem %>% group_by(haulid) %>% arrange(tl) %>% mutate(cs = cumsum(wtcpue_q)/sum(wtcpue_q))
hauls <- trawldem %>% group_by(haulid) %>% 
  filter(cs > 0.95) %>%
  slice(1) %>%
  select(haulid,tl) %>%
  as.data.frame()

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

# add the type, size, species, mean trophic level and 95th percentile of trophic level
trawl_sep <- subset(trawl_sep,trawl_sep$type =="pel")
colnames(trawl_sep)[which(colnames(trawl_sep) %in% c("wtcpue","wtcpue_q","tlweight"))] <- c("wtcpue_Dem","wtcpue_q_Dem","tlw_Dem")
trawl <- cbind(trawl,trawl_sep[match(trawl$haulid,trawl_sep$haulid), c("wtcpue_Dem","wtcpue_q_Dem","tlw_Dem")])
trawl <- trawl[,c('haulid','region','year','lon','lat','depth',"wtcpue_q_Dem","tlw_Dem")]
trawl <- cbind(trawl,hauls[match(trawl$haulid,hauls$haulid), c(2)])
colnames(trawl) <- c('haulid','region','year','lon','lat','depth',"biomass","tlw","tl_95")
trawl <- subset(trawl,trawl$biomass < 10^100 | !(is.na(trawl$biomass)))
trawl <- subset(trawl,!(trawl$biomass ==0))
trawl$tlw <- trawl$tlw/trawl$biomass

# remove highest 2% of biomass per year and survey
high <- trawl %>%
  group_by(region,year)  %>%
  slice_max(biomass, prop = 0.02) 

#low <- trawl %>%
#  group_by(region,year)  %>%
#  slice_min(biomass, prop = 0.02) 

LH_ends <- c(unique(high$haulid)) #c(unique(high$haulid),unique(low$haulid))

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
coords$uni_cell <- tr$uni_cell

trawl <- cbind(trawl,coords[match(trawl$uniq,coords[,3]),c("uni_cell")])
colnames(trawl)[ncol(trawl)] <- "uni_cell"

cpue <- trawl %>%
  group_by(uni_cell,year) %>%
  summarise_at (c("biomass","tlw","tl_95"),mean, na.rm=T) %>%
  as.data.frame ()
cpue$uni <- paste(cpue$uni_cell,cpue$year)
cpue$year <- as.numeric(cpue$year)

depth_grid <- trawl %>%
  group_by(uni_cell,year) %>%
  summarise_at (c("depth"),mean, na.rm=T) %>%
  as.data.frame ()

#### get depth prediction
depth_grid <- subset(depth_grid,depth_grid$year %in% c(1990:2015))
depth_grid <- depth_grid %>%
  group_by(uni_cell) %>%
  summarise_at (c("depth"),mean, na.rm=T) %>%
  as.data.frame ()

cpuepel <- subset(cpue,cpue$year %in% c(1990:2015))
cpuepel <- cpuepel %>%
  group_by(uni_cell) %>%
  summarise_at (c("biomass"),mean, na.rm=T) %>%
  as.data.frame ()

grid_master <- cbind(grid_master,cpuepel[match(grid_master@data$uni_cell,cpuepel$uni_cell),c("biomass")])
colnames(grid_master@data)[ncol(grid_master@data)]<- "biomass"
grid_master <- cbind(grid_master,depth_grid[match(grid_master@data$uni_cell,depth_grid$uni_cell),c("depth")])
colnames(grid_master@data)[ncol(grid_master@data)]<- "depth"
grid_master <- subset(grid_master,!(is.na(grid_master@data$biomass)))
grid_master@data$biomass <- grid_master@data$biomass/1000 # kg per km2 to tonnes per km2

pdf("figures/Pelagic depth effect.pdf",width=5.5,height=5.5)
par(mfrow=c(2,2), mar=c(6, 4, 2, 1))

t1 <- subset(grid_master@data,grid_master@data$ECO_REG =="Northern Gulf of Mexico")
plot(t1$biomass~t1$depth,xlab="Depth (m)",ylab="Biomass (MT per km2)",xlim=c(0,max(t1$depth)),las=1,main="Northern Gulf of Mexico")

t1 <- subset(grid_master@data,grid_master@data$ECO_REG =="North Sea")
plot(t1$biomass~t1$depth,xlab="Depth (m)",ylab="",xlim=c(0,max(t1$depth)),las=1,main="North Sea")

t1 <- subset(grid_master@data,grid_master@data$ECO_REG =="Northern California")
plot(t1$biomass~t1$depth,xlab="Depth (m)",ylab="Biomass (MT per km2)",xlim=c(0,max(t1$depth)),las=1,main="Northern California")

t1 <- subset(grid_master@data,grid_master@data$ECO_REG =="North American Pacific Fijordland")
plot(t1$biomass~t1$depth,xlab="Depth (m)",ylab="",xlim=c(0,max(t1$depth)),las=1,main="North American Pacific Fijordland")

dev.off()


ec <- unique(grid_master@data$ECO_REG)
for (j in 1:21){
t1 <- subset(grid_master@data,grid_master@data$ECO_REG ==ec[j])
plot(t1$biomass~t1$depth,xlab="Depth (m)",ylab="",xlim=c(0,max(t1$depth)),las=1,main=ec[j])
}

roundUp <- function(x) 10^ceiling(log10(x)) # round to nearest 10
grid_master$biomass[grid_master$biomass > pretty(quantile(grid_master$biomass, 0.95))[1]] <- pretty(quantile(grid_master$biomass, 0.95))[1]

nco <- sf::st_as_sf(grid_master)

library(ggplot2)
library(viridis)
library(gridExtra)

# prepare plot
ctrys <- rnaturalearth::ne_countries(scale = 50, returnclass = "sf")
minlong <- -180 #round(min(filedata$long)-1)
maxlong <- 51
minlat  <- 23
maxlat  <- 83
coordslim <- c(minlong,maxlong,minlat,maxlat)
coordxmap <- round(seq(minlong,maxlong,length.out = 4))
coordymap <- round(seq(minlat,maxlat,length.out = 4))

# plot map
figmap <- ggplot(nco) + 
  geom_sf( aes(fill=biomass), colour = NA ) +
  scale_fill_viridis(name="Pelagic biomass \n (MT/km2)") +
  geom_sf(data = ctrys, fill="grey",colour=NA) 

figmap <-  figmap +  theme(plot.background=element_blank(),
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

pdf("figures/Pelagic biomass.pdf",width=10,height=4) 
grid.arrange(figmap, nrow = 1,
             left = "Latitude",bottom = "Longitude")
dev.off()


