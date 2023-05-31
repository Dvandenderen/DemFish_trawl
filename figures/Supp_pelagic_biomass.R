
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

# ------------------------------------------------------------------------------
# load survey data and spatial object of the area surveyed
# ------------------------------------------------------------------------------

  load("processed data/surveyed_grid.RData")
  source("scripts - data processing/source_combine_all_surveys_after_cleaning.R")

# ------------------------------------------------------------------------------
# get all survey estimates per year on the grid 
# ------------------------------------------------------------------------------

  # get biomass weighted trophic level 
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

  # add the type, size, species and mean trophic level
  trawl_sep <- subset(trawl_sep,trawl_sep$type =="pel")
  colnames(trawl_sep)[which(colnames(trawl_sep) %in% c("wtcpue","wtcpue_q","tlweight"))] <- c("wtcpue_Pel","wtcpue_q_Pel","tlw_Pel")
  trawl <- cbind(trawl,trawl_sep[match(trawl$haulid,trawl_sep$haulid), c("wtcpue_Pel","wtcpue_q_Pel","tlw_Pel")])
  trawl <- trawl[,c('haulid','region','year','lon','lat','depth',"wtcpue_q_Pel","tlw_Pel")]
  colnames(trawl) <- c('haulid','region','year','lon','lat','depth',"biomass","tlw")
  trawl$tlw <- trawl$tlw/trawl$biomass 
  
  # remove NA's
  trawl <- subset(trawl,!(is.na(trawl$biomass)))
  
  # remove Aleutian island boundary
  trawl <- subset(trawl, trawl$lon > -180)
  
  # some data seem too high
  # e.g. plot(trawl$biomass[trawl$region == "NS-IBTS"])
  # decided to remove all outside 1.5 x the interquantile range (log10(biomass))
  # for each 'survey region' and year
  
  # data can be selected based on 1.5 x the interquantile range
  trawl$biomass <- log10(trawl$biomass)
  IQR           <- trawl %>% 
    group_by(region,year) %>%  
    summarise(quantile(biomass,0.25),quantile(biomass,0.75))
  IQR           <- as.data.frame(IQR)
  IQR$IQR       <- IQR[,4]-IQR[,3]   
  IQR$high      <- IQR[,4] + IQR$IQR*1.5
  IQR$low       <- IQR[,3] - IQR$IQR*1.5
  IQR$uni       <- paste(IQR$region,IQR$year)
  trawl$regy    <- paste(trawl$region,trawl$year)
  trawl         <- cbind(trawl, IQR[match(trawl$regy,IQR$uni), c("high","low")])
  trawl$high    <- trawl$high - trawl$biomass
  trawl$low     <- trawl$biomass - trawl$low
  trawl         <- subset(trawl,trawl$high >= 0 & trawl$low >= 0)
  trawl$biomass <- 10^trawl$biomass
  trawl         <- trawl[,-c(9:11)]

  # now get overlap between survey and grid_master
  trawl$uniq <- paste(trawl$lon,trawl$lat,sep="_")
  coords_uni <- unique(trawl$uniq)
  t <- strsplit(coords_uni, "_")
  coords<- matrix(unlist(t), ncol=2, byrow=TRUE)
  coords <- as.data.frame(coords)
  coords[,1] <- as.numeric(as.character(coords[,1]))
  coords[,2] <- as.numeric(as.character(coords[,2]))
  coords[,3] <- coords_uni
  coordinates(coords)<- ~ V1 + V2  
  crs(coords) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  coords$uni_cell <- over(coords,grid_master)[,1]
  trawl <- cbind(trawl,coords@data[match(trawl$uniq,coords@data[,1]),c("uni_cell")])
  colnames(trawl)[ncol(trawl)] <- "uni_cell"
  
  # remove all single grid observations across all years
  tt <- as.data.frame(table(trawl$uni_cell))
  tt <- subset(tt,tt$Freq >1)
  trawl <- subset(trawl,trawl$uni_cell %in% tt$Var1)
  
  # get mean biomass and mean trophic level per grid cell and year
  cpue      <- trawl %>%
    group_by(uni_cell,year) %>%
    summarise_at (c("biomass","tlw"),mean, na.rm=T) %>%
    as.data.frame ()
  cpue$uni  <- paste(cpue$uni_cell,cpue$year)
  cpue$year <- as.numeric(cpue$year)

  depth_grid <- trawl %>%
    group_by(uni_cell,year) %>%
    summarise_at (c("depth"),mean, na.rm=T) %>%
    as.data.frame ()

  # get depth prediction
  depth_grid <- subset(depth_grid,depth_grid$year %in% c(1990:2015))
  depth_grid <- depth_grid %>%
    group_by(uni_cell) %>%
    summarise_at (c("depth"),mean, na.rm=T) %>%
    as.data.frame ()

  # get pelagic data, link to grid and create plot
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

  pdf("figures/Supp_pelagic_biomass_with_depth.pdf",width=5.5,height=5.5)
  par(mfrow=c(2,2), mar=c(6, 4, 2, 1))
  
  t1 <- subset(grid_master@data,grid_master@data$ECO_REG =="Northern Gulf of Mexico")
  plot(t1$biomass~t1$depth,xlab="Depth (m)",ylab="Biomass (MT per km2)",xlim=c(0,max(t1$depth)),las=1,main="Northern Gulf of Mexico")
  
  t1 <- subset(grid_master@data,grid_master@data$ECO_REG =="North Sea")
  plot(t1$biomass~t1$depth,xlab="Depth (m)",ylab="",xlim=c(0,max(t1$depth)),las=1,main="North Sea")
  
  t1 <- subset(grid_master@data,grid_master@data$ECO_REG =="Northern California")
  plot(t1$biomass~t1$depth,xlab="Depth (m)",ylab="Biomass (MT per km2)",xlim=c(0,max(t1$depth)),las=1,main="Northern California")
  
  t1 <- subset(grid_master@data,grid_master@data$ECO_REG =="North American Pacific Fijordland")
  plot(t1$biomass~t1$depth,xlab="Depth (m)",ylab="",xlim=c(0,max(t1$depth)),las=1,main="Pacific Fijordland")
  
  dev.off()

# now make map with pelagic biomass
  ec <- unique(grid_master@data$ECO_REG)
  for (j in 1:21){
  t1 <- subset(grid_master@data,grid_master@data$ECO_REG ==ec[j])
  plot(t1$biomass~t1$depth,xlab="Depth (m)",ylab="",xlim=c(0,max(t1$depth)),las=1,main=ec[j])
  }

  roundUp <- function(x) 10^ceiling(log10(x)) # round to nearest 10
  grid_master$biomass[grid_master$biomass > pretty(quantile(grid_master$biomass, 0.95))[1]] <- pretty(quantile(grid_master$biomass, 0.95))[1]
  
  nco <- sf::st_as_sf(grid_master)


# prepare plot
  ctrys <- rnaturalearth::ne_countries(scale = 50, returnclass = "sf")
  minlong <- -180 #round(min(filedata$long)-1)
  maxlong <- 51
  minlat  <- 23
  maxlat  <- 83
  coordslim <- c(minlong,maxlong,minlat,maxlat)
  coordxmap <- round(seq(minlong,maxlong,length.out = 4))
  coordymap <- round(seq(minlat,maxlat,length.out = 4))
  
  # plot map with pelagic mean biomass
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

pdf("figures/Supp_pelagic_biomass_map.pdf",width=10,height=4) 
grid.arrange(figmap, nrow = 1,
             left = "Latitude",bottom = "Longitude")
dev.off()


