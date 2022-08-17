
rm(list=ls())

# load libraries
  library(rgdal)
  library(raster)
  library(sp)
  library(rgeos)
  library(maptools)
  library(dplyr)
  library(sf)
  library(tidyverse)

# --------------------------------------------------------------------------------
## load survey data and spatial object of the area surveyed
# --------------------------------------------------------------------------------

  load("cleaned data/surveyed_grid.RData")
  source("scripts - data processing/source_combine_all_surveys_after_cleaning.R")

# --------------------------------------------------------------------------------
# get all survey estimates per year on the grid 
# --------------------------------------------------------------------------------

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
  trawl_sep <- subset(trawl_sep,trawl_sep$type =="dem")
  colnames(trawl_sep)[which(colnames(trawl_sep) %in% c("wtcpue","wtcpue_q","tlweight"))] <- c("wtcpue_Dem","wtcpue_q_Dem","tlw_Dem")
  trawl <- cbind(trawl,trawl_sep[match(trawl$haulid,trawl_sep$haulid), c("wtcpue_Dem","wtcpue_q_Dem","tlw_Dem")])
  trawl <- trawl[,c('haulid','region','year','lon','lat','depth',"wtcpue_q_Dem","tlw_Dem")]
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

  # and save depth per grid cell and year
  depth_grid <- trawl %>%
    group_by(uni_cell,year) %>%
    summarise_at (c("depth"),mean, na.rm=T) %>%
    as.data.frame ()
  save(depth_grid,file="cleaned data/Depth_grid.RData")

  # clean and continue
  rm(list=setdiff(ls(), c("cpue","grid_master")))

  # get ocean area per cell 
  cpue_good <- cbind(cpue,grid_master@data[match(cpue$uni_cell,grid_master@data$uni_cell),c("ocean_sqkm")])
  colnames(cpue_good)[ncol(cpue_good)] <- "ocean_sqkm" 

# --------------------------------------------------------------------------------
# add fisheries landings information
# --------------------------------------------------------------------------------

  # load Regs fisheries database (v4.0)
  Fisheriesfolder <- "C:/Users/danie/Dropbox/Werk/Demersal fish and fisheries/Data analysis"
  C8084 <- readRDS(paste(Fisheriesfolder,"Catch_all_1980_1984.rds",sep="/"))
  C8589 <- readRDS(paste(Fisheriesfolder,"Catch_all_1985_1989.rds",sep="/"))
  C9094 <- readRDS(paste(Fisheriesfolder,"Catch_all_1990_1994.rds",sep="/"))
  C9599 <- readRDS(paste(Fisheriesfolder,"Catch_all_1995_1999.rds",sep="/"))
  C0004 <- readRDS(paste(Fisheriesfolder,"Catch_all_2000_2004.rds",sep="/"))
  C0509 <- readRDS(paste(Fisheriesfolder,"Catch_all_2005_2009.rds",sep="/"))
  C1014 <- readRDS(paste(Fisheriesfolder,"Catch_all_2010_2014.rds",sep="/"))
  C1515 <- readRDS(paste(Fisheriesfolder,"Catch_all_2015_2015.rds",sep="/"))
  
  # total catch for demersal and pelagic groups
  Catch <- rbind(C8084,C8589,C9094,C9599,C0004,C0509,C1014,C1515)
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

  # load Watson cells
  cells <- read.csv(file="C:/Users/danie/Dropbox/Werk/Demersal fish and fisheries/Data analysis/World_watson.csv",sep=";",header=T)

  # link to one degrees grid
  catchcoord <-data.frame(Longitude = cells$LonCentre , Latitude = cells$LatCentre)
  coordinates(catchcoord)<- ~ Longitude + Latitude  
  crs(catchcoord) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  tr <- over(catchcoord,grid_master)
  cells$uni_cell <- tr$uni_cell
  cells <- subset(cells,!(is.na(cells$uni_cell)))

  # get all cells for all years
  cells2 <- as.data.frame(lapply(cells, rep, length(min(Catch$Year):max(Catch$Year))))
  cells <- data.frame(cells2,Year = rep(min(Catch$Year):max(Catch$Year),each=nrow(cells)))
  cells$uni <- paste(cells$Cell,cells$Year)
  cells <- cbind(cells, Catch[match(cells$uni,Catch$uni), c("catch","catch_pel")])                    
  cells$catch[is.na(cells$catch)] <- 0
  cells$catch_pel[is.na(cells$catch_pel)] <- 0

  # now get the sum in each grid_master cell
  Fisheries <- aggregate(list(cells$catch,cells$catch_pel,cells$OceanAreasqkm), 
                         by= list(cells$uni_cell,cells$Year),FUN= sum)
  colnames(Fisheries) <-  c("uni_cell","Year","Catch","Catch_pel","Area_catch")
  Fisheries$Catch <- Fisheries$Catch *1000  # tonnes per cell per year ---> kg per cell per year
  Fisheries$Catch_sqkm <- Fisheries$Catch/Fisheries$Area_catch # kg per cell per year ---> kg per km2 per year
  Fisheries$Catch_pel <- Fisheries$Catch_pel *1000  # tonnes per cell per year ---> kg per cell per year
  Fisheries$Catch_pel_sqkm <- Fisheries$Catch_pel/Fisheries$Area_catch # kg per cell per year ---> kg per km2 per year
  Fisheries$uni <- paste(Fisheries$uni_cell,Fisheries$Year)

  cpue_good$uni <- paste(cpue_good$uni_cell,cpue_good$year)
  cpue_good <- cbind(cpue_good,Fisheries[match(cpue_good$uni,Fisheries$uni),c("Catch_sqkm","Catch_pel_sqkm")])
  cpue_good$Catch_sqkm[is.na(cpue_good$Catch_sqkm)] <- 0
  cpue_good$Catch_pel_sqkm[is.na(cpue_good$Catch_pel_sqkm)] <- 0

  # save data 
  save(cpue_good,file="cleaned data/Biomass_grid.RData")
