###########
# select all assessment areas that overlap with survey 
# select all species for these assessment areas
# select per species and assessment area the kg/km2 in the survey 
# multiply with ocean area and sum over the assessment area
# compare biomass of survey with biomass from stock assessment

##
library(ramlegacy)
library(dplyr)
library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(maptools)
library(tidyverse)
library(sf)

# downloads current latest version 4.44 
# download_ramlegacy()

# find it 
#ram_dir(vers = "4.44")

# and add folder to specific location
ram <- readRDS(paste(getwd(),'_noGIT/ramlegacy/4.44/RLSADB v4.44/DB Files With Assessment Data/v4.44.rds',sep=""))

# get estimate of area overlap between stock assessments and survey area
load("cleaned data/overlap_stockass_survey_area.RData")
colnames(final) <- c("areanb","overlap","name") # to simplify script

# now find all regions that overlap a large part of their area with the surveyed area in the ocean
fin <- subset(final,final$overlap > 0.3)

# select all stocks
stock <- ram$stock
stock <- subset(stock,stock$areaid %in% fin$name)
stock <- cbind(stock,fin[match(stock$areaid,fin$name),c(2)])
colnames(stock)[ncol(stock)] <- "overlap"

# get their average biomass between 2005 and 2009
ramdata   <- ram$timeseries_values_views
ramunits  <- ram$timeseries_units_views
rammethod <- ram$assessment
stock$SSB <- NA ;stock$SSB_units <- NA 
stock$TB  <- NA ;stock$TB_units <- NA 
stock$TC  <- NA ;stock$TC_units <- NA 
stock$TL  <- NA ;stock$TL_units <- NA 
stock$assessmethod <- NA

for(j in 1:nrow(stock)){
  # get biomass
  spec <- subset(ramdata,ramdata$stockid == stock$stockid[j] & ramdata$year %in% c(2000:2005))
  stock$SSB[j] <- mean(spec$SSB,na.rm=T) 
  stock$TB[j]  <- mean(spec$TB,na.rm=T)
  stock$TC[j]  <- mean(spec$TC,na.rm=T)
  stock$TL[j]  <- mean(spec$TL,na.rm=T)
  
  # get units
  if (nrow(spec)>0){
  specunits <- subset(ramunits,ramunits$stockid == stock$stockid[j])
  stock$SSB_units[j] <- specunits$SSB
  stock$TB_units[j]  <- specunits$TB
  stock$TC_units[j]  <- specunits$TC
  stock$TL_units[j]  <- specunits$TL
  stock_method <- subset(rammethod,rammethod$stockid == stock$stockid[j] & rammethod$mostrecent == 999)
  stock$assessmethod[j] <- stock_method$assessmethod
}}

stock$SSB <- ifelse(stock$SSB_units %in% c("E00","E00eggs","E00larvae","relative"), NA,stock$SSB) # all other values in metric tonnes (1000 kg)
stock$TB <- ifelse(stock$TB_units %in% c("E00","E00eggs","E00larvae","relative"), NA,stock$TB) # all other values in metric tonnes (1000 kg)
stock$biomass <- ifelse(is.na(stock$TB),stock$SSB,stock$TB)
stock <- subset(stock,!(is.na(stock$biomass)))

# some biomass is too low (probably still relative)
# remove all stocks where catch is higher than biomass 
stock$TC <- ifelse(stock$TC_units %in% c("E00","E00eggs","E00larvae"), NA,stock$TC) # all other values in metric tonnes (1000 kg)
stock$catch <- ifelse(is.na(stock$TC),stock$TL,stock$TC)
stock$frac <- stock$catch / stock$biomass # if larger than 1 unrealistic
stock <- subset(stock,stock$frac < 1)

# get cleaned survey data
source("scripts - data processing/source_combine_all_surveys_after_cleaning.R")
trawl <- subset(trawl, trawl$year %in% c(2000:2005))

# get unique location
trawl$uniq <- paste(trawl$lon,trawl$lat,sep="_")
coords_uni <- unique(trawl$uniq)
t <- strsplit(coords_uni, "_")
coords<- matrix(unlist(t), ncol=2, byrow=TRUE)
coords <- as.data.frame(coords)
coords[,1] <- as.numeric(as.character(coords[,1]))
coords[,2] <- as.numeric(as.character(coords[,2]))
coords[,3] <- coords_uni

# load surveyed grid
load("cleaned data/surveyed_grid.RData")

# combine observations with surveyed grid
coord <-data.frame(Longitude = coords[,1], Latitude = coords[,2])
coordinates(coord)<- ~ Longitude + Latitude  
crs(coord) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
grid_master <- spTransform(grid_master,CRS(proj4string(coord))) # make it similar to bargrid
grid_master@proj4string # check coordinates reference system again
#tr <- over(coord,grid_master)
#coords$uni_cell <- tr$uni_cell
#trawl <- cbind(trawl,coords[match(trawl$uniq,coords$V3),c("uni_cell")])
#colnames(trawl)[ncol(trawl)] <- "uni_cell"

# open James Rising assessment polygons
shape <- readOGR(dsn = paste(getwd(),'_noGIT/James Rising data/shapes',sep="") ,layer="ram")
crs(shape) <- CRS("+init=epsg:4326")

### load regions (IDs in shapefile match rownumber in csv file)
regions<-read.csv(paste(getwd(),'_noGIT/James Rising data/latlon.csv',sep = ""),header=T)
regions$SP_ID<-c(1:232)

# combine to get the region name -- note: the csv name order match the polygon order 
shape@data$Regions <- regions$name
shape <- subset(shape,shape@data$SP_ID %in% fin$areanb)

# estimate per species total biomass
stock$wt <- NA
stock$wt_q <- NA
stock$scientificname <- ifelse(stock$scientificname == "Theragra chalcogramma", "Gadus chalcogrammus",stock$scientificname)

# get land and remove the land part
land <- rnaturalearth::ne_countries(scale = 110, returnclass = "sp")
land <- aggregate(land)
proj4string(land)<-CRS("+init=epsg:4326")
landsf <- st_as_sf(land)

for (iStock in 1:nrow(stock)){
  print(iStock)
  reg <- st_as_sf(grid_master)
  coordmatch <- coords
  
  # get the grid cells that overlap with the assessment area
  ass_area <- stock$areaid[iStock]
  ass_nb   <- subset(regions,regions$name == ass_area)
  ass_nb   <- ass_nb$SP_ID
  #source("scripts - data analyses/Clean_stock_assessment_polygons.R")
  pol <- subset(shape,shape@data$SP_ID == ass_nb)
  pol <- spTransform(pol,CRS("+proj=longlat +datum=WGS84 +no_defs"))
  shape1 <- st_as_sf(pol)
  shape1 <- st_make_valid(shape1)
  shape1 <- st_difference(shape1,landsf)
  pi <- st_intersection(shape1,reg)

  # now get the overlap and move back to sp
  pi <- as_Spatial(pi)
  reg <- grid_master 
  pi@data$overlap <- area(pi) / 1000000
  reg <- cbind(reg,pi@data[match(reg@data$uni_cell,pi@data$uni_cell),c("overlap")])
  colnames(reg@data)[ncol(reg@data)] <- "overlap_kmsq"
  reg <- subset(reg,!(is.na(reg@data$overlap_kmsq)))

  if (stock$stocklong[iStock] == "Whiting Irish Sea"){
    reg <- subset(reg,!(reg@data$uni_cell %in% c("ID_986","ID_999")))
  }
  
  # search the trawl coordinates that overlap with the grid cells
  if (length(reg)>0){
    tr <- over(coord,reg)
    coordmatch$uni_cell <- tr$uni_cell
    coordmatch <- subset(coordmatch,!(is.na(coordmatch$uni_cell)))
    trawl_sub <- cbind(trawl,coordmatch[match(trawl$uniq,coordmatch$V3),c("uni_cell")])
    colnames(trawl_sub)[ncol(trawl_sub)] <- "uni_cell"
    trawl_sub <- subset(trawl_sub,!(is.na(trawl_sub$uni_cell)))
    
    # sum by haul and species
    trawl_sub <- trawl_sub %>% 
      group_by(haulid,year,uni_cell,spp) %>%
      summarize_at(.vars=c('wtcpue', 'wtcpue_q'), .funs = function(x) sum(x)) 
    
    # now get all unique hauls (zero fish should be part of the count)
    newdat <- data.frame(haulid = unique(trawl_sub$haulid))
    newdat <- cbind(newdat,trawl_sub[match(newdat$haulid,trawl_sub$haulid),c("year","uni_cell")])
    
    # now select the species and average kg/km2 per grid cell for each year
    trawl_sub <- subset(trawl_sub,trawl_sub$spp == stock$scientificname[iStock])
    
    # remove lowest and highest 2% of wgtcpue
    high <- trawl_sub %>%
      group_by(spp,year) %>%
      slice_max(wtcpue, prop = 0.02) 
    
    LH_ends <- c(unique(high$haulid))
    
    trawl_sub <- trawl_sub %>% 
      filter(!(haulid %in% LH_ends)) %>%
      as.data.frame()
    
    newdat <- cbind(newdat,trawl_sub[match(newdat$haulid,trawl_sub$haulid),c("wtcpue","wtcpue_q")])
    newdat$wtcpue[is.na(newdat$wtcpue)] <- 0
    newdat$wtcpue_q[is.na(newdat$wtcpue_q)] <- 0
    
    newdat <- newdat %>% 
      group_by(year,uni_cell) %>%
      summarize_at(.vars=c('wtcpue', 'wtcpue_q'), .funs = function(x) mean(x)) %>%
      as.data.frame
    
    #now link to the ocean km2 area of the grid cell
    newdat <- cbind(newdat,reg@data[match(newdat$uni_cell,reg@data$uni_cell),c("overlap_kmsq")])
    colnames(newdat)[ncol(newdat)] <- "overlap_kmsq"
    newdat$wt <- newdat$wtcpue * newdat$overlap_kmsq
    newdat$wt_q <- newdat$wtcpue_q * newdat$overlap_kmsq
    
    newdat <- newdat %>% 
      group_by(year) %>%
      summarize_at(.vars=c('wt', 'wt_q'), .funs = function(x) sum(x)) %>%
      as.data.frame
    
    stock$wt[iStock] <- mean(newdat$wt)/1000 # kg to metric tonnes (MT)
    stock$wt_q[iStock] <-  mean(newdat$wt_q)/1000 # kg to metric tonnes (MT)
  }}

noname <- subset(stock,stock$wt_q == 0) # most no names are correct (or invertebrates) but Theragra chalcogramma is renamed (above)
stock <- subset(stock,stock$wt_q > 0)
stock <- subset(stock,stock$wt_q < 10^100)

# herring spring and fall can be combined in same area
herring <- subset(stock,stock$stockid %in% c("HERR4TFA","HERR4TSP"))
herring_new <- herring[1,]
herring_new$stockid <- "HERR4TSP_TFA"
herring_new$stocklong <- "Herring NAFO 4T fall and spring spawners"
herring_new$SSB <- sum(herring$SSB)
herring_new$TB <- sum(herring$TB)
herring_new$TL <- sum(herring$TL)
herring_new$biomass <- sum(herring$biomass)
herring_new$catch   <- sum(herring$catch)
herring_new$frac <- mean(herring$frac)
stock <- subset(stock,!(stock$stockid %in% c("HERR4TFA","HERR4TSP")))
stock <- rbind(stock,herring_new)

stocksave <- stock
colnames(stocksave)[10] <- "area_overlap_in_ocean"
colnames(stocksave)[23] <- "survey_biomass_uncorrected"
colnames(stocksave)[24] <- "survey_biomass_corrected"

write.csv(stocksave,file="cleaned data/stock assessment comparison.csv",row.names = F)
