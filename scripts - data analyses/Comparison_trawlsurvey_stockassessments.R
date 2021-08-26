
library(ramlegacy)
library(dplyr)
library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(maptools)
library(tidyverse)

# downloads current latest version 4.44 
# download_ramlegacy()

# find it 
#ram_dir(vers = "4.44")

# and add folder to specific location
ram <- readRDS(paste(getwd(),'_noGIT/ramlegacy/4.44/RLSADB v4.44/DB Files With Assessment Data/v4.44.rds',sep=""))

# get estimate of area overlap between stock assessments and survey area
load("cleaned data/overlap_stockass_survey_area.RData")
colnames(final) <- c("areanb","overlap","fr_land","name") # to simplify script

# now find all species that overlap a large part of their area in the ocean
final$ocean <- 1-final$fr_land
final$toanalyze <- final$overlap / final$ocean *100 # percentage of overlap relative to total ocean area (larger than 100% is possible, some grid cells are partly on land)
fin <- subset(final,final$toanalyze > 10)

# select all stocks
stock <- ram$stock
stock <- subset(stock,stock$areaid %in% fin$name)
stock <- cbind(stock,fin[match(stock$areaid,fin$name),c(2,6)])

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
# remove all stocks where catch is much higher than biomass 
stock$TC <- ifelse(stock$TC_units %in% c("E00","E00eggs","E00larvae"), NA,stock$TC) # all other values in metric tonnes (1000 kg)
stock$catch <- ifelse(is.na(stock$TC),stock$TL,stock$TC)
stock$frac <- stock$catch / stock$biomass # if much larger than 1 unrealistic
stock <- subset(stock,stock$frac < 6)

#### now get one degrees grid for the surveyed area and calculate surface (minus land)

# get cleaned survey data
source("scripts - data processing/source_combine_all_surveys_after_cleaning.R")
trawl_backup <- trawl

# get unique location
trawl$uniq <- paste(trawl$lon,trawl$lat,sep="_")
coords_uni <- unique(trawl$uniq)
t <- strsplit(coords_uni, "_")
coords<- matrix(unlist(t), ncol=2, byrow=TRUE)
coords <- as.data.frame(coords)
coords[,1] <- as.numeric(as.character(coords[,1]))
coords[,2] <- as.numeric(as.character(coords[,2]))
coords[,3] <- coords_uni

# assign area of interest one degrees
gt<-(GridTopology(c(-179.5, -89.75), c(1, .5), c(360*1, 180*2))) # c(long, lat), c(cellsize long, lat), c(nb of grids long, lat)
grt<-SpatialGrid(gt, proj4string=CRS("+init=epsg:4326"))
spix <- as(grt, "SpatialPixels")
spol <- as(spix, "SpatialPolygons")
rnames<-sapply(slot(spol, "polygons"), function(x) slot(x, "ID"))
LOCUNI<-as.data.frame(seq(1,length(spix)))
rownames(LOCUNI)<-rnames
bargrid<-SpatialPolygonsDataFrame(spol, LOCUNI)
bargrid@bbox # make sure "min" is a whole number
colnames(bargrid@data)[1] <- "uni" 

coord <-data.frame(Longitude = coords[,1], Latitude = coords[,2])
coordinates(coord)<- ~ Longitude + Latitude  
crs(coord) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
bargrid <- spTransform(bargrid,CRS(proj4string(coord))) # make it similar to bargrid
bargrid@proj4string # check coordinates reference system again
tr <- over(coord,bargrid)
coords$one_degrees <- tr$uni

# select all with survey data
bargrid <- subset(bargrid,bargrid@data$uni %in% coords$one_degrees)
bargrid@data$kmsq <- area(bargrid)/1000000

land <- rnaturalearth::ne_countries(scale = 110, returnclass = "sp")
land <- aggregate(land)

pi<-gIntersection(bargrid,land,byid = TRUE)
res <- data.frame( ID = sapply(slot(pi, "polygons"), function(x) slot(x, "ID")), land = area(pi) / 1000000)
res$ID <- gsub("g", "", res$ID)
res <- res %>% separate(ID, c("A", NA))

bargrid <- cbind(bargrid,res[match(bargrid$uni,res$ID),c(2)])
colnames(bargrid@data)[ncol(bargrid@data)] <- "land_kmsq"
bargrid@data$land_kmsq[is.na(bargrid@data$land_kmsq)] <- 0
bargrid@data$ocean_kmsq <- bargrid@data$kmsq - bargrid@data$land_kmsq

# select per species and assessment area the kg/km2, multiply with ocean area and sum over the assessment area
trawl <- subset(trawl, trawl$year %in% c(2000:2005))

# get coordinates
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
bargrid <- spTransform(bargrid,CRS(proj4string(coord))) # make it similar to bargrid

# open James Rising assessment polygons
shape <- readOGR(dsn = paste(getwd(),'_noGIT/James Rising data/shapes',sep="") ,layer="ram")
crs(shape) <- CRS("+init=epsg:4326")

### load regions (IDs in shapefile match rownumber in csv file)
regions<-read.csv(paste(getwd(),'_noGIT/James Rising data/latlon.csv',sep = ""),header=T)
regions$SP_ID<-c(1:232)

# combine to get the region name -- note: the csv name order match the polygon order 
shape@data$Regions <- regions$name

stock$wt <- NA
stock$wt_q <- NA
stock$scientificname <- ifelse(stock$scientificname == "Theragra chalcogramma", "Gadus chalcogrammus",stock$scientificname)

for (iStock in 1:nrow(stock)){
  print(iStock)
  reg <- bargrid
  coordmatch <- coords
  
  # get the grid cells that overlap with the assessment area
  ass_area <- stock$areaid[iStock]
  ass_nb   <- subset(regions,regions$name == ass_area)
  ass_nb   <- ass_nb$SP_ID
  source("scripts - data analyses/Clean_stock_assessment_polygons.R")
  pol <- shape1
  pol <- spTransform(pol,CRS("+proj=longlat +datum=WGS84 +no_defs"))
  
  pi <- intersect(pol,reg)
  pi@data$overlap <- area(pi) / 1000000
  reg <- cbind(reg,pi@data[match(reg@data$uni,pi@data$uni),c("overlap")])
  colnames(reg@data)[ncol(reg@data)] <- "overlap_kmsq"
  reg@data$frac <- reg@data$overlap_kmsq / reg@data$ocean_kmsq
  reg <- subset(reg,reg@data$frac >= 1)
  
  # search the trawl coordinates that overlap with the grid cells
  if (length(reg)>0){
  tr <- over(coord,reg)
  coordmatch$one_degrees <- tr$uni
  coordmatch <- subset(coordmatch,!(is.na(coordmatch$one_degrees)))
  trawl_sub <- cbind(trawl,coordmatch[match(trawl$uniq,coordmatch$V3),c("one_degrees")])
  colnames(trawl_sub)[ncol(trawl_sub)] <- "one_degrees"
  trawl_sub <- subset(trawl_sub,!(is.na(trawl_sub$one_degrees)))
  
  # sum by haul and species
  trawl_sub <- trawl_sub %>% 
    group_by(haulid,year,one_degrees,spp) %>%
    summarize_at(.vars=c('wtcpue', 'wtcpue_q'), .funs = function(x) sum(x)) 
  
  # now get all unique hauls (zero fish should be part of the count)
  newdat <- data.frame(haulid = unique(trawl_sub$haulid))
  newdat <- cbind(newdat,trawl_sub[match(newdat$haulid,trawl_sub$haulid),c("year","one_degrees")])
  
  # now select the species and average kg/km2 per grid cell for each year
  trawl_sub <- subset(trawl_sub,trawl_sub$spp == stock$scientificname[iStock])
  
  # remove lowest and highest 2% of wgtcpue
  high <- trawl_sub %>%
    group_by(spp,year) %>%
    slice_max(wtcpue, prop = 0.02) 
  
  low <- trawl_sub %>%
    group_by(spp,year) %>%
   slice_min(wtcpue, prop = 0.02) 
  
  LH_ends <- c(unique(high$haulid),unique(low$haulid))
  
  trawl_sub <- trawl_sub %>% 
    filter(!(haulid %in% LH_ends)) %>%
    as.data.frame()
  
  newdat <- newdat %>% 
    filter(!(haulid %in% LH_ends)) %>%
    as.data.frame()
  
  newdat <- cbind(newdat,trawl_sub[match(newdat$haulid,trawl_sub$haulid),c("wtcpue","wtcpue_q")])
  newdat$wtcpue[is.na(newdat$wtcpue)] <- 0
  newdat$wtcpue_q[is.na(newdat$wtcpue_q)] <- 0
  
  newdat <- newdat %>% 
    group_by(year,one_degrees) %>%
    summarize_at(.vars=c('wtcpue', 'wtcpue_q'), .funs = function(x) mean(x)) %>%
    as.data.frame
    
  #now link to the ocean km2 area of the grid cell
  newdat <- cbind(newdat,reg@data[match(newdat$one_degrees,reg@data$uni),c("ocean_kmsq")])
  colnames(newdat)[ncol(newdat)] <- "ocean_kmsq"
  newdat$wt <- newdat$wtcpue * newdat$ocean_kmsq
  newdat$wt_q <- newdat$wtcpue_q * newdat$ocean_kmsq

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
stocksave <- stocksave[,c(1:7,11:22,24,25)]
stocksave$toanalyze <- ifelse(stocksave$toanalyze > 100, 100,stocksave$toanalyze )
colnames(stocksave)[8] <- "area_overlap_perc"
colnames(stocksave)[20] <- "survey_biomass_uncorrected"
colnames(stocksave)[21] <- "survey_biomass_corrected"
write.csv(stocksave,file="cleaned data/stock assessment comparison.csv",row.names = F)

plot(log10(stock$biomass),log10(stock$wt_q),xlim=c(0,8),ylim=c(0,8),xlab="stock assess. biomass - log10(metric tonnes)",
     ylab="trawl survey biomass - log10(metric tonnes)",main="gear eff. corrected",las=1,pch=16)
abline(0,1)
#points(log10(stock$biomass[stock$toanalyze <30]),log10(stock$wt_q[stock$toanalyze <30]),col="blue",pch=16)


plot(log10(stock$biomass),log10(stock$wt),xlim=c(0,8),ylim=c(0,8),xlab="stock assess. biomass - log10(metric tonnes)",
     ylab="trawl survey biomass - log10(metric tonnes)",main="gear uncorrected",las=1,pch=16)
abline(0,1)

# check per region
pac <- subset(stock,stock$region %in% c("US West Coast","Canada West Coast","US Alaska"))

plot(log10(pac$biomass),log10(pac$wt_q),xlim=c(0,8),ylim=c(0,8),xlab="stock assess. biomass",
     ylab="trawl survey biomass",main="westCoast - gear cor",las=1,pch=16)
abline(0,1)
pac$diff <- pac$biomass - pac$wt_q
pac$diff <- log10(pac$biomass) - log10(pac$wt_q)
tt <- subset(pac,pac$diff >2)
points(log10(tt$biomass[6]), log10(tt$wt_q[6]),pch=16,col="blue")

  EastC <- subset(stock,stock$region %in% c("US East Coast","Canada East Coast","US Southeast and Gulf"))

plot(log10(EastC$biomass),log10(EastC$wt_q),xlim=c(0,8),ylim=c(0,8),xlab="stock assess. biomass",
     ylab="trawl survey biomass",main="eastCoast - gear cor",las=1,pch=16)
abline(0,1)
EastC$diff <- EastC$biomass - EastC$wt_q
EastC$diff <- log10(EastC$biomass) - log10(EastC$wt_q)
tt <- subset(EastC,EastC$diff >2)
points(log10(tt$biomass[2]), log10(tt$wt_q[2]),pch=16,col="blue")


EU <- subset(stock,stock$region %in% c("European Union","Europe non EU"))

plot(log10(EU$biomass),log10(EU$wt_q),xlim=c(0,8),ylim=c(0,8),xlab="stock assess. biomass",
     ylab="trawl survey biomass",main="EU - gear cor",las=1,pch=16)
abline(0,1)
EU$diff <- EU$biomass - EU$wt_q
EU$diff <- log10(EU$biomass) - log10(EU$wt_q)
tt <- subset(EU,EU$diff >2)
points(log10(tt$biomass[1]), log10(tt$wt_q[1]),pch=16,col="blue")
tt <- subset(EU,EU$diff < -2)



mod1 <- lm(log10(stock$biomass)~ log10(stock$wt_q))
summary(mod1)

rmse(log10(stock$biomass),log10(stock$wt_q))


stock$diff <- log10(stock$biomass) - log10(stock$wt_q)
points(log10(stock$biomass[stock$diff < 1 & stock$diff > -1]), log10(stock$wt_q[stock$diff < 1 & stock$diff > -1]),col="blue",pch=16)

subset(stock,stock$diff < -1)

subset(stock,stock$diff > 1)
