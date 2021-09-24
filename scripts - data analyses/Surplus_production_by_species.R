
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

# get species separate
trawl_sep <- trawl %>% 
  group_by(haulid,region,gear,year,month,lon,lat,spp) %>%
  summarize_at(.vars=c('wtcpue', 'wtcpue_q'), .funs = function(x) sum(x)) %>% 
  dplyr::select(haulid,region,gear,year,month,lon,lat,spp,wtcpue,wtcpue_q) %>%
  as.data.frame()

# now get all stations and years with data
trawl <- trawl %>% 
  group_by(haulid,region,gear,year,month,lon,lat) %>%
  summarize_at(.vars=c('wtcpue', 'wtcpue_q'), .funs = function(x) sum(x)) %>% 
  dplyr::select(haulid,region,gear,year,month,lon,lat,wtcpue,wtcpue_q) %>%
  as.data.frame()

# add the type, size, species to estimate surplus production 
trawl_sep <- subset(trawl_sep,trawl_sep$type =="dem")
colnames(trawl_sep)[which(colnames(trawl_sep) %in% c("wtcpue","wtcpue_q"))] <- c("wtcpue_Dem","wtcpue_q_Dem")
trawl <- cbind(trawl,trawl_sep[match(trawl$haulid,trawl_sep$haulid), c("wtcpue_Dem","wtcpue_q_Dem")])
trawl <- trawl[,c('haulid','region','year','lon','lat',"wtcpue_q_Dem")]
colnames(trawl) <- c('haulid','region','year','lon','lat',"biomass")
trawl$biomass <- ifelse(is.na(trawl$biomass),0,trawl$biomass)
trawl <- subset(trawl,trawl$biomass < 10^100)

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
  summarise_at (c("biomass"),mean, na.rm=T) %>%
  as.data.frame ()
cpue$uni <- paste(cpue$one_degrees,cpue$year)
cpue$year <- as.numeric(cpue$year)

cpue <- cbind(cpue,grid_master@data[match(cpue$one_degrees,grid_master@data$uni),c("ECO_REG")])
colnames(cpue)[ncol(cpue)] <- "ECO_REG" 
cpue <- subset(cpue,!(is.na(cpue$one_degrees)))

rm(list=setdiff(ls(), c("cpue","grid_master")))

# now get all grid cells with multiple years of data, to be used for surplus production
source("scripts - data analyses/Surplus_production_source_gridcells_timeseries.R")

# remove all NAs
cpue <- subset(cpue,!(is.na(cpue$biomass)))

##########
#### for two regions - part of the survey is in a different year and not all years are sampled
##########

# region 1 - North American Pacific Fijordland
degrees_sub <- c(25607,25967,26327,26328,28132,28492)  # grid cells with biomass estimate in "wrong" year
years <- data.frame(year = c(2003,2005,2007,2009,2011,2013,2015,2017,2019) ) # years for which data is needed

# get for all points with data +- 1 year the average
for (ideg in 1:length(degrees_sub)){
  print(ideg)
  cell <- subset(cpue,cpue$one_degrees == degrees_sub[ideg])
  res <- merge(cell,years,by.x='year',by.y='year',all.x=T,all.y=T)
  res <- res %>% fill(one_degrees,ECO_REG, .direction = "updown")
  res$uni <-paste(res$one_degrees,res$year)
  nb <- which(is.na(res$biomass))
  nb <- subset(nb,!(nb %in% c(1,nrow(res))))
  print(nb)
  res$biomass[nb] <- (res$biomass[nb + 1] +  res$biomass[nb - 1]) / 2
  cpue <- subset(cpue,!(cpue$one_degrees == degrees_sub[ideg]))
  cpue <- rbind(cpue,res)
}

# region 2 - Aleutian Islands 
degrees_sub <- c(25575, 25935, 26292, 26293, 26294, 26295, 26651, 26652,26653, 27011)   # grid cells with biomass estimate in "wrong" year
years <- data.frame(year = c(1983,1986,1991,1994,1997,2000,2002,2004,2006,2010,2012,2014,2016,2018) ) # years for which data is needed

for (ideg in 1:length(degrees_sub)){
  print(ideg)
  cell <- subset(cpue,cpue$one_degrees == degrees_sub[ideg])
  res <- merge(cell,years,by.x='year',by.y='year',all.x=T,all.y=T)
  res <- res %>% fill(one_degrees,ECO_REG, .direction = "updown")
  res$uni <-paste(res$one_degrees,res$year)
  nb <- which(is.na(res$biomass))
  nb <- subset(nb,!(nb %in% c(1,nrow(res))))
  print(nb)
  res$biomass[nb] <- (res$biomass[nb + 1] +  res$biomass[nb - 1]) / 2
  cpue <- subset(cpue,!(cpue$one_degrees ==  degrees_sub[ideg]))
  cpue <- rbind(cpue,res)
}

# remove again all NAs 
cpue <- subset(cpue,!(is.na(cpue$biomass)))

########
### now that some time gaps are filled in, other spatial gaps will be filled based 
### on information in the cell and the annual increase/decline of the total area relative
### to the reference year

# first remove all data from years with insufficient data
cpue_good <- c()
for (iReg in 1:nrow(timeser)){
  regdat <- subset(cpue,cpue$ECO_REG == timeser$EcReg[iReg])
  year_series <- subset(timeser,timeser$EcReg == regdat$ECO_REG[1])
  
  if (regdat$ECO_REG[1] == "Gulf of Alaska"){
    years <- c(1984, 1987, 1990, 1993, 1996, 1999, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019)
    
  } else if(regdat$ECO_REG[1] == "Oregon, Washington, Vancouver Coast and Shelf") {
    years <- c(1980, 1983, 1986, 1989, 1992, 1995, 1998, 2001, 2003, 2004, 2005, 2006, 2007, 
               2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018) 
    
  } else if(regdat$ECO_REG[1] ==  "Northern California") {
    years <- c(1989, 1992, 1995, 1998, 2001, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010,
               2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)
    
  } else if(regdat$ECO_REG[1] == "Aleutian Islands") {
    years   <- c(1983,1986,1991,1994,1997,2000,2002,2004,2006,2010,2012,2014,2016,2018)
    
  } else if(regdat$ECO_REG[1] == "North American Pacific Fijordland") {
    
    years   <- c(2003,2005,2007,2009,2011,2013,2015,2017,2019)
    
  } else { years <- c(year_series[,2]:year_series[,3]) }
  
  regdat <- subset(regdat,regdat$year %in% years)  
  
  cpue_good <- rbind(cpue_good,regdat)  
}

# keep data without filled spatial gaps
cpue_real <- cpue_good

# fill spatial gaps
for (ideg in 1:length(degrees)){
  cell <- subset(cpue_good,cpue_good$one_degrees == degrees[ideg]) # select 1 cell
  year_series <- subset(timeser,timeser$EcReg == cell$ECO_REG[1])  # get the time series info 
  years <- sort(unique(cpue_good$year[cpue_good$ECO_REG ==cell$ECO_REG[1]])) # get all years with data in the region
  
  year_out <- years[which(!(years %in% cell$year))]  # check which years have no data in the cell
  year_in  <- years[which((years %in% cell$year))]  # check which years have data in the cell
  
  if(length(year_out) > 0){
    for (iYear in 1:length(year_out)){
      dd <- year_in[which(abs(year_in-year_out[iYear])==min(abs(year_in-year_out[iYear])))][1]
      
      # calculate biomass difference between reference year and year to fill data for the region 
      reg  <- subset(cpue_real,cpue_real$ECO_REG == cell$ECO_REG[1]) 
      reg  <- subset(reg, reg$year %in% c(year_out[iYear], dd))
      reg  <- reg[reg$one_degrees %in%  names(table(reg$one_degrees))[table(reg$one_degrees) == 2] , ]
      corfactor <-  mean(reg$biomass[reg$year == year_out[iYear]])/ mean(reg$biomass[reg$year == dd])
      
      # get, per year, new information and bind to the dataset
      newdat <- data.frame(one_degrees = degrees[ideg],year = year_out[iYear],
                           biomass =  cell$biomass[cell$year == dd] * corfactor,
                           uni = paste(degrees[ideg],year_out[iYear]),
                           ECO_REG =  cell$ECO_REG[1])
      cpue_good <- rbind(cpue_good,newdat) 
    }}}

# data is ready, remove all grid cells that are not part of time series
cpue_good <- subset(cpue_good,cpue_good$one_degrees %in% degrees)

# get ocean area
cpue_good <- cbind(cpue_good,grid_master@data[match(cpue_good$one_degrees,grid_master@data$uni),c("ocean_sqkm")])
colnames(cpue_good)[ncol(cpue_good)] <- "ocean_sqkm" 

#########
### add fisheries landings information
########

# load Regs fisheries database (v4.0)
C8084 <- readRDS("C:/Users/danie/Dropbox/Werk/Demersal fish and fisheries/Data analysis/Catch_all_1980_1984.rds")
C8589 <- readRDS("C:/Users/danie/Dropbox/Werk/Demersal fish and fisheries/Data analysis/Catch_all_1985_1989.rds")
C9094 <- readRDS("C:/Users/danie/Dropbox/Werk/Demersal fish and fisheries/Data analysis/Catch_all_1990_1994.rds")
C9599 <- readRDS("C:/Users/danie/Dropbox/Werk/Demersal fish and fisheries/Data analysis/Catch_all_1995_1999.rds")
C0004 <- readRDS("C:/Users/danie/Dropbox/Werk/Demersal fish and fisheries/Data analysis/Catch_all_2000_2004.rds")
C0509 <- readRDS("C:/Users/danie/Dropbox/Werk/Demersal fish and fisheries/Data analysis/Catch_all_2005_2009.rds")
C1014 <- readRDS("C:/Users/danie/Dropbox/Werk/Demersal fish and fisheries/Data analysis/Catch_all_2010_2014.rds")
C1515 <- readRDS("C:/Users/danie/Dropbox/Werk/Demersal fish and fisheries/Data analysis/Catch_all_2015_2015.rds")

### total catch for all demersal groups
Catch <- rbind(C8084,C8589,C9094,C9599,C0004,C0509,C1014,C1515)
#Catch <- subset(Catch, Catch$Funcgroup %in% c(4,5,6,10:24))
Catch <- subset(Catch, Catch$Funcgroup %in% c(6,12,15,18,22,24)) # only large fish
Catch$Tot <- Catch$Reported + Catch$IUU + Catch$Discards
Catch <- aggregate(Catch$Tot, by= list(Catch$Cell,Catch$IYear),FUN= sum,na.rm=T)
colnames(Catch) <- c("Cell","Year","catch")
Catch$uni <- paste(Catch$Cell,Catch$Year)

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
cells <- cbind(cells, Catch[match(cells$uni,Catch$uni), c("catch")])                    
colnames(cells)[ncol(cells)] <- "catch"
cells$catch[is.na(cells$catch)] <- 0

# now get the sum in each grid_master cell
Fisheries <- aggregate(list(cells$catch,cells$OceanAreasqkm), by= list(cells$one_degrees,cells$Year),FUN= sum)
colnames(Fisheries) <-  c("one_degrees","Year","Catch","Area_catch")
Fisheries$Catch <- Fisheries$Catch *1000  # tonnes per cell per year ---> kg per cell per year
Fisheries$Catch_sqkm <- Fisheries$Catch/Fisheries$Area_catch # kg per cell per year ---> kg per km2 per year
Fisheries$uni <- paste(Fisheries$one_degrees,Fisheries$Year)

#########
### estimate aggregated surplus production based on biomass(t+1) - biomass(t) + catch(t)
########
prod <- data.frame(uni =NA, prod =NA)

for (ideg in 1:length(degrees)){
  cell <- subset(cpue_good,cpue_good$one_degrees == degrees[ideg])
  cell <- cell[order(cell$year),] 
  cell$biomass_plusone <- c(cell$biomass[2:nrow(cell)],NA)
  cell$year_plusone <- c(cell$year[2:nrow(cell)],NA)
  
  # in some surveys not all years are sampled -- get sum of catch for all years
  years <- data.frame(year = c(min(cell$year):max(cell$year)))
  cell_all <- merge(cell,years,by.x='year',by.y='year',all.x=T,all.y=T)
  cell_all <- cell_all %>% fill(one_degrees,ECO_REG, .direction = "updown")
  cell_all$uni <-paste(cell_all$one_degrees,cell_all$year)
  cell_all <- cbind(cell_all,Fisheries[match(cell_all$uni,Fisheries$uni),c("Catch_sqkm")])
  colnames(cell_all)[ncol(cell_all)] <- "Catch_sqkm" 
  
  nb    <- which(!(is.na(cell_all$biomass))) # run the index to get the correct years for the catch
  nb_gr <- c(nb[-1],nb[length(nb)]) 
  cell_all$idx          <- c(rep(nb,(nb_gr-nb)),NA)
  totcatch <- aggregate(cell_all$Catch_sqkm,by=list(cell_all$idx),sum)
  cell_all <- cbind(cell_all,totcatch[match(cell_all$idx,totcatch$Group.1),c("x")])
  colnames(cell_all)[ncol(cell_all)] <- "totCatch" 
  
  # sum of catch
  cell <- cbind(cell,cell_all[match(cell$uni,cell_all$uni),c("totCatch")]) 
  colnames(cell)[ncol(cell)] <- "totCatch" 
  
  # estimate surplus production in kg per km2 per year
  cell$prod <- (cell$biomass_plusone - cell$biomass + cell$totCatch)/(cell$year_plusone-cell$year)  
  prod <- rbind(prod,cell[,c('uni','prod')])
}

cpue_good <- cbind(cpue_good,prod[match(cpue_good$uni,prod[,1]),c(2)])
colnames(cpue_good)[ncol(cpue_good)] <- "prod"

cpue_good <- cbind(cpue_good,Fisheries[match(cpue_good$uni,Fisheries$uni),c("Catch_sqkm")])
colnames(cpue_good)[ncol(cpue_good)] <- "Catch_sqkm" 

cpue_good$cell_biomass <- cpue_good$biomass * cpue_good$ocean_sqkm
cpue_good$cell_catch   <- cpue_good$Catch_sqkm * cpue_good$ocean_sqkm
cpue_good$cell_prod    <- cpue_good$prod * cpue_good$ocean_sqkm

cpue_final <- subset(cpue_good,!(is.na(cpue_good$cell_prod)))

# remove lowest and highest 2% of biomass per survey
high <- cpue_final %>%
  group_by(ECO_REG,year)  %>%
  slice_max(biomass, prop = 0.02) 

low <- cpue_final %>%
  group_by(ECO_REG,year)  %>%
  slice_min(biomass, prop = 0.02) 

LH_ends <- c(unique(high$uni),unique(low$uni))

cpue_final <- cpue_final %>% 
  filter(!(uni %in% LH_ends)) %>%
  as.data.frame()


tt <-  aggregate(list(cpue_final$cell_biomass,cpue_final$cell_catch,cpue_final$cell_prod,cpue_final$ocean_sqkm),
                 by=list(cpue_final$ECO_REG,cpue_final$year),FUN=sum)
colnames(tt) <- c("ECO_REG","year","biomass","catch","prod","ocean_sqkm")

tt$biomass <- tt$biomass/tt$ocean_sqkm
tt$catch   <- tt$catch/tt$ocean_sqkm
tt$prod    <- tt$prod/tt$ocean_sqkm

par(mfrow = c(3, 4))

trcFunc <- function(x,a,b){((a*x)-(b*x^2))}
timeser$Bmsy <- NA
timeser$bio00_05 <- NA

for (j in 1:nrow(timeser)){
  reg_plot <- subset(tt,tt$ECO_REG == timeser$EcReg[j])
  plot(reg_plot$biomass,reg_plot$prod,pch=16,xlab="Biomass kg/km2", ylab="Surplus prod kg/km2/y",
       main = timeser$EcReg[j],xlim=c(1,max(reg_plot$biomass)))
  
  x <- reg_plot$biomass
  y <- reg_plot$prod
  
  mfit <- nls(y~trcFunc(x,a,b),start=list(a=1,b=0.1))
  x <- c(1:max(reg_plot$biomass))
  y <- coef(mfit)[1]*x - coef(mfit)[2]*x^2
  lines(y~x)
  timeser$Bmsy[j] <- coef(mfit)[1]/(2*coef(mfit)[2])
  timeser$bio00_05[j] <- mean(reg_plot$biomass[reg_plot$year %in% c(2000:2005)])
}

timeser$state <- timeser$bio00_05/timeser$Bmsy

load("cleaned data/Ecoregion_environment_info.Rdata") ## redo info in C:\Users\danie\Documents\Online for git\FishGrowth\Data

# and C:\Users\danie\Dropbox\Werk\Archief\2018 Nat Eco Evo\Final_analysis_manuscript\Environmental_data_Ecoregions

timeser <- cbind(timeser,dat[match(timeser$EcReg,dat$Name_spalding),c(4,5,7,8,12,14)])
timeser$region <- c("a","a","a","p","a","p","a","a","p","p","a","p","a","a","a","a","p","a","p","a","a")

NS <- subset(tt,tt$ECO_REG =="North Sea")
plot(NS$biomass ~ NS$year,type="l")

