
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
  group_by(haulid,region,gear,year,month,lon,lat,type) %>%
  summarize_at(.vars=c('wtcpue', 'wtcpue_q','tlweight'), .funs = function(x) sum(x,na.rm=T)) %>% 
  dplyr::select(haulid,region,gear,year,month,lon,lat,type,wtcpue,wtcpue_q,tlweight) %>%
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
trawl <- trawl[,c('haulid','region','year','lon','lat',"wtcpue_q_Dem","tlw_Dem")]
colnames(trawl) <- c('haulid','region','year','lon','lat',"biomass","tlw")
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
  res$tlw[nb] <- (res$tlw[nb + 1] +  res$tlw[nb - 1]) / 2
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
  res$tlw[nb] <- (res$tlw[nb + 1] +  res$tlw[nb - 1]) / 2
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
                       tlw  =  cell$tlw [cell$year == dd],
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
Catch <- subset(Catch, Catch$Funcgroup %in% c(4,5,6,10:24))
#Catch <- subset(Catch, Catch$Funcgroup %in% c(6,12,15,18,22,24)) # only large fish
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
cpue_good$cell_tlw     <- cpue_good$tlw * cpue_good$ocean_sqkm

cpue_final <- subset(cpue_good,!(is.na(cpue_good$cell_catch)))

tt <-  aggregate(list(cpue_final$cell_biomass,cpue_final$cell_catch,cpue_final$cell_prod,cpue_final$cell_tlw ,cpue_final$ocean_sqkm),
                      by=list(cpue_final$ECO_REG,cpue_final$year),FUN=sum,na.rm=T)
colnames(tt) <- c("ECO_REG","year","biomass","catch","prod","tl","ocean_sqkm")

tt$biomass <- tt$biomass/tt$ocean_sqkm
tt$catch   <- tt$catch/tt$ocean_sqkm
tt$prod    <- tt$prod/tt$ocean_sqkm
tt$tl      <- tt$tl/tt$ocean_sqkm

### fit a model relationship biomass-ER
library(lme4)

tt$Lbio <- log10(tt$biomass)
tt$ER <- tt$catch / tt$biomass
tt$LER <- log10(tt$ER)


fish  <- lmer(Lbio ~ ER + (1 | ECO_REG), data = tt)

library(nlme)
fish2 <- lme(biomass ~ ER, random = ~ 1 | ECO_REG, data=tt)
fish3 <- lme(biomass ~ LER, random = ~ 1 | ECO_REG, data=tt)

fish4 <- lme(Lbio ~ LER, random = ~ 1 + LER | ECO_REG, data=tt)
fish5 <- lme(Lbio ~ LER, random = ~ 1 | ECO_REG, data=tt)
random <- ranef(fish5)
random2 <- ranef(fish4)
#####


 par(mfrow = c(3, 4))

 trcFunc <- function(x,a,b){((a*x)-(b*x^2))}

 timeser$bio <- NA
 timeser$prod <- NA
 timeser$catch <- NA
 timeser$tl <- NA
 timeser$biocor <- NA
 timeser$biocor2 <- NA
 
 for (j in 1:nrow(timeser)){
   reg_plot <- subset(tt,tt$ECO_REG == timeser$EcReg[j])
   reg_plot$ER <- reg_plot$catch/reg_plot$biomass
   plot(reg_plot$biomass  ~ reg_plot$ER,main = timeser$EcReg[j],xlim=c(0,max(reg_plot$ER)))
   
   x <- seq(0.001,0.5,length.out =500)
   y = 10^(3.7275267 - 0.4117613 *log10(x) + random$`(Intercept)`[which(rownames(random)==timeser$EcReg[j])])    
   y2 <- 10^(3.7388992 - 0.3966991 *log10(x)+ random2$`(Intercept)`[which(rownames(random2)==timeser$EcReg[j])]+
                random2$LER[which(rownames(random2)==timeser$EcReg[j])] *log10(x))
   lines(y~x)
   lines(y2~x,col="red")
   timeser$biocor[j] <- 10^(3.7388992 - 0.3966991 *log10(0.02)+ random2$`(Intercept)`[which(rownames(random2)==timeser$EcReg[j])]+
                           random2$LER[which(rownames(random2)==timeser$EcReg[j])] *log10(0.02))
   timeser$biocor2[j] <-10^(3.7275267 - 0.4117613 *log10(0.02) + random$`(Intercept)`[which(rownames(random)==timeser$EcReg[j])])    
   timeser$bio[j] <- mean(reg_plot$biomass)
   timeser$prod[j] <- mean(reg_plot$prod,na.rm=T)
   timeser$catch[j] <- mean(reg_plot$catch)
   timeser$tl[j] <- mean(reg_plot$tl)
 }
 
 par(mfrow= c(1,1))  
 ENV <- subset(grid_master@data,grid_master@data$uni %in% degrees)
 ENV <- aggregate(list(ENV$Depth,ENV$SST,ENV$NPP,ENV$chla,ENV$chla_wiffs,ENV$maxchla,ENV$lz_prod,ENV$ben_prod),
                  by=list(ENV$ECO_REG),FUN= mean ,na.rm=T)
 
 colnames(ENV) <- c("EcReg","Depth","SST","NPP","chla","chla_wiffs","maxchla","lz_prod","ben_prod")
 
 timeser <- cbind(timeser,ENV[match(timeser$EcReg,ENV$EcReg),c(2:9)])
 timeser$Depth <- abs(timeser$Depth)
 timeser$region <-  c(1,1,1,3,1,3,1,1,3,3,2,3,1,2,2,2,3,2,3,2,2) #c("a","a","a","a","p","p","a","a","p","p","a","p","a","a","a","a","p","a","p","a","a")
 timeser$ER <- timeser$catch/timeser$bio

 #setwd("C:/Users/danie/Desktop")
 #save(timeser,file = "dataRegions.Rdata")
 
 # first correct for fishing history
 dr <- timeser
 mod1 <- lm(dr$bio~dr$SST + dr$tl + log10(dr$ER))
 summary(mod1)
 
 mod1 <- lm(dr$bio~dr$SST  + log10(dr$ER))
 plot(residuals(mod1)~dr$tl,xlab="bioW trophic level",ylab="Residual biomass",pch=16)
 
 mod1 <- lm(dr$bio~dr$SST  + dr$tl)
 plot(residuals(mod1)~log10(dr$ER), xlab="log10(ER)",ylab="Residual biomass",pch=16)
 
 # now correct biomass for respiration costs
 timeser <- data.frame(timeser, resids = residuals(mod1))
 timeser$tcor <- timeser$bio/0.5^((timeser$SST-10)/10)
 param.Q10.^((tempdata(1:param.bottom+1 , (param.region+1))-10)/10);
 
 summary(lm(residuals(mod1)~timeser$SST * as.factor(timeser$region)))
 
 plot(residuals(mod1)~log10(timeser$ER))
 summary(lm(timeser$bio~timeser$SST))
 
 x= c(1,25)
 y= (67045.6 + -2862.1*x)
 lines(x=x,y=y)
 
 y2 <- c(67045,67045/2^2)
 lines(x=x,y=y2,col="blue")
 
 timeser$second <- timeser$lz_prod + timeser$ben_prod
 
 plot(timeser$second~timeser$SST,ylab="food for fish production",xlab="SST",pch=16,las=1)
 
 plot(log10(timeser$bio)~timeser$SST,ylab="log10(Biomass)",xlab="SST",las=1)
 points(log10(timeser$bio[timeser$region==1])~timeser$SST[timeser$region==1],col="blue",pch=16)
 points(log10(timeser$bio[timeser$region==2])~timeser$SST[timeser$region==2],col="orange",pch=16)
 points(log10(timeser$bio[timeser$region==3])~timeser$SST[timeser$region==3],col="red",pch=16)
 
 summary(lm(log10(timeser$bio)~timeser$SST))
 x=c(0,30)
 y1 = 4.9460 + - 0.0491*x
 y2 <- c(4.65,log10(10^4.65/1.9^3))
 lines(x,y1)
 lines(x,y2,lty=3)
 
 
 plot(log10(timeser$bio)~timeser$second,ylab="log10(Biomass)",xlab="ER",las=1)
 points(log10(timeser$bio[timeser$region==1])~timeser$ER[timeser$region==1],col="blue",pch=16)
 points(log10(timeser$bio[timeser$region==2])~timeser$ER[timeser$region==2],col="orange",pch=16)
 points(log10(timeser$bio[timeser$region==3])~timeser$ER[timeser$region==3],col="red",pch=16)
 
  # first correct for fishing history
 mod1 <- lm(timeser$bio~timeser$ER * as.factor(timeser$region))
 plot(residuals(mod1)~timeser$SST,ylab="(Biomass)_residuals",xlab="SST",las=1)
 points(residuals(mod1)[timeser$region==1]~timeser$SST[timeser$region==1],col="blue",pch=16)
 points(residuals(mod1)[timeser$region==2]~timeser$SST[timeser$region==2],col="orange",pch=16)
 points(residuals(mod1)[timeser$region==3]~timeser$SST[timeser$region==3],col="red",pch=16)
 
 library(ggplot2)
 library(jtools)

 timeser$bio_log <- log10(timeser$bio)
 mod1 <- lm(biozero~ SST + tl , data=timeser)
 summary(mod1)
 effect_plot(mod1, tl, interval = TRUE, plot.points = TRUE)
 
  