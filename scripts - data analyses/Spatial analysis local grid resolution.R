# spatial analysis all grid cells

rm(list=ls())

### load libraries
library(rgdal)
library(raster)
library(sp)
library(rgeos)
library(maptools)
library(dplyr)
library(sf)
library(spind)


##################
load("cleaned data/surveyed_grid.RData") # get grid information
load("cleaned data/Depth_grid.RData") # get depth per grid cell and year
load("cleaned data/Biomass_grid.RData") # get biomass per grid cell and year
load("cleaned data/sstdat_1967_2018_COBE.RData") # get SST COBE

# create function to run for all grid cells - three periods
getdat <- function(filename,t_start, t_end, sstcobe,grid){
  #  subset t_start : t_end
  cpue_final <- subset(filename,filename$year >= t_start & filename$year <= t_end)
  
  #  now estimate average biomass/ catch etc. per cell
  cpue_final <-  aggregate(list(cpue_final$biomass,cpue_final$tlw,cpue_final$Catch_sqkm),
                           by=list(cpue_final$uni_cell),FUN=mean,na.rm=T)
  colnames(cpue_final) <- c("uni_cell","biomass","tlw","Catch_sqkm")
  
  # combine and remove all cells without information (not sampled)
  grid <- cbind(grid,cpue_final[match(grid@data$uni_cell,cpue_final$uni_cell),c(2:4)])
  grid <- subset(grid,!(is.na(grid@data$biomass))) # remove all cells without information (not sampled)
  
  # add SST based on COBE timeseries data
  sstcobe <- data.frame(uni_cell = sstcobe$uni_cell, SST_time = rowMeans(sstcobe[,which(colnames(sstcobe) %in% t_start:t_end)]))
  grid <- cbind(grid,sstcobe[match(grid@data$uni_cell,sstcobe$uni_cell),c(2)])
  colnames(grid@data)[ncol(grid@data)] <- "SST_time"
  
  # prepare the data
  dat <- grid@data
  dat$ER_log <- log10((dat$Catch_sqkm + 1)/dat$biomass)
  dat$z_prod <-  dat$lz_prod + dat$mz_prod
  dat <- subset(dat,!(is.na(dat$z_prod)))
  dat <- subset(dat,!(is.na(dat$ben_prod)))
  dat <- subset(dat,!(is.na(dat$SST_time)))
  dat <- subset(dat,!(is.na(dat$tlw)))
  dat$biomass <- log10(dat$biomass/1000)
  dat$ben_prod <- 10*dat$ben_prod  # get detritus flux
  return(dat) 
}

# ------------------------------------------------------------------------------
# get the data
# ------------------------------------------------------------------------------

dat9095 <- getdat(cpue_good,1990,1995,sstdat,grid_master) 
dat0005 <- getdat(cpue_good,2000,2005,sstdat,grid_master) 
dat1015 <- getdat(cpue_good,2010,2015,sstdat,grid_master) 


  # Moran's I correlation coefficient
  #library(ape)
  #dat.dists <- as.matrix(dist(cbind(dat$long, dat$lat)))
  #dat.dists.inv <- 1/dat.dists
  #diag(dat.dists.inv) <- 0
  #dat.dists.inv[1:5, 1:5]
  #Moran.I(dat$biomass, dat.dists.inv)

### wavelets autocorrelation 1990-1995
  coords <- dat9095[ ,19:20]
  coords <- round(coords,digits =0)


  mwrm <- WRM(biomass ~ SST_time +  ER_log   + tlw + z_prod + ben_prod, family = "gaussian", 
              data = dat9095, coord = coords, level = 1, plot = TRUE)
  summary(mwrm)
  
  ### best model
  mwrm2 <- WRM(biomass ~ SST_time +  ER_log   + z_prod + ben_prod, family = "gaussian", 
              data = dat9095, coord = coords, level = 1, plot = TRUE)
  summary(mwrm2)

  mwrm$AIC;mwrm2$AIC
  
### wavelets autocorrelation 2000-2005
  coords <- dat0005[ ,19:20]
  coords <- round(coords,digits =0)
  
  mwrm <- WRM(biomass ~ SST_time +  ER_log   + tlw + z_prod + ben_prod, family = "gaussian", 
              data = dat0005, coord = coords, level = 1, plot = TRUE)
  summary(mwrm)
  
  ### best model
  mwrm2 <- WRM(biomass ~ SST_time +  ER_log   + z_prod + ben_prod, family = "gaussian", 
               data = dat0005, coord = coords, level = 1, plot = TRUE)
  summary(mwrm2)

  mwrm3 <- WRM(biomass ~ SST_time +  ER_log   + z_prod , family = "gaussian", 
               data = dat0005, coord = coords, level = 1, plot = TRUE)
  summary(mwrm3)
  
  mwrm$AIC;mwrm2$AIC;mwrm3$AIC
  
  ### wavelets autocorrelation 2010-2015
  coords <- dat1015[ ,19:20]
  coords <- round(coords,digits =0)
  
  mwrm <- WRM(biomass ~ SST_time +  ER_log   + tlw + z_prod + ben_prod, family = "gaussian", 
              data = dat1015, coord = coords, level = 1, plot = TRUE)
  summary(mwrm)
  mwrm2 <- WRM(biomass ~ SST_time +  ER_log   + z_prod + ben_prod, family = "gaussian", 
               data = dat1015, coord = coords, level = 1, plot = TRUE)
  summary(mwrm2)

  mwrm3 <- WRM(biomass ~ SST_time +  ER_log  + tlw + z_prod , family = "gaussian", 
               data = dat1015, coord = coords, level = 1, plot = TRUE)
  summary(mwrm3)
  
  mwrm4 <- WRM(biomass ~ SST_time +  ER_log   + z_prod , family = "gaussian", 
               data = dat1015, coord = coords, level = 1, plot = TRUE)
  summary(mwrm4)
  
  mwrm$AIC;mwrm2$AIC;mwrm3$AIC;mwrm4$AIC
  
  
