
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

##################
load("cleaned data/surveyed_grid.RData")
load("cleaned data/211216_depth_grid.RData")
load("cleaned data/211216_biomass_grid.RData")

################
# create data for 
# 1) 3 time periods x 2 different regional boundaries
# 2) once by only selecting depths 50 - 250 m - time period xxxx
# 3) once for all grid cells - time period xxxx

#  now estimate average biomass/ catch per cell for 2001-2010
cpue_final <- subset(cpue_good,cpue_good$year > 2001 & cpue_good$year < 2010)
tt <-  aggregate(list(cpue_final$biomass,cpue_final$tlw,
                      cpue_final$Catch_sqkm,cpue_final$Catch_pel_sqkm),
                 by=list(cpue_final$uni_cell),FUN=mean,na.rm=T)
colnames(tt) <- c("uni_cell","biomass","tlw","Catch_sqkm","Catch_pel_sqkm")

grid_master <- cbind(grid_master,tt[match(grid_master@data$uni_cell,tt$uni_cell),c(2:5)])
grid_master <- subset(grid_master,!(is.na(grid_master@data$biomass))) # remove all cells without information (not sampled)

# obtain measured depth 
grid_master <- cbind(grid_master,depth_grid[match(grid_master@data$uni_cell,depth_grid$uni_cell),c("depth")])
colnames(grid_master@data)[ncol(grid_master@data)] <- "depth"

# convert mapped depth to positive
grid_master@data$Depth_map <- abs(grid_master@data$Depth_map)

# save data 
save(grid_master,file="cleaned data/final_2001_10biomass_grid.RData")

# now get estimate per ecoregion 
cpue_good <- grid_master@data
cpue_good[,c(7:18,22:26)]  <- cpue_good[,c(7:18,22:26)] * cpue_good$ocean_sqkm # weighted with the size of the ocean area

tt <-  aggregate(list(cpue_good[,c(4,7:18,22:26)]),
                 by=list(cpue_good$ECO_REG),FUN=sum,na.rm=T)
cnam <- colnames(cpue_good[,c(4,7:18,22:26)])
colnames(tt) <- c("ECO_REG",cnam)
tt[,3:19] <- tt[,3:19]/tt$ocean_sqkm

# save data 
save(tt,file="cleaned data/final_2001_10biomass_ecoregion.RData")

# now get estimate per ecoregion and subdivision
cpue_good <- grid_master@data
cpue_good[,c(7:18,22:26)]  <- cpue_good[,c(7:18,22:26)] * cpue_good$ocean_sqkm # weighted with the size of the ocean area

tt <-  aggregate(list(cpue_good[,c(4,7:18,22:26)]),
                 by=list(cpue_good$subdivision),FUN=sum,na.rm=T)
cnam <- colnames(cpue_good[,c(4,7:18,22:26)])
colnames(tt) <- c("subdivision",cnam)
tt[,3:19] <- tt[,3:19]/tt$ocean_sqkm

# save data 
save(tt,file="cleaned data/final_2001_10biomass_subdivision.RData")
