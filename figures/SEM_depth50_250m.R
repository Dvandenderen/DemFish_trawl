

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
library(lavaan)

##################
load("cleaned data/surveyed_grid.RData") # get grid information
load("cleaned data/211216_depth_grid.RData") # get depth per grid cell and year
load("cleaned data/211216_biomass_grid.RData") # get biomass per grid cell and year
load("cleaned data/sstdat_1967_2018_COBE.RData") # get SST COBE

source("scripts - data analyses/source_get_bio_function.R") # source script to obtain biomass per ecoregion/subdiv

################
# create dataset by selecting depths 50 - 250 m - time period xxxx
depth_grid$uni <- paste(depth_grid$uni_cell,depth_grid$year)

cpue_depth <- cbind(cpue_good,depth_grid[match(cpue_good$uni,depth_grid$uni),c(3)])
colnames(cpue_depth)[ncol(cpue_depth)] <- "depth"
cpue_depth <- subset(cpue_depth,cpue_depth$depth > 50 & cpue_depth$depth < 250)

test <- subset(cpue_depth, cpue_depth$year %in% c(2001:2005))
test <- subset(cpue_depth,cpue_depth$depth > 50 & cpue_depth$depth < 250)
grid_new <- cbind(grid_master,test[match(grid_master@data$uni_cell,cpue_good$uni_cell),c(3,4)])
grid_new <- subset(grid_new,!(is.na(grid_new@data$biomass)))
plot(grid_new,add=T,col="blue")

# scale parameters
semparam <- c("NPP","lz_prod","ben_prod","biomass","tlw","tl_95","depth","SST_time","ER_log")

## hypothesized model 
modT2='biomass ~ tlw + ER_log + SST_time + ben_prod + lz_prod
       tlw ~ NPP + ER_log
       lz_prod ~ NPP +  SST_time 
       ben_prod ~ NPP + SST_time + depth+lz_prod
       NPP ~ SST_time'



cpue_good <- subset(cpue_good,cpue_good$uni %in% cpue_depth$uni)
datbio <- get_bio(cpue_good,t_start = 2000,t_end = 2005,spatialunit = "subdivision")  # spatialunit = "ECO_REG" or "subdivision"
datbio$ER_log <- log10(datbio$Catch_sqkm/datbio$biomass)
datbio[,semparam] <- scale(datbio[,semparam])
f.modT1.est <- sem(modT2,fixed.x=F,data=datbio)
summary(f.modT1.est,rsq=T,stand=T)
