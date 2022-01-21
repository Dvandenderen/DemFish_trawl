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
library(tidyverse)
library(nlme)

##################
load("cleaned data/surveyed_grid.RData") # get grid information
load("cleaned data/211216_depth_grid.RData") # get depth per grid cell and year
load("cleaned data/211216_biomass_grid.RData") # get biomass per grid cell and year
load("cleaned data/sstdat_1967_2018_COBE.RData") # get SST COBE

# run for all grid cells
filename <- cpue_good
grid <- grid_master
depthgr <- depth_grid
sstcobe <- sstdat
t_start <- 2010
t_end <- 2015

#  subset t_start : t_end
cpue_final <- subset(filename,filename$year >= t_start & filename$year <= t_end)
freq_dat <- as.data.frame(table(cpue_final$uni_cell))
freq_dat <- subset(freq_dat,freq_dat$Freq > 1)
cpue_final <- subset(cpue_final,cpue_final$uni_cell %in% freq_dat$Var1)

# add depth 
depthgr$uni <- paste(depthgr$uni_cell, depthgr$year)
cpue_final <- cbind(cpue_final,depthgr[match(cpue_final$uni,depthgr$uni),c("depth")])
colnames(cpue_final)[ncol(cpue_final)] <- "depth"

#  now estimate average biomass/ catch etc. per cell
cpue_final <-  aggregate(list(cpue_final$biomass,cpue_final$tlw,cpue_final$tl_95,
                              cpue_final$Catch_sqkm,cpue_final$Catch_pel_sqkm,cpue_final$depth),
                         by=list(cpue_final$uni_cell),FUN=mean,na.rm=T)
colnames(cpue_final) <- c("uni_cell","biomass","tlw","tl_95","Catch_sqkm","Catch_pel_sqkm","depth")

grid <- cbind(grid,cpue_final[match(grid@data$uni_cell,cpue_final$uni_cell),c(2:7)])
grid <- subset(grid,!(is.na(grid@data$biomass))) # remove all cells without information (not sampled)

# convert mapped depth to positive
grid@data$Depth_map <- abs(grid@data$Depth_map)

# add column to count grid cells per subdivision/ecoregion
grid@data$countgrid <- 1

# add SST based on COBE timeseries data
sstcobe <- data.frame(uni_cell = sstcobe$uni_cell, SST_time = rowMeans(sstcobe[,which(colnames(sstcobe) %in% t_start:t_end)]))
grid <- cbind(grid,sstcobe[match(grid@data$uni_cell,sstcobe$uni_cell),c(2)])
colnames(grid@data)[ncol(grid@data)] <- "SST_time"

library(nlme)

dat <- grid@data
dat$ER_log <- log10((dat$Catch_sqkm + 0.001)/dat$biomass)
dat <- subset(dat,!(is.na(dat$lz_prod)))
dat <- subset(dat,!(is.na(dat$ben_prod)))
dat <- subset(dat,!(is.na(dat$SST_time)))
dat <- subset(dat,dat$biomass < 200000)
dat.lm <- lm(biomass ~ SST_time +  ER_log + tlw + lz_prod, data=dat)
dat$Resid <- rstandard(dat.lm)

library(sp)
coordinates(dat) <- ~long + lat   #effectively convert the data into a spatial data frame
bubble(dat, "Resid")

data.spatialCor.gls <- gls(biomass ~ SST_time +  ER_log + tlw + lz_prod + ben_prod, data=dat,
                           method = "REML")

plot(nlme:::Variogram(data.spatialCor.gls, form = ~long +
                        lat, resType = "normalized"))

data.spatialCor.glsExp <- gls(biomass ~ SST_time +  ER_log + tlw + lz_prod + ben_prod, data = dat,
                              correlation = corExp(form = ~lat + long, nugget = TRUE),
                              method = "REML")
data.spatialCor.glsGaus <- gls(biomass ~ SST_time +  ER_log + tlw + lz_prod + ben_prod, data = dat,
                               correlation = corGaus(form = ~lat + long, nugget = TRUE),
                               method = "REML")
data.spatialCor.glsLin <- gls(biomass ~ SST_time +  ER_log + tlw + lz_prod + ben_prod, data = dat,
                              correlation = corLin(form = ~lat + long, nugget = TRUE),
                              method = "REML")
data.spatialCor.glsRatio <- gls(biomass ~ SST_time +  ER_log + tlw + lz_prod + ben_prod, data = dat,
                                correlation = corRatio(form = ~lat + long, nugget = TRUE),
                                method = "REML")
data.spatialCor.glsSpher <- gls(biomass ~ SST_time +  ER_log + tlw + lz_prod + ben_prod, data = dat,
                                correlation = corSpher(form = ~lat + long, nugget = TRUE),
                                method = "REML")

AIC(data.spatialCor.gls, data.spatialCor.glsExp, data.spatialCor.glsGaus,
    data.spatialCor.glsRatio,
    data.spatialCor.glsSpher)

plot(residuals(data.spatialCor.glsRatio , type = "normalized") ~
       fitted(data.spatialCor.glsRatio ))

plot(nlme:::Variogram(data.spatialCor.glsRatio, form = ~lat + long,
                      resType = "normalized"))

mo1 <- gls(biomass ~ SST_time +  ER_log + tlw + lz_prod, data = dat,
                                correlation = corRatio(form = ~lat + long, nugget = TRUE),
                                method = "REML")

mo2 <- gls(biomass ~ SST_time +  ER_log + tlw + ben_prod, data = dat,
                                correlation = corRatio(form = ~lat + long, nugget = TRUE),
                                method = "REML")

mo3 <- gls(biomass ~ SST_time +  ER_log + tlw + lz_prod, data = dat,
                              correlation = corExp(form = ~lat + long, nugget = TRUE),
                              method = "REML")

