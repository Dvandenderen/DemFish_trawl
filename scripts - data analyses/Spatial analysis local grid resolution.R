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
  
  predictions <- predict(mwrm, newdata = dat1015) 
  dat1015$resids <- predictions
  
  
# convert mapped depth to positive
grid@data$Depth_map <- abs(grid@data$Depth_map)

# add column to count grid cells per subdivision/ecoregion
grid@data$countgrid <- 1


library(nlme)

dat <- grid@data
dat$ER_log <- log10((dat$Catch_sqkm + 0.001)/dat$biomass)
dat <- subset(dat,!(is.na(dat$lz_prod)))
dat <- subset(dat,!(is.na(dat$ben_prod)))
dat <- subset(dat,!(is.na(dat$SST_time)))
dat <- subset(dat,!(is.na(dat$tlw)))
#dat <- subset(dat,dat$biomass < 200000)
dat$biomass <- log10(dat$biomass/1000)
dat$ben_prod <- 10*dat$ben_prod  # get detritus flux
dat$ben_prod <- dat$ben_prod      # g / m**2 / year --> tonnes/km2/year
dat$lz_prod <-  dat$lz_prod

datll <- dat
coordinates(datll)<- ~long+lat
proj4string(datll) = CRS("+init=epsg:4326")
datUTM <- spTransform(datll, CRS("+init=epsg:32748"))
datll <- coordinates(datUTM)
dat$UTMlong <- datll[,1]
dat$UTMlat <- datll[,2]

# Moran's I correlation coefficient
library(ape)
dat.dists <- as.matrix(dist(cbind(dat$long, dat$lat)))
dat.dists.inv <- 1/dat.dists
diag(dat.dists.inv) <- 0
dat.dists.inv[1:5, 1:5]
Moran.I(dat$biomass, dat.dists.inv)

### wavelets autocorrelation 
coords <- dat[ ,19:20]
coords <- round(coords,digits =0)

library(spind)
mwrm <- WRM(biomass ~ SST_time +  ER_log  + tlw + lz_prod + ben_prod, family = "gaussian", 
            data = dat, coord = coords, level = 1, plot = TRUE)

summary(mwrm)
predictions <- predict(mwrm, newdata = dat) 
dat$resids <- dat$biomass-predictions

### fit without autocorrelatio structure
dat$reg <- ifelse(dat$long < -110,"pac","atl")
dat$reg <- ifelse(dat$long >-10,"at2",dat$reg)
data.spatialCor.gls <- gls(biomass ~ SST_time +  ER_log + tlw + lz_prod + ben_prod + reg, data=dat,
                           method = "REML")

## make bubble plot
datbub <-data.frame(dat$long,dat$lat,dat$uni_cell,resids=resid(data.spatialCor.gls))
datbub$resids <- ifelse(datbub$resids > .6, .6,datbub$resids)
datbub$resids <- ifelse(datbub$resids < -.6, -.6,datbub$resids)

grid_master <- grid_master[,-22]
grid_master <- cbind(grid_master,datbub[match(grid_master@data$uni_cell,datbub$dat.uni_cell),c(4)])
colnames(grid_master@data)[ncol(grid_master@data)] <- "resids"
ncoords <- subset(grid_master,!(is.na(grid_master@data$resids)))

ctrys <- rnaturalearth::ne_countries(scale = 50, returnclass = "sf")
minlong <- -180 #round(min(filedata$long)-1)
maxlong <- 51
minlat  <- 23
maxlat  <- 83
coordslim <- c(minlong,maxlong,minlat,maxlat)
coordxmap <- round(seq(minlong,maxlong,length.out = 4))
coordymap <- round(seq(minlat,maxlat,length.out = 4))

nco <- sf::st_as_sf(ncoords)

ggplot(nco) + 
  geom_sf( aes(fill=resids), colour = NA ) +
  scale_fill_gradient2(low = "#fc9272",mid="#ffffd4",high= "#3182bd",
                        midpoint = 0,limits=c(-.6,.6),"Residuals") +  
  geom_sf(data = ctrys, fill="grey",colour=NA) +
      theme(plot.background=element_blank(),
           panel.background=element_blank(),
           axis.text.y   = element_text(size=11),
           axis.text.x   = element_text(size=11),
           axis.title.y  = element_text(size=11),
           axis.title.x  = element_text(size=11),
           panel.border  = element_rect(colour = "grey", size=.5,fill=NA),
           legend.text   = element_text(size=11),
           legend.title  = element_text(size=11))+
  coord_sf(xlim = c(-180, 51), ylim = c(20, 84), expand = FALSE) +
  scale_x_continuous(breaks=coordxmap)  +
  scale_y_continuous(breaks=coordymap)

# plot variogram without 
plot(nlme:::Variogram(data.spatialCor.gls, form = ~UTMlong +  UTMlat, resType = "normalized"))

### select the spatial autocorrelation structure
data.spatialCor.glsExp <- gls(biomass ~ SST_time +  ER_log + tlw + lz_prod + ben_prod + reg, data = dat,
                              correlation = corExp(form = ~UTMlat + UTMlong, nugget = TRUE),
                              method = "REML")
data.spatialCor.glsGaus <- gls(biomass ~ SST_time +  ER_log + tlw + lz_prod + ben_prod, data = dat,
                               correlation = corGaus(form = ~UTMlat + UTMlong, nugget = TRUE),
                               method = "REML")
data.spatialCor.glsLin <- gls(biomass ~ SST_time +  ER_log + tlw + lz_prod + ben_prod, data = dat,
                              correlation = corLin(form = ~UTMlat + UTMlong, nugget = TRUE),
                              method = "REML")
data.spatialCor.glsRatio <- gls(biomass ~ SST_time +  ER_log + tlw + lz_prod + ben_prod, data = dat,
                                correlation = corRatio(form = ~UTMlat + UTMlong, nugget = TRUE),
                                method = "REML")
data.spatialCor.glsSpher <- gls(biomass ~ SST_time +  ER_log + tlw + lz_prod + ben_prod, data = dat,
                                correlation = corSpher(form = ~UTMlat + UTMlong, nugget = TRUE),
                                method = "REML")

AIC(data.spatialCor.gls, data.spatialCor.glsExp, data.spatialCor.glsGaus,
    data.spatialCor.glsRatio,
    data.spatialCor.glsSpher)

#plot(residuals(data.spatialCor.glsExp , type = "normalized") ~ fitted(data.spatialCor.glsExp ))
plot(nlme:::Variogram(data.spatialCor.glsExp, form = ~UTMlat + UTMlong,  
                      resType = "normalized"))


# the correlation between two observations a distance r apart is
# (1 − nugget)*exp(−r/range)

#####################################                    
##### now estimate per period #######
#####################################

#### 1990-1995
filename <- cpue_good
grid <- grid_master
depthgr <- depth_grid
sstcobe <- sstdat
t_start <- 1990
t_end <- 1995

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
dat <- subset(dat,!(is.na(dat$tlw)))
#dat <- subset(dat,dat$biomass < 200000)
dat$biomass <- log10(dat$biomass/1000)
dat$ben_prod <- 10*dat$ben_prod  # get detritus flux
dat$ben_prod <- dat$ben_prod      # g / m**2 / year --> tonnes/km2/year
dat$lz_prod <-  dat$lz_prod

datll <- dat
coordinates(datll)<- ~long+lat
proj4string(datll) = CRS("+init=epsg:4326")
datUTM <- spTransform(datll, CRS("+init=epsg:32748"))
datll <- coordinates(datUTM)
dat$UTMlong <- datll[,1]
dat$UTMlat <- datll[,2]

mo1 <- gls(biomass ~ SST_time  +  ER_log + tlw + lz_prod + ben_prod, data = dat,
           correlation = corExp(form = ~UTMlat + UTMlong, nugget = TRUE),
           method = "ML")

mo2 <- gls(biomass ~ ER_log + tlw + ben_prod, data = dat,
           correlation = corExp(form = ~UTMlat + UTMlong, nugget = TRUE),
           method = "ML")

mo3 <- gls(biomass ~ ER_log +  ben_prod, data = dat,
           correlation = corExp(form = ~UTMlat + UTMlong, nugget = TRUE),
           method = "ML")

anova(mo1,mo2)
anova(mo2,mo3)

mo3 <- gls(biomass ~ ER_log + tlw + ben_prod , data = dat,
           correlation = corExp(form = ~UTMlat + UTMlong, nugget = TRUE),
           method = "REML")
summary(mo3)

#### 2000-2005
filename <- cpue_good
grid <- grid_master
depthgr <- depth_grid
sstcobe <- sstdat
t_start <- 2000
t_end <- 2005

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
dat <- subset(dat,!(is.na(dat$tlw)))
#dat <- subset(dat,dat$biomass < 200000)
dat$biomass <- log10(dat$biomass/1000)
dat$ben_prod <- 10*dat$ben_prod  # get detritus flux
dat$ben_prod <- dat$ben_prod      # g / m**2 / year --> tonnes/km2/year
dat$lz_prod <-  dat$lz_prod

mo1 <- gls(biomass ~ SST_time  +  ER_log + tlw + lz_prod + ben_prod, data = dat,
           correlation = corExp(form = ~lat + long, nugget = TRUE),
           method = "ML")

mo2 <- gls(biomass ~ ER_log + SST_time + ben_prod, data = dat,
           correlation = corExp(form = ~lat + long, nugget = TRUE),
           method = "ML")

mo3 <- gls(biomass ~ ER_log +  ben_prod , data = dat,
           correlation = corExp(form = ~lat + long, nugget = TRUE),
           method = "ML")

anova(mo1,mo2)
anova(mo2,mo3)

mo3 <- gls(biomass ~ ER_log +  ben_prod  , data = dat,
           correlation = corExp(form = ~lat + long, nugget = TRUE),
           method = "REML")
summary(mo3)

#### 2010-2015
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
dat <- subset(dat,!(is.na(dat$tlw)))
#dat <- subset(dat,dat$biomass < 200000)
dat$biomass <- log10(dat$biomass/1000)
dat$ben_prod <- 10*dat$ben_prod  # get detritus flux
dat$ben_prod <- dat$ben_prod      # g / m**2 / year --> tonnes/km2/year
dat$lz_prod <-  dat$lz_prod

mo1 <- gls(biomass ~ SST_time  +  ER_log + tlw + lz_prod + ben_prod, data = dat,
           correlation = corExp(form = ~lat + long, nugget = TRUE),
           method = "ML")

mo2 <- gls(biomass ~ ER_log  +  ben_prod, data = dat,
           correlation = corExp(form = ~lat + long, nugget = TRUE),
           method = "ML")

mo3 <- gls(biomass ~ ER_log , data = dat,
           correlation = corExp(form = ~lat + long, nugget = TRUE),
           method = "ML")

anova(mo1,mo2)
anova(mo2,mo3)

mo3 <- gls(biomass ~ ER_log  , data = dat,
           correlation = corExp(form = ~lat + long, nugget = TRUE),
           method = "REML")
summary(mo3)

