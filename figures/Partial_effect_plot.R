
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
load("processed data/surveyed_grid.RData") # get grid information
load("processed data/Depth_grid.RData") # get depth per grid cell and year
load("processed data/Biomass_grid.RData") # get biomass per grid cell and year
load("processed data/sstdat_1967_2018_COBE.RData") # get SST COBE
source("scripts - data analyses/source_get_bio_function.R") # source script to obtain biomass per ecoregion/subdiv

dat <- get_bio(cpue_good,t_start = 2000,t_end = 2005,spatialunit = "subdivision")  # spatialunit = "ECO_REG" or "subdivision"
dat$LER <- log10(dat$Catch_sqkm/dat$biomass)
dat$biomass <- dat$biomass/1000

################
# partial effect for each spatial scale and time period

dat1 <- get_bio(cpue_good,t_start = 1990,t_end = 1995,spatialunit = "subdivision")  # spatialunit = "ECO_REG" or "subdivision"
dat1$LER <- log10(dat1$Catch_sqkm/dat1$biomass)
dat1$biomass <- dat1$biomass/1000
dat1$z_prod  <- dat1$lz_prod + dat1$mz_prod 

dat2 <- get_bio(cpue_good,t_start = 2000,t_end = 2005,spatialunit = "subdivision")  # spatialunit = "ECO_REG" or "subdivision"
dat2$LER <- log10(dat2$Catch_sqkm/dat2$biomass)
dat2$biomass <- dat2$biomass/1000
dat2$z_prod  <- dat2$lz_prod + dat2$mz_prod 

dat3 <- get_bio(cpue_good,t_start = 2010,t_end = 2015,spatialunit = "subdivision")  # spatialunit = "ECO_REG" or "subdivision"
dat3$LER <- log10(dat3$Catch_sqkm/dat3$biomass)
dat3$biomass <- dat3$biomass/1000
dat3$z_prod  <- dat3$lz_prod + dat3$mz_prod 

dat4 <- get_bio(cpue_good,t_start = 1990,t_end = 1995,spatialunit = "ECO_REG")  # spatialunit = "ECO_REG" or "subdivision"
dat4$LER <- log10(dat4$Catch_sqkm/dat4$biomass)
dat4$biomass <- dat4$biomass/1000
dat4$z_prod  <- dat4$lz_prod + dat4$mz_prod 

dat5 <- get_bio(cpue_good,t_start = 2000,t_end = 2005,spatialunit = "ECO_REG")  # spatialunit = "ECO_REG" or "subdivision"
dat5$LER <- log10(dat5$Catch_sqkm/dat5$biomass)
dat5$biomass <- dat5$biomass/1000
dat5$z_prod  <- dat5$lz_prod + dat5$mz_prod 

dat6 <- get_bio(cpue_good,t_start = 2010,t_end = 2015,spatialunit = "ECO_REG")  # spatialunit = "ECO_REG" or "subdivision"
dat6$LER <- log10(dat6$Catch_sqkm/dat6$biomass)
dat6$biomass <- dat6$biomass/1000
dat6$z_prod  <- dat6$lz_prod + dat6$mz_prod 

source("scripts - data analyses/Source_partial_effect_lm.R") 

pdf("figures/Partial_effect.pdf",width=7.5,height=2.8)
par(mfrow=c(1,4), mar=c(6, 4, 2, 1))

mod1 <- lm(biomass~ LER + SST_time + tlw + z_prod, data=dat1)
mod2 <- lm(biomass~ LER + SST_time + tlw + z_prod, data=dat2)
mod3 <- lm(biomass~ LER + SST_time + tlw + z_prod, data=dat3)
mod4 <- lm(biomass~ LER + SST_time + tlw + z_prod, data=dat4)
mod5 <- lm(biomass~ LER + SST_time + tlw + z_prod, data=dat5)
mod6 <- lm(biomass~ LER + SST_time + tlw + z_prod, data=dat6)

effect(mod1,"SST_time",0.95,x.label="Temp.",rug =F,y.label = "Partial biomass effect",  line.color='blue',ci.color = "white")
effect2(mod2,"SST_time",0.95,x.label="",rug =F,y.label = "", line.color='blue',line.type = 3)
effect2(mod3,"SST_time",0.95,x.label="",rug =F,y.label = "", line.color='blue',line.type = 5)
effect2(mod4,"SST_time",0.95,x.label="",rug =F,y.label = "", line.color='red')
effect2(mod5,"SST_time",0.95,x.label="",rug =F,y.label = "", line.color='red',line.type = 3)
effect2(mod6,"SST_time",0.95,x.label="",rug =F,y.label = "", line.color='red',line.type = 5)

effect(mod1,"LER",0.95,x.label="Fishing exploit. rate",rug =F,y.label = "",  line.color='blue',ci.color = "white")
effect2(mod2,"LER",0.95,x.label="",rug =F,y.label = "", line.color='blue',line.type = 3)
effect2(mod3,"LER",0.95,x.label="",rug =F,y.label = "", line.color='blue',line.type = 5)
effect2(mod4,"LER",0.95,x.label="",rug =F,y.label = "", line.color='red')
effect2(mod5,"LER",0.95,x.label="",rug =F,y.label = "", line.color='red',line.type = 3)
effect2(mod6,"LER",0.95,x.label="",rug =F,y.label = "", line.color='red',line.type = 5)

effect(mod1,"tlw",0.95,x.label="Mean trophic level",rug =F,y.label = "",  line.color='blue',ci.color = "white")
effect2(mod2,"tlw",0.95,x.label="",rug =F,y.label = "", line.color='blue',line.type = 3)
effect2(mod3,"tlw",0.95,x.label="",rug =F,y.label = "", line.color='blue',line.type = 5)
effect2(mod4,"tlw",0.95,x.label="",rug =F,y.label = "", line.color='red')
effect2(mod5,"tlw",0.95,x.label="",rug =F,y.label = "", line.color='red',line.type = 3)
effect2(mod6,"tlw",0.95,x.label="",rug =F,y.label = "", line.color='red',line.type = 5)

effect(mod1,"z_prod",0.95,x.label="Zoop. prod.",rug =F,y.label = "",  line.color='blue',ci.color = "white")
effect2(mod2,"z_prod",0.95,x.label="",rug =F,y.label = "", line.color='blue',line.type = 3)
effect2(mod3,"z_prod",0.95,x.label="",rug =F,y.label = "", line.color='blue',line.type = 5)
effect2(mod4,"z_prod",0.95,x.label="",rug =F,y.label = "", line.color='red')
effect2(mod5,"z_prod",0.95,x.label="",rug =F,y.label = "", line.color='red',line.type = 3)
effect2(mod6,"z_prod",0.95,x.label="",rug =F,y.label = "", line.color='red',line.type = 5)

legend(50,10,legend=c("1990-95","2000-05","2010-15"), bty = "n",
       col=c("black", "black", "black"), lty=c(1,3,5), cex=0.8, x.intersp=2)

legend(50,-20,legend=c("Subdivision","Ecoregion"), bty = "n",
       col=c("blue", "red"), lty=c(1,1), cex=0.8, x.intersp=2)

dev.off()



