
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
library(Metrics)

#-------------------------------------------------------------------------------
### model is based on 
# flux in == flux out  
# --> dat$ben_prod*TE^(dat$tlw-1) + q*dat$z_prod*TE^(dat$tlw-2.1) = C/B*B
#-------------------------------------------------------------------------------

# select data
  load("cleaned data/surveyed_grid.RData") # get grid information
  load("cleaned data/Depth_grid.RData") # get depth per grid cell and year
  load("cleaned data/Biomass_grid.RData") # get biomass per grid cell and year
  load("cleaned data/sstdat_1967_2018_COBE.RData") # get SST COBE
  source("scripts - data analyses/source_get_bio_function.R") # source script to obtain biomass per ecoregion/subdiv

  dat    <- get_bio(cpue_good,t_start = 1990,t_end = 2015,spatialunit = "ECO_REG")  # spatialunit = "ECO_REG" or "subdivision"
  dat    <- subset(dat,dat$countgrid >= 6)
  dat$ER <- dat$Catch_sqkm/dat$biomass

# get fraction demersal in catch
  dat$frac <- dat$Catch_sqkm/(dat$Catch_sqkm + dat$Catch_pel_sqkm)

# conversions
  dat$NPP <- dat$NPP * 9*365 # mg C / m**2 / day --> mg/m2/year == kg/km2/year
  dat$ben_prod <- 10*dat$ben_prod # get detritus flux
  dat$ben_prod <- dat$ben_prod *1000     # g / m**2 / year --> kg/km2/year
  dat$z_prod <-  dat$lz_prod*1000  + dat$mz_prod*1000


#-------------------------------------------------------------------------------
# first estimate the "best" temperature sensitivity of the transfer efficiency
  
  Q10 <- seq(.2,2.5,.01)
  Q10dat <- as.data.frame(matrix(data = NA,ncol=8,nrow=length(Q10)))
  Q10dat$Q10 <- Q10
  
  for (i in 1:length(Q10)){
    TE <- 0.05 * Q10[i] ^((dat$SST_time-10)/10)
    bio <- dat$ben_prod*TE^(dat$tlw-1) + dat$frac*dat$z_prod*TE^(dat$tlw-2.1) /dat$ER
    #plot(log10(bio),log10(dat$biomass))
    #abline(0,1)
    mod1 <- lm(log10(bio)~log10(dat$biomass))
  
    Q10dat[i,1] <- rmse(log10(dat$biomass),log10(bio))
    Q10dat[i,2] <- summary(mod1)$adj.r.squared
  }
  
  for (i in 1:length(Q10)){
    TE <- 0.075 * Q10[i] ^((dat$SST_time-10)/10)
    bio <- dat$ben_prod*TE^(dat$tlw-1) + dat$frac*dat$z_prod*TE^(dat$tlw-2.1) /dat$ER
    #plot(log10(bio),log10(dat$biomass))
    #abline(0,1)
    mod1 <- lm(log10(bio)~log10(dat$biomass))
    
    Q10dat[i,3] <- rmse(log10(dat$biomass),log10(bio))
    Q10dat[i,4] <- summary(mod1)$adj.r.squared
  }
  
  for (i in 1:length(Q10)){
    TE <- 0.1 * Q10[i] ^((dat$SST_time-10)/10)
    bio <- dat$ben_prod*TE^(dat$tlw-1) + dat$frac*dat$z_prod*TE^(dat$tlw-2.1) /dat$ER
    #plot(log10(bio),log10(dat$biomass))
    #abline(0,1)
    mod1 <- lm(log10(bio)~log10(dat$biomass))
    
    Q10dat[i,5] <- rmse(log10(dat$biomass),log10(bio))
    Q10dat[i,6] <- summary(mod1)$adj.r.squared
  }
  
  for (i in 1:length(Q10)){
    TE <- 0.15 * Q10[i] ^((dat$SST_time-10)/10)
    bio <- dat$ben_prod*TE^(dat$tlw-1) + dat$frac*dat$z_prod*TE^(dat$tlw-2.1) /dat$ER
    #plot(log10(bio),log10(dat$biomass))
    #abline(0,1)
    mod1 <- lm(log10(bio)~log10(dat$biomass))
    
    Q10dat[i,7] <- rmse(log10(dat$biomass),log10(bio))
    Q10dat[i,8] <- summary(mod1)$adj.r.squared
  }

  pdf("figures/Q10_tropho_model.pdf",width=6.9,height=3.56)  
  par(mfrow=c(1,2), mar=c(4, 4, 2, 1))
  plot(Q10dat$Q10,Q10dat$V2,type="l",lwd=2,ylab="R-square",ylim=c(0,0.7),yaxt="n", xlab="Q10")
  axis(2,c(0,0.35,0.7),las=1)
  lines(Q10dat$Q10,Q10dat$V4,lty=3,lwd=2,col="red")
  lines(Q10dat$Q10,Q10dat$V6,lty=5,lwd=2,col="blue")
  lines(Q10dat$Q10,Q10dat$V8,lty=4,lwd=2,col="grey")
  lines(x=c(0.55,0.55),y=c(-1,0.8),lty=5)
  legend("topright",legend=c("TE = 0.05","TE = 0.075", "TE = 0.10","TE = 0.15"),
         cex = 0.8,lty=c(1,3,5,4),col = c("black","red","blue","grey"),box.lty=0,lwd=c(2,2,2,2))
  plot(Q10dat$Q10,Q10dat$V1,type="l",lwd=2,ylab="RMSE",ylim=c(0.3,0.7),yaxt="n", xlab="Q10")
  lines(Q10dat$Q10,Q10dat$V3,lty=3,lwd=2,col="red")
  lines(Q10dat$Q10,Q10dat$V5,lty=5,lwd=2,col="blue")
  lines(Q10dat$Q10,Q10dat$V7,lty=4,lwd=2,col="grey")
  axis(2,c(0.3,0.5,0.7),las=1)
  lines(x=c(0.55,0.55),y=c(0,0.8),lty=5)
  dev.off()

#-------------------------------------------------------------------------------
# now make the prediction for the main manuscript
  
  TE <- 0.075 * 0.55 ^((dat$SST_time-10)/10)
  bio <- dat$ben_prod*TE^(dat$tlw-1) + dat$frac*dat$z_prod*TE^(dat$tlw-2.1) /dat$ER
  bio <- bio/1000 #kg per km2 to tonnes per km2
  biomass_obs <- dat$biomass/1000 #kg per km2 to tonnes per km2

  mod1 <- lm(log10(bio)~log10(biomass_obs))
  x <- c(-0.25,2.5)
  y <- mod1$coefficients[1] + mod1$coefficients[2]*x
  pdf("figures/Predicted_versus_observed.pdf",width=8,height=4.5) 
  par(mfrow=c(1,2), mar=c(4, 4, 2, 1))
  plot(log10(bio),log10(biomass_obs),xlab="Predicted biomass",main="Tropho. dynamic eq.",
       ylab="Observed biomass",xaxt="n",yaxt="n",xlim=c(-0.25,2.7),ylim=c(-0.25,2.7),pch=16)
  lines(y,x,lty=1)
  axis(1,c(0,1,2),c("1","10","100"))
  axis(2,c(0,1,2),c("1","10","100"),las=1)
  abline(0,1,lty=5)
  text(2,0,"RMSE = 0.38 \n R2 = 0.66")
  text(0,2.6,"")
  
  dat$LER <- log10(dat$ER)
  dat$biomass <- dat$biomass/1000
  mod1 <- lm(dat$biomass~ dat$LER + dat$SST_time + dat$tlw + dat$z_prod)
  dat$stat_pred <- predict(mod1,newdata= dat)
  dat <- dat[-6,]
  biorep <- biomass_obs[-6]
  mod1 <- lm(log10(dat$stat_pred)~log10(biorep))
  plot(log10(dat$stat_pred),log10(biorep),xlab="Predicted biomass",main="SEM prediction",
       ylab="",xaxt="n",yaxt="n",xlim=c(-0.25,2.7),ylim=c(-0.25,2.7),pch=16)
  x <- c(-1,4)
  y <- mod1$coefficients[1] + mod1$coefficients[2]*x
  lines(y,x,lty=5)
  axis(1,c(0,1,2),c("1","10","100"))
  axis(2,c(0,1,2),c("","",""),las=1)
  abline(0,1)
  points(-0.25,log10(biomass_obs[6]),pch=8)
  text(2,0,"RMSE = 0.29 \n R2 = 0.47")
  text(0,2.6,"b)")
  dev.off()

#-------------------------------------------------------------------------------
# get sensitivity values for the result section
  TE <- 0.075 * 0.55 ^((dat$SST_time-10)/10)
  TE <- mean(TE)
  bio <- dat$ben_prod*TE^(dat$tlw-1) + dat$frac*dat$z_prod*TE^(dat$tlw-2.1) /dat$ER
  mod1 <- lm(log10(bio)~log10(dat$biomass))
  summary(mod1)$adj.r.squared
  
  TE <- 0.075 * 0.55 ^((dat$SST_time-10)/10)
  bio <- dat$ben_prod*TE^(dat$tlw-1) + dat$frac*dat$z_prod*TE^(dat$tlw-2.1) / mean(dat$ER)
  mod1 <- lm(log10(bio)~log10(dat$biomass))
  summary(mod1)$adj.r.squared
  
  TE <- 0.075 * 0.55 ^((dat$SST_time-10)/10)
  bio <- dat$ben_prod*TE^(dat$tlw-1) + dat$frac*mean(dat$z_prod)*TE^(dat$tlw-2.1) / dat$ER
  mod1 <- lm(log10(bio)~log10(dat$biomass))
  summary(mod1)$adj.r.squared
  
#-------------------------------------------------------------------------------
# get prediction for the subdivisions - supplement
  dat    <- get_bio(cpue_good,t_start = 1990,t_end = 2015,spatialunit = "subdivision")  # spatialunit = "ECO_REG" or "subdivision"
  dat    <- subset(dat,dat$countgrid >= 6)
  dat$ER <- dat$Catch_sqkm/dat$biomass
  
  # get fraction demersal in catch
  dat$frac <- dat$Catch_sqkm/(dat$Catch_sqkm + dat$Catch_pel_sqkm)
  
  # conversions
  dat$NPP <- dat$NPP * 9*365 # mg C / m**2 / day --> mg/m2/year == kg/km2/year
  dat$ben_prod <- 10*dat$ben_prod # get detritus flux
  dat$ben_prod <- dat$ben_prod *1000     # g / m**2 / year --> kg/km2/year
  dat$z_prod <-  dat$lz_prod*1000  + dat$mz_prod*1000
  
  TE <- 0.075 * 0.55 ^((dat$SST_time-10)/10)
  bio <- dat$ben_prod*TE^(dat$tlw-1) + dat$frac*dat$z_prod*TE^(dat$tlw-2.1) /dat$ER
  bio <- bio/1000 #kg per km2 to tonnes per km2
  biomass_obs <- dat$biomass/1000 #kg per km2 to tonnes per km2
  
  mod1 <- lm(log10(bio)~log10(biomass_obs))
  x <- c(-0.25,2.5)
  y <- mod1$coefficients[1] + mod1$coefficients[2]*x
  pdf("figures/Predicted_versus_observed_subdivision.pdf",width=8,height=4.5) 
  par(mfrow=c(1,2), mar=c(4, 4, 2, 1))
  plot(log10(bio),log10(biomass_obs),xlab="Predicted biomass",main="Tropho. dynamic eq.",
       ylab="Observed biomass",xaxt="n",yaxt="n",xlim=c(-0.25,2.7),ylim=c(-0.25,2.7),pch=16)
  lines(y,x,lty=1)
  axis(1,c(0,1,2),c("1","10","100"))
  axis(2,c(0,1,2),c("1","10","100"),las=1)
  abline(0,1,lty=5)
  text(2,0,"RMSE = 0.39 \n R2 = 0.65")
  text(0,2.6)
  
  dat$LER <- log10(dat$ER)
  dat$biomass <- dat$biomass/1000
  mod1 <- lm(dat$biomass~ dat$LER + dat$SST_time + dat$tlw + dat$z_prod)
  dat$stat_pred <- predict(mod1,newdata= dat)
  dat <- dat[-c(20,24),]
  biorep <- biomass_obs[-c(20,24)]
  mod1 <- lm(log10(dat$stat_pred)~log10(biorep))
  plot(log10(dat$stat_pred),log10(biorep),xlab="Predicted biomass",main="SEM prediction",
       ylab="",xaxt="n",yaxt="n",xlim=c(-0.25,2.7),ylim=c(-0.25,2.7),pch=16)
  x <- c(-1,4)
  y <- mod1$coefficients[1] + mod1$coefficients[2]*x
  lines(y,x,lty=1)
  axis(1,c(0,1,2),c("1","10","100"))
  axis(2,c(0,1,2),c("","",""),las=1)
  abline(0,1,lty=5)
  points(c(-0.25,-0.25),log10(biomass_obs[c(20,24)]),pch=8)
  text(2,0,"RMSE = 0.30 \n R2 = 0.42")
  text(0,2.6,"b)")
  dev.off()
  
  