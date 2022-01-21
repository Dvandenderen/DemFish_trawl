
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

### model is based on 
# flux in == flux out  
# --> dat$ben_prod*TE^(dat$tlw-1) + dat$lz_prod*TE^(dat$tlw-2.1) = C/B*B

##################
load("cleaned data/surveyed_grid.RData") # get grid information
load("cleaned data/211216_depth_grid.RData") # get depth per grid cell and year
load("cleaned data/211216_biomass_grid.RData") # get biomass per grid cell and year
load("cleaned data/sstdat_1967_2018_COBE.RData") # get SST COBE

source("scripts - data analyses/source_get_bio_function.R") # source script to obtain biomass per ecoregion/subdiv

# select data
dat <- get_bio(cpue_good,t_start = 1990,t_end = 2015,spatialunit = "ECO_REG")  # spatialunit = "ECO_REG" or "subdivision"
dat <- subset(dat,dat$countgrid >= 6)
dat$ER <- dat$Catch_sqkm/dat$biomass

# get fraction demersal in catch versus pelagic
dat$frac <- dat$Catch_sqkm/(dat$Catch_sqkm + dat$Catch_pel_sqkm)

# conversions
dat$NPP <- dat$NPP * 9*365 # mg C / m**2 / day --> mg/m2/year == kg/km2/year
dat$ben_prod <- 10*dat$ben_prod # get detritus flux
dat$ben_prod <- dat$ben_prod *1000     # g / m**2 / year --> kg/km2/year
dat$lz_prod <-  dat$lz_prod*1000

##### 
# first estimate the "best" temperature sensitivity of the transfer efficiency
Q10 <- seq(.2,1.8,.01)
Q10dat <- as.data.frame(matrix(data = NA,ncol=2,nrow=length(Q10)))
Q10dat$Q10 <- Q10

for (i in 1:length(Q10)){
  TE <- 0.14 * Q10[i] ^((dat$SST_time-10)/10)
  bio <- dat$ben_prod*TE^(dat$tlw-1) + dat$frac*dat$lz_prod*TE^(dat$tlw-2.1) /dat$ER
  # plot(log10(bio),log10(dat$biomass))
  # abline(0,1)
  mod1 <- lm(log10(bio)~log10(dat$biomass))

  Q10dat[i,1] <- rmse(log10(dat$biomass),log10(bio))
  Q10dat[i,2] <- summary(mod1)$adj.r.squared
}

pdf("figures/Q10_tropho_model.pdf",width=6.9,height=3.56)  
par(mfrow=c(1,2), mar=c(4, 4, 2, 1))
plot(Q10dat$Q10,Q10dat$V2,type="l",lwd=2,ylab="R-square",ylim=c(0,0.7),yaxt="n", xlab="Q10")
axis(2,c(0,0.35,0.7),las=1)
lines(x=c(0.55,0.55),y=c(-1,0.8),lty=5)
plot(Q10dat$Q10,Q10dat$V1,type="l",lwd=2,ylab="RMSE",ylim=c(0.3,0.7),yaxt="n", xlab="Q10")
axis(2,c(0.3,0.5,0.7),las=1)
lines(x=c(0.55,0.55),y=c(0,0.8),lty=5)
dev.off()

#### 
# now make the prediction for the main manuscript

TE <- 0.1 * 0.55 ^((dat$SST_time-10)/10)
bio <- dat$ben_prod*TE^(dat$tlw-1) + dat$frac*dat$lz_prod*TE^(dat$tlw-2.1) /dat$ER
bio <- bio/1000 #kg per km2 to tonnes per km2
biomass_obs <- dat$biomass/1000 #kg per km2 to tonnes per km2

mod1 <- lm(log10(bio)~log10(biomass_obs))
x <- c(-0.25,2.5)
y <- mod1$coefficients[1] + mod1$coefficients[2]*x
pdf("figures/Predicted_versus_observed.pdf",width=4.5,height=4.5) 
plot(log10(bio),log10(biomass_obs),xlab="Predicted biomass"
     ,ylab="Observed biomass",xaxt="n",yaxt="n",xlim=c(-0.25,2.5),ylim=c(-0.25,2.5),pch=16)
lines(y,x,lty=5)
axis(1,c(0,1,2),c("1","10","100"))
axis(2,c(0,1,2),c("1","10","100"),las=1)
abline(0,1)

text(2,0,"RMSE = 0.42 \n R2 = 0.64")

dev.off()

# check sensitivity
# take average of each individual parameter and check how it changes the rmse/R2
Sens <- as.data.frame(matrix(data=NA,nrow=7,ncol=5))

datsens <- dat

Q10t <- 0.55

# get best model
TE <- 0.1 * Q10t ^((datsens$SST_time-10)/10)
bio <- datsens$ben_prod*TE^(datsens$tlw-1) + datsens$frac*datsens$lz_prod*TE^(datsens$tlw-2.1) /datsens$ER
bio <- bio/1000 #kg per km2 to tonnes per km2
datsens$biomass <- datsens$biomass/1000 #kg per km2 to tonnes per km2
plot(log10(datsens$biomass)~log10(bio))
mod1 <- lm(log10(bio)~log10(datsens$biomass))
Sens[1,1] <- rmse(log10(datsens$biomass),log10(bio))
Sens[1,2] <- summary(mod1)$adj.r.squared
Sens[1,3] <- "model"
Sens[1,4] <- 0
Sens[1,5] <- 1
biomod <- bio

# check mean of zoop flux
datsens <- dat
TE <- 0.1 * Q10t ^((datsens$SST_time-10)/10)
bio <- (datsens$ben_prod)*TE^(datsens$tlw-1) + datsens$frac*mean(datsens$lz_prod)*TE^(datsens$tlw-2.1) /datsens$ER
bio <- bio/1000 #kg per km2 to tonnes per km2
datsens$biomass <- datsens$biomass/1000 #kg per km2 to tonnes per km2
plot(log10(datsens$biomass)~log10(bio))
mod1 <- lm(log10(datsens$biomass)~log10(bio))
Sens[2,1] <- rmse(log10(datsens$biomass),log10(bio))
Sens[2,2] <- summary(mod1)$adj.r.squared
Sens[2,3] <- "zoop prod"
mod1 <- lm(log10(bio)~log10(biomod))
Sens[2,4] <- rmse(log10(biomod),log10(bio))
Sens[2,5] <- summary(mod1)$adj.r.squared

# check mean of benth flux
datsens <- dat
TE <- 0.1 * Q10t ^((datsens$SST_time-10)/10)
bio <- mean(datsens$ben_prod)*TE^(datsens$tlw-1) + datsens$frac*(datsens$lz_prod)*TE^(datsens$tlw-2.1) /datsens$ER
bio <- bio/1000 #kg per km2 to tonnes per km2
datsens$biomass <- datsens$biomass/1000 #kg per km2 to tonnes per km2
plot(log10(datsens$biomass)~log10(bio))
mod1 <- lm(log10(datsens$biomass)~log10(bio))
Sens[3,1] <- rmse(log10(datsens$biomass),log10(bio))
Sens[3,2] <- summary(mod1)$adj.r.squared
Sens[3,3] <- "benth flux"
mod1 <- lm(log10(bio)~log10(biomod))
Sens[3,4] <- rmse(log10(biomod),log10(bio))
Sens[3,5] <- summary(mod1)$adj.r.squared

# check mean of tlw
datsens <- dat
TE <- 0.1 * Q10t ^((datsens$SST_time-10)/10)
bio <- datsens$ben_prod*TE^(mean(datsens$tlw)-1) + datsens$frac*datsens$lz_prod*TE^(mean(datsens$tlw)-2.1) /datsens$ER
bio <- bio/1000 #kg per km2 to tonnes per km2
datsens$biomass <- datsens$biomass/1000 #kg per km2 to tonnes per km2
plot(log10(datsens$biomass)~log10(bio))
mod1 <- lm(log10(datsens$biomass)~log10(bio))
Sens[4,1] <- rmse(log10(datsens$biomass),log10(bio))
Sens[4,2] <- summary(mod1)$adj.r.squared
Sens[4,3] <- "tlw"
mod1 <- lm(log10(bio)~log10(biomod))
Sens[4,4] <- rmse(log10(biomod),log10(bio))
Sens[4,5] <- summary(mod1)$adj.r.squared

# check mean of pel/dem fraction
datsens <- dat
TE <- 0.1 * Q10t ^((datsens$SST_time-10)/10)
bio <- datsens$ben_prod*TE^(datsens$tlw-1) + mean(datsens$frac)*datsens$lz_prod*TE^(datsens$tlw-2.1) /datsens$ER
bio <- bio/1000 #kg per km2 to tonnes per km2
datsens$biomass <- datsens$biomass/1000 #kg per km2 to tonnes per km2
plot(log10(datsens$biomass)~log10(bio))
mod1 <- lm(log10(datsens$biomass)~log10(bio))
Sens[5,1] <- rmse(log10(datsens$biomass),log10(bio))
Sens[5,2] <- summary(mod1)$adj.r.squared
Sens[5,3] <- "peldem_frac"
mod1 <- lm(log10(bio)~log10(biomod))
Sens[5,4] <- rmse(log10(biomod),log10(bio))
Sens[5,5] <- summary(mod1)$adj.r.squared

# check mean of temperature
datsens <- dat
TE <- 0.1 * Q10t ^((mean(datsens$SST_time)-10)/10)
bio <- datsens$ben_prod*TE^(datsens$tlw-1) + datsens$frac*datsens$lz_prod*TE^(datsens$tlw-2.1) /datsens$ER
bio <- bio/1000 #kg per km2 to tonnes per km2
datsens$biomass <- datsens$biomass/1000 #kg per km2 to tonnes per km2
plot(log10(datsens$biomass)~log10(bio))
mod1 <- lm(log10(datsens$biomass)~log10(bio))
Sens[6,1] <- rmse(log10(datsens$biomass),log10(bio))
Sens[6,2] <- summary(mod1)$adj.r.squared
Sens[6,3] <- "temp"
mod1 <- lm(log10(bio)~log10(biomod))
Sens[6,4] <- rmse(log10(biomod),log10(bio))
Sens[6,5] <- summary(mod1)$adj.r.squared

# check mean of ER
datsens <- dat
TE <- 0.1 * Q10t ^((datsens$SST_time-10)/10)
bio <- datsens$ben_prod*TE^(datsens$tlw-1) + datsens$frac*datsens$lz_prod*TE^(datsens$tlw-2.1) /mean(datsens$ER)
bio <- bio/1000 #kg per km2 to tonnes per km2
datsens$biomass <- datsens$biomass/1000 #kg per km2 to tonnes per km2
plot(log10(datsens$biomass)~log10(bio))
mod1 <- lm(log10(datsens$biomass)~log10(bio))
Sens[7,1] <- rmse(log10(datsens$biomass),log10(bio))
Sens[7,2] <- summary(mod1)$adj.r.squared
Sens[7,3] <- "ER"
mod1 <- lm(log10(bio)~log10(biomod))
Sens[7,4] <- rmse(log10(biomod),log10(bio))
Sens[7,5] <- summary(mod1)$adj.r.squared

pdf("figures/Sensitivity_check_tropho_model.pdf",width=6.9,height=6.9)  
par(mfrow=c(2,2), mar=c(4, 4, 2, 1))

barplot(Sens$V2,names.arg = c("M","1","2","3","4","5","6"),ylim=c(0,.7),
        yaxt="n",ylab="R-square")
text(8,0.65,"(a)")
axis(2,c(0,0.35,0.7),las=1)

barplot(Sens$V1,names.arg = c("M","1","2","3","4","5","6"),ylim=c(0,.6),
        yaxt="n",ylab="RMSE")
text(1,0.55,"(b)")
axis(2,c(0,0.3,0.6),las=1)

barplot(Sens$V5,names.arg = c("PM","1","2","3","4","5","6"),ylim=c(0,1),
        yaxt="n",ylab="R-square")
text(8,0.95,"(c)")
axis(2,c(0,.5,1),las=1)

barplot(Sens$V4,names.arg = c("PM","1","2","3","4","5","6"),ylim=c(0,.5),
        yaxt="n",ylab="RMSE")
text(1,0.45,"(d)")
axis(2,c(0,0.25,0.5),las=1)

dev.off()

##################################
### get prediction for subdivision
dat <- get_bio(cpue_good,t_start = 1990,t_end = 2015,spatialunit = "subdivision")  # spatialunit = "ECO_REG" or "subdivision"
dat <- subset(dat,dat$countgrid >= 6)
dat$ER <- dat$Catch_sqkm/dat$biomass

# get fraction demersal in catch versus pelagic
dat$frac <- dat$Catch_sqkm/(dat$Catch_sqkm + dat$Catch_pel_sqkm)

# conversions
dat$NPP <- dat$NPP * 9*365 # mg C / m**2 / day --> mg/m2/year == kg/km2/year
dat$ben_prod <- 10*dat$ben_prod # get detritus flux
dat$ben_prod <- dat$ben_prod *1000     # g / m**2 / year --> kg/km2/year
dat$lz_prod <-  dat$lz_prod*1000

TE <- 0.1 * 0.55 ^((dat$SST_time-10)/10)
bio <- dat$ben_prod*TE^(dat$tlw-1) + dat$frac*dat$lz_prod*TE^(dat$tlw-2.1) /dat$ER
bio <- bio/1000 #kg per km2 to tonnes per km2
biomass_obs <- dat$biomass/1000 #kg per km2 to tonnes per km2

mod1 <- lm(log10(bio)~log10(biomass_obs))
x <- c(-0.25,2.5)
y <- mod1$coefficients[1] + mod1$coefficients[2]*x
pdf("figures/Predicted_versus_observed_subsup.pdf",width=4.5,height=4.5) 
plot(log10(bio),log10(biomass_obs),xlab="Predicted biomass"
     ,ylab="Observed biomass",xaxt="n",yaxt="n",xlim=c(-0.25,2.5),ylim=c(-0.25,2.5),pch=16)
lines(y,x,lty=5)
axis(1,c(0,1,2),c("1","10","100"))
axis(2,c(0,1,2),c("1","10","100"),las=1)
abline(0,1)

text(2,0,"RMSE = 0.42 \n R2 = 0.61")

dev.off()