
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
load("cleaned data/Depth_grid.RData") # get depth per grid cell and year
load("cleaned data/220224_biomass_grid.RData") # get biomass per grid cell and year
load("cleaned data/sstdat_1967_2018_COBE.RData") # get SST COBE

source("scripts - data analyses/source_get_bio_function.R") # source script to obtain biomass per ecoregion/subdiv

save.image (file = "figures/my_work_space_up.RData")

# create data for 
# 1) 3 time periods x 2 different regional boundaries
# 2) same as 1, but only selecting depths 50 - 250 m
# data selection and results are obtained with R Markdown

# R Markdown -->> run SEM - knitted output #
# analyse output
datSEM <- read.csv("figures/SEM_out.csv")

# get average standardized coef
datSEMavg <- data.frame(datSEM[1:20,2:4],AVG_all = rowMeans(data.frame(datSEM[1:20,6],datSEM[81:100,6])),
                        AVG_years = rowMeans(data.frame(datSEM[21:40,6],datSEM[41:60,6],datSEM[61:80,6],
                                                        datSEM[101:120,6],datSEM[121:140,6],datSEM[141:160,6])))

# get nb of times p <0.05
pcount = data.frame(datSEM[1:20,9],datSEM[81:100,9])
datSEMavg$pcount_all <- rowSums(pcount <0.05)

pcount = data.frame(datSEM[21:40,9],datSEM[41:60,9],datSEM[61:80,9],
                    datSEM[101:120,9],datSEM[121:140,9],datSEM[141:160,9])
datSEMavg$pcount_years <- rowSums(pcount <0.05)

# R Markdown -->> run SEM - knitted output_50_250_mdepth #
# analyse output
datSEM <- read.csv("figures/SEM_out50_250.csv")

# get average standardized coef
datSEMavg <- data.frame(datSEM[1:30,2:4],AVG = rowMeans(data.frame(datSEM[1:30,6],datSEM[31:60,6],datSEM[61:90,6],datSEM[91:120,6],
                                                                  datSEM[121:150,6],datSEM[151:180,6])))

# get nb of times p <0.05
pcount = data.frame(datSEM[1:30,9],datSEM[31:60,9],datSEM[61:90,9],datSEM[91:120,9],
                    datSEM[121:150,9],datSEM[151:180,9])
datSEMavg$pcount <- rowSums(pcount <0.05)

###
################### plot spatial map of residuals 

  library(ggplot2)
  library(viridis)
  library(gridExtra)  

  # prepare map
  ctrys <- rnaturalearth::ne_countries(scale = 50, returnclass = "sf")
  minlong <- -180 #round(min(filedata$long)-1)
  maxlong <- 51
  minlat  <- 23
  maxlat  <- 83
  coordslim <- c(minlong,maxlong,minlat,maxlat)
  coordxmap <- round(seq(minlong,maxlong,length.out = 4))
  coordymap <- round(seq(minlat,maxlat,length.out = 4))

  # get spatial unit, time period
  tst <- c(1990,2000,2010,1990,2000,2010) ; ten <- c(1995,2005,2015,1995,2005,2015) 
  unit <- c("subdivision","subdivision","subdivision","ECO_REG","ECO_REG","ECO_REG")
  figlab <- c("a)","b)","c)","d)","e)","f)")
  
  # biomass script
  source("scripts - data analyses/source_get_bio_function.R") 
  
for (j in 1:6){
  load("cleaned data/surveyed_grid.RData") # get grid information
  load("cleaned data/Depth_grid.RData") # get depth per grid cell and year
  load("cleaned data/220224_biomass_grid.RData") # get biomass per grid cell and year
  load("cleaned data/sstdat_1967_2018_COBE.RData") # get SST COBE
 
  datbio <- get_bio(cpue_good,t_start = tst[j],t_end = ten[j],spatialunit = unit[j])  # spatialunit = "ECO_REG" or "subdivision"
  datbio <- subset(datbio,datbio$countgrid >= 6)
  datbio$ER_log <- log10((datbio$Catch_sqkm /datbio$biomass))
  datbio$biomass <- datbio$biomass / 1000 # get tonnes/km2
  
  mod1 <- lm(biomass ~ tlw + ER_log + SST_time + ben_prod + lz_prod,dat=datbio)
  #plot(mod1) # residuals in most runs reasonable  
  datbio$residuals <- residuals(mod1)

  resplot <- subset(cpue_good,cpue_good$year %in% c(tst[j]:ten[j]))
  freq_dat <- as.data.frame(table(resplot$uni_cell))
  freq_dat <- subset(freq_dat,freq_dat$Freq > 1)
  resplot <- subset(resplot,resplot$uni_cell %in% freq_dat$Var1)
  
  grid_master <- subset(grid_master,grid_master@data$uni_cell %in% resplot$uni_cell)
  grid_master <- cbind(grid_master,datbio[match(grid_master@data[,unit[j]],datbio[,unit[j]]),c("residuals")])
  colnames(grid_master@data)[ncol(grid_master@data)]<- "resid"
  grid_master <- subset(grid_master,!(is.na(grid_master@data$resid)))
  nco <- sf::st_as_sf(grid_master)
  
  # plot map
  figmap <- ggplot(nco) + 
    geom_sf( aes(fill=resid), colour = NA ) + ggtitle(figlab[j]) +
    scale_fill_viridis(name="Residual biomass \n (MT/km2)") +
    geom_sf(data = ctrys, fill="grey",colour=NA) 
  
  figmap <-  figmap +  theme(plot.background=element_blank(),
                             panel.background=element_blank(),
                             axis.text.y   = element_text(size=9),
                             axis.text.x   = element_text(size=9),
                             axis.title.y  = element_blank(),
                             plot.title = element_text(size = 11),
                             axis.title.x  = element_blank(),
                             panel.border  = element_rect(colour = "grey", size=.5,fill=NA),
                             legend.text   = element_text(size=9),
                             legend.title  = element_text(size=9),
                             legend.key.size = unit(.4, 'cm'))+
    coord_sf(xlim = c(-180, 51), ylim = c(20, 84), expand = FALSE) +
    scale_x_continuous(breaks=coordxmap)  +
    scale_y_continuous(breaks=coordymap)
  assign(paste("figmap",j,sep="_"),figmap)
}
  pdf("figures/Residual biomass variation.pdf",width=10,height=6) 
  grid.arrange(figmap_1,figmap_4,figmap_2,figmap_5,figmap_3,figmap_6, nrow = 3,
               left = "Latitude",bottom = "Longitude")
  dev.off()
  
####################################
# below used to check different runs and to plot spatial map of residuals 

# scale parameters
semparam <- c("NPP","lz_prod","ben_prod","biomass","tlw","tl_95","depth","SST_time","ER_log")

## hypothesized model 
modT2='biomass ~ tlw + ER_log + SST_time + ben_prod + lz_prod
       tlw ~ NPP + ER_log 
       lz_prod ~ NPP +  SST_time 
       ben_prod ~ NPP + SST_time + depth +lz_prod
       NPP ~ SST_time'

datbio <- get_bio(cpue_good,t_start = 1990,t_end = 1995,spatialunit = "subdivision")  # spatialunit = "ECO_REG" or "subdivision"
datbio <- subset(datbio,datbio$countgrid >= 6)
datbio$ER_log <- log10((datbio$Catch_sqkm /datbio$biomass))
datbio[,semparam] <- scale(datbio[,semparam])
f.modT1.est <- sem(modT2,fixed.x=F,data=datbio)
summary(f.modT1.est,rsq=T,stand=T)

# check modification indices
modindices(f.modT1.est, sort = TRUE, maximum.number = 10) 

# scale parameters
semparam <- c("NPP","lz_prod","biomass","tlw","depth","SST_time","ER_log")

## hypothesized model 
modT3='biomass ~ tlw + ER_log + SST_time + lz_prod
       tlw ~ NPP + ER_log 
       lz_prod ~ NPP +  SST_time 
       NPP ~ SST_time'

datbio <- get_bio(cpue_good,t_start = 1990,t_end = 1995,spatialunit = "ECO_REG")  # spatialunit = "ECO_REG" or "subdivision"
datbio <- subset(datbio,datbio$countgrid >= 6)
datbio$ER_log <- log10((datbio$Catch_sqkm /datbio$biomass))
datbio[,semparam] <- scale(datbio[,semparam])
f.modT1.est <- sem(modT3,fixed.x=F,data=datbio)
summary(f.modT1.est,rsq=T,stand=T)


# check with linear model
mod1 <- lm(biomass ~ tlw + ER_log + SST_time + ben_prod + lz_prod,dat=datbio)
plot(mod1) # residuals in most runs reasonable  
datbio$residuals <- residuals(mod1)

resplot <- subset(cpue_good,cpue_good$year %in% c(2000:2005))
freq_dat <- as.data.frame(table(resplot$uni_cell))
freq_dat <- subset(freq_dat,freq_dat$Freq > 1)
resplot <- subset(resplot,resplot$uni_cell %in% freq_dat$Var1)

grid_master <- subset(grid_master,grid_master@data$uni_cell %in% resplot$uni_cell)
grid_master <- cbind(grid_master,datbio[match(grid_master@data$subdivision,datbio$subdivision),c("residuals")])
colnames(grid_master@data)[ncol(grid_master@data)]<- "resid"
grid_master <- subset(grid_master,!(is.na(grid_master@data$resid)))

ncoords <- grid_master

ctrys <- rnaturalearth::ne_countries(scale = 50, returnclass = "sf")
minlong <- -180 #round(min(filedata$long)-1)
maxlong <- 51
minlat  <- 23
maxlat  <- 83
coordslim <- c(minlong,maxlong,minlat,maxlat)
coordxmap <- round(seq(minlong,maxlong,length.out = 4))
coordymap <- round(seq(minlat,maxlat,length.out = 4))

nco <- sf::st_as_sf(ncoords)

library(ggplot2)
library(viridis)
library(gridExtra)

# plot map
figmap <- ggplot(nco) + 
  geom_sf( aes(fill=resid), colour = NA ) + 
  scale_fill_viridis(name="Residual variation \n in biomass") +
  geom_sf(data = ctrys, fill="grey",colour=NA) 

figmap <-  figmap +  theme(plot.background=element_blank(),
                           panel.background=element_blank(),
                           axis.text.y   = element_text(size=11),
                           axis.text.x   = element_text(size=11),
                           axis.title.y  = element_blank(),
                           axis.title.x  = element_blank(),
                           panel.border  = element_rect(colour = "grey", size=.5,fill=NA),
                           legend.text   = element_text(size=11),
                           legend.title  = element_text(size=11))+
  coord_sf(xlim = c(-180, 51), ylim = c(20, 84), expand = FALSE) +
  scale_x_continuous(breaks=coordxmap)  +
  scale_y_continuous(breaks=coordymap)

reg <- unionSpatialPolygons(grid_master,grid_master$subdivision)
figmap  <- figmap +  geom_polygon(data = reg, aes(x = long, y = lat, group = group),color="grey",fill=NA)











