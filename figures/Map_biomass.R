
rm(list=ls())

### load libraries
library(sp)
library(maptools)
library(sf)
library(gridExtra)
library(viridis)
library(ggplot2)
library(latex2exp)
library(rgdal)

##################
load("processed data/surveyed_grid.RData") # get grid information
load("processed data/Biomass_grid.RData") # get biomass per grid cell and year

cpue_final <- subset(cpue_good,cpue_good$year >= 1990 & cpue_good$year <= 2015)
cpue_final <-  aggregate(list(cpue_final$biomass,cpue_final$tlw,
                              cpue_final$Catch_sqkm,cpue_final$Catch_pel_sqkm),
                         by=list(cpue_final$uni_cell),FUN = mean,na.rm=T)
colnames(cpue_final) <- c("uni_cell","biomass","tlw","Catch_sqkm","Catch_pel_sqkm")

grid_master <- cbind(grid_master,cpue_final[match(grid_master@data$uni_cell,cpue_final$uni_cell),c(2:5)])
ncoords <- subset(grid_master,!(is.na(grid_master@data$biomass)))

ctrys <- rnaturalearth::ne_countries(scale = 50, returnclass = "sf")
minlong <- -180 #round(min(filedata$long)-1)
maxlong <- 51
minlat  <- 23
maxlat  <- 83
coordslim <- c(minlong,maxlong,minlat,maxlat)
coordxmap <- round(seq(minlong,maxlong,length.out = 4))
coordymap <- round(seq(minlat,maxlat,length.out = 4))

idx <- which(colnames(ncoords@data) %in% c("biomass"))
ncoords@data[,idx] <- ncoords@data[,idx]/1000 # kg per km2 to tonnes per km2
ncoords$dembio <- ncoords$biomass

roundUp <- function(x) 10^ceiling(log10(x)) # round to nearest 10
ncoords$dembio[ncoords$dembio > pretty(quantile(ncoords$dembio, 0.95))[1]] <- pretty(quantile(ncoords$dembio, 0.95))[1]
ncoords$dembio[ncoords$dembio>80] <- 80

nco <- sf::st_as_sf(ncoords)

# load ecoregions
shape <- readOGR(dsn = "data/MEOW shapefiles" ,layer="meow_ecos")
shape <- subset(shape,shape$ECOREGION %in% unique(nco$ECO_REG))
shape <- sf::st_as_sf(shape)

# plot map
figmap1 <- ggplot(nco) + 
  geom_sf( aes(fill=dembio), colour = NA ) + 
  scale_fill_viridis(name="Tonnes / km2 \n \n", limits = c(0,80),
                     labels = c("20","40","60",TeX("$\\geq$80")),breaks=c(20,40,60,80))  + 
  geom_sf(data=shape,col="darkgrey",fill=NA) +
  geom_sf(data = ctrys, fill="grey",colour=NA) +
  coord_sf(crs = 5070,xlim = c(-4927853,3633046),ylim=c(307076,6029855))

figmap1 <- figmap1 +  theme(plot.background=element_blank(),
                            panel.grid.major = element_line(colour = "grey"),
                            panel.background=element_blank(),
                            axis.text.y   = element_text(size=10),
                            axis.text.x   = element_text(size=10),
                            axis.title.y  = element_blank(),
                            plot.title = element_text(size = 11),
                            axis.title.x  = element_blank(),
                            panel.border  = element_rect(colour = "grey", size=.5,fill=NA),
                            legend.text   = element_text(size=10),
                            legend.title  = element_text(size=10),
                            legend.key.size = unit(.4, 'cm'),
                            legend.position = "bottom")

figmap2 <- ggplot(nco) + 
  geom_sf( aes(fill=dembio), colour = NA ) + 
  scale_fill_viridis(name="Tonnes / km2 \n \n", limits = c(0,80),
                     labels = c("20","40","60",TeX("$\\geq$80")),breaks=c(20,40,60,80))  +
  geom_sf(data=shape,col="darkgrey",fill=NA) +
  geom_sf(data = ctrys, fill="grey",colour=NA) +
  coord_sf(crs =  "+proj=aea +lat_0=23 +lon_0=10 +lat_1=29.5 +lat_2=45.5 +x_0=0 
           +y_0=0 +datum=NAD83 +units=m +no_defs", 
           xlim = c(-2427853,2333046),ylim=c(807076,6029855))

figmap2 <-  figmap2 +  theme(plot.background=element_blank(),
                             panel.grid.major = element_line(colour = "grey"),
                             panel.background=element_blank(),
                             axis.text.y   = element_text(size=10),
                             axis.text.x   = element_text(size=10),
                             axis.title.y  = element_blank(),
                             plot.title = element_text(size = 11),
                             axis.title.x  = element_blank(),
                             panel.border  = element_rect(colour = "grey", size=.5,fill=NA),
                             legend.text   = element_text(size=10),
                             legend.title  = element_text(size=9),
                             legend.key.size = unit(.4, 'cm'),
                             legend.position = "bottom")

upper  <- cowplot::plot_grid(figmap1,figmap2,nrow = 1, rel_widths=c(0.60,0.40),labels = c('a)',''), label_size = 10,label_fontface="plain")


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
load("processed data/surveyed_grid.RData") # get grid information
load("processed data/Depth_grid.RData") # get depth per grid cell and year
load("processed data/Biomass_grid.RData") # get biomass per grid cell and year
load("processed data/sstdat_1967_2018_COBE.RData") # get SST COBE

source("scripts - data analyses/source_get_bio_function.R") # source script to obtain biomass per ecoregion/subdiv

dat <- get_bio(cpue_good,t_start = 1990,t_end = 2015,spatialunit = "ECO_REG")  # spatialunit = "ECO_REG" or "subdivision"
dat$ER <- dat$Catch_sqkm / dat$biomass
dat$biomass <-dat$biomass / 1000

sc1 <- qplot(SST_time, biomass, data=dat) + 
  geom_smooth(method="lm",color="grey", formula=y~x,se=F,linetype = "dashed")+
           xlab("Temperature \n (degrees C)") + ylab("Biomass \n (Tonnes / km2)")+
           scale_y_continuous(breaks=seq(0, 100, by = 25)) + 
           scale_x_continuous(breaks=seq(0, 24, by = 12)) +
           theme(plot.background=element_blank(),
           panel.background=element_blank(),
           axis.text.y   = element_text(size=11),
           axis.text.x   = element_text(size=11),
           axis.title.y  = element_text(size=11),
           axis.title.x  = element_text(size=11),
           panel.border  = element_rect(colour = "grey", size=.5,fill=NA),
           legend.text   = element_text(size=11),
           legend.title  = element_text(size=11)) 

sc2 <- qplot(log10(ER), biomass, data=dat) + ylab("") +
      scale_y_continuous(breaks=seq(0, 100, by = 25)) + 
      scale_x_continuous(name = "Exploit. rate \n (per year)", 
                         labels = scales::math_format(10^.x)) +
      annotation_logticks(sides='b',short = unit(0.025, "cm"),
                          mid = unit(0.05, "cm"),long = unit(0.075, "cm")) +
  theme(plot.background=element_blank(),
        panel.background=element_blank(),
        axis.text.y   = element_text(size=11),
        axis.text.x   = element_text(size=11),
        axis.title.y  = element_text(size=11),
        axis.title.x  = element_text(size=11),
        panel.border  = element_rect(colour = "grey", size=.5,fill=NA),
        legend.text   = element_text(size=11),
        legend.title  = element_text(size=11)) + 
  geom_smooth(color="grey", method="lm", formula=y~x,se=F,linetype = "dashed")
  
sc3 <- qplot(NPP, biomass, data=dat) + ylab("") +
        xlab("Net primary prod.\n (mg C / m2 / day)") + ylab("")+
        scale_y_continuous(breaks=seq(0, 100, by = 25)) + 
        scale_x_continuous(breaks=seq(250, 750, by = 250),limits = c(190,750)) +
        theme(plot.background=element_blank(),
        panel.background=element_blank(),
        axis.text.y   = element_text(size=11),
        axis.text.x   = element_text(size=11),
        axis.title.y  = element_text(size=11),
        axis.title.x  = element_text(size=11),
        panel.border  = element_rect(colour = "grey", size=.5,fill=NA),
        legend.text   = element_text(size=11),
        legend.title  = element_text(size=11))  +
        geom_smooth(method="lm",color="grey", formula=y~x,se=F,linetype = "dashed")

lower <- cowplot::plot_grid(sc1,sc2,sc3,nrow = 1, rel_widths=c(1.1,1,1),
                   labels = c('b)','c)','d)'), 
                   label_size = 10,label_fontface="plain")

pdf("figures/Map_biomass.pdf",width=10,height=6.9) 
cowplot::plot_grid(upper,lower,ncol=1,rel_heights = c(0.66,0.33),labels=c("","")) 
dev.off()

# estimate correlations at ecoregion scale
  dat$ER_log  <- log10(dat$ER)
  cor.test(dat$bio,dat$ER_log, method="pearson")
  cor(dat$bio,dat$SST_time)
  cor(dat$bio,dat$NPP)
  cor(dat$bio,dat$lz_prod+dat$mz_prod)
  cor(dat$bio,dat$ben_prod)
  cor(dat$bio,dat$depth)
  cor(dat$bio,dat$tlw)

