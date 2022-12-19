
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

# ------------------------------------------------------------------------------
# obtain data for the SEM
# ------------------------------------------------------------------------------

  load("cleaned data/surveyed_grid.RData") # get grid information
  load("cleaned data/Depth_grid.RData") # get depth per grid cell and year
  load("cleaned data/Biomass_grid.RData") # get biomass per grid cell and year
  load("cleaned data/sstdat_1967_2018_COBE.RData") # get SST COBE
  
  source("scripts - data analyses/source_get_bio_function.R") # source script to obtain biomass per ecoregion/subdiv
  
  save.image (file = "figures/my_work_space.RData")

  # data selection and results are obtained with R Markdown

# ------------------------------------------------------------------------------
# R Markdown -->> run SEM final - knitted output #
# ------------------------------------------------------------------------------

  # analyse output
  datSEM <- read.csv("figures/SEM_out.csv")
  
  # get average standardized coef
  datSEMavg <- data.frame(datSEM[1:9,2:4],AVG_all = rowMeans(data.frame(datSEM[1:9,11],datSEM[81:89,11])),
                          AVG_years = rowMeans(data.frame(datSEM[21:29,11],datSEM[41:49,11],datSEM[61:69,11],
                                                          datSEM[101:109,11],datSEM[121:129,11],datSEM[141:149,11])))
  
  Rsqavg <- data.frame(datSEM[17:20,2:4],AVG_all = rowMeans(data.frame(datSEM[17:20,6],datSEM[97:100,6])),
                          AVG_years = rowMeans(data.frame(datSEM[37:40,6],datSEM[57:60,6],datSEM[77:80,6],
                                                          datSEM[117:120,6],datSEM[137:140,6],datSEM[157:160,6])))
  
  
  # get nb of times p <0.05
  pcount = data.frame(datSEM[1:9,9],datSEM[81:89,9])
  datSEMavg$pcount_all <- rowSums(pcount <0.05)
  
  pcount = data.frame(datSEM[21:29,9],datSEM[41:49,9],datSEM[61:69,9],
                      datSEM[101:109,9],datSEM[121:129,9],datSEM[141:149,9])
  datSEMavg$pcount_years <- rowSums(pcount <0.05)
  datSEMavg;Rsqavg
  
# ------------------------------------------------------------------------------
# R Markdown -->> run SEM final - knitted output_50_250_mdepth #
# ------------------------------------------------------------------------------
  
  # analyse output
  datSEM <- read.csv("figures/SEM_out50_250.csv")
  
  # get average standardized coef
  datSEMavg <- data.frame(datSEM[1:9,2:4],AVG_all = rowMeans(data.frame(datSEM[1:9,11],datSEM[81:89,11])),
                          AVG_years = rowMeans(data.frame(datSEM[21:29,11],datSEM[41:49,11],datSEM[61:69,11],
                                                          datSEM[101:109,11],datSEM[121:129,11],datSEM[141:149,11])))
  
  Rsqavg <- data.frame(datSEM[17:20,2:4],AVG_all = rowMeans(data.frame(datSEM[17:20,6],datSEM[97:100,6])),
                       AVG_years = rowMeans(data.frame(datSEM[37:40,6],datSEM[57:60,6],datSEM[77:80,6],
                                                       datSEM[117:120,6],datSEM[137:140,6],datSEM[157:160,6])))
  
  # get nb of times p <0.05
  pcount = data.frame(datSEM[1:9,9],datSEM[81:89,9])
  datSEMavg$pcount_all <- rowSums(pcount <0.05)
  
  pcount = data.frame(datSEM[21:29,9],datSEM[41:49,9],datSEM[61:69,9],
                      datSEM[101:109,9],datSEM[121:129,9],datSEM[141:149,9])
  datSEMavg$pcount_years <- rowSums(pcount <0.05)
  datSEMavg;Rsqavg
  
# ------------------------------------------------------------------------------
# R Markdown -->> run SEM final - sensitivity of grid nb1 #
# ------------------------------------------------------------------------------  

  # analyse output
  datSEM <- read.csv("figures/SEM_out_grid1.csv")
  
  # get average standardized coef
  datSEMavg <- data.frame(datSEM[1:9,2:4],AVG_all = rowMeans(data.frame(datSEM[1:9,11],datSEM[81:89,11])),
                          AVG_years = rowMeans(data.frame(datSEM[21:29,11],datSEM[41:49,11],datSEM[61:69,11],
                                                          datSEM[101:109,11],datSEM[121:129,11],datSEM[141:149,11])))
  
  Rsqavg <- data.frame(datSEM[17:20,2:4],AVG_all = rowMeans(data.frame(datSEM[17:20,6],datSEM[97:100,6])),
                       AVG_years = rowMeans(data.frame(datSEM[37:40,6],datSEM[57:60,6],datSEM[77:80,6],
                                                       datSEM[117:120,6],datSEM[137:140,6],datSEM[157:160,6])))
  
  # get nb of times p <0.05
  pcount = data.frame(datSEM[1:9,9],datSEM[81:89,9])
  datSEMavg$pcount_all <- rowSums(pcount <0.05)
  
  pcount = data.frame(datSEM[21:29,9],datSEM[41:49,9],datSEM[61:69,9],
                      datSEM[101:109,9],datSEM[121:129,9],datSEM[141:149,9])
  datSEMavg$pcount_years <- rowSums(pcount <0.05)
  datSEMavg;Rsqavg
  
# ------------------------------------------------------------------------------
# R Markdown -->> run SEM final - sensitivity of grid nb10 #
# ------------------------------------------------------------------------------
  
  # analyse output
  datSEM <- read.csv("figures/SEM_out_grid10.csv")
  
  # get average standardized coef
  datSEMavg <- data.frame(datSEM[1:9,2:4],AVG_all = rowMeans(data.frame(datSEM[1:9,11],datSEM[81:89,11])),
                          AVG_years = rowMeans(data.frame(datSEM[21:29,11],datSEM[41:49,11],datSEM[61:69,11],
                                                          datSEM[101:109,11],datSEM[121:129,11],datSEM[141:149,11])))
  
  Rsqavg <- data.frame(datSEM[17:20,2:4],AVG_all = rowMeans(data.frame(datSEM[17:20,6],datSEM[97:100,6])),
                       AVG_years = rowMeans(data.frame(datSEM[37:40,6],datSEM[57:60,6],datSEM[77:80,6],
                                                       datSEM[117:120,6],datSEM[137:140,6],datSEM[157:160,6])))
  
  # get nb of times p <0.05
  pcount = data.frame(datSEM[1:9,9],datSEM[81:89,9])
  datSEMavg$pcount_all <- rowSums(pcount <0.05)
  
  pcount = data.frame(datSEM[21:29,9],datSEM[41:49,9],datSEM[61:69,9],
                      datSEM[101:109,9],datSEM[121:129,9],datSEM[141:149,9])
  datSEMavg$pcount_years <- rowSums(pcount <0.05)
  datSEMavg;Rsqavg
  
# ------------------------------------------------------------------------------
# plot spatial map of residuals 
# ------------------------------------------------------------------------------
  
  library(ggplot2)
  library(viridis)
  library(gridExtra)  
  library(latex2exp)
  
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
  load("cleaned data/Biomass_grid.RData") # get biomass per grid cell and year
  load("cleaned data/sstdat_1967_2018_COBE.RData") # get SST COBE
 
  datbio <- get_bio(cpue_good,t_start = tst[j],t_end = ten[j],spatialunit = unit[j])  # spatialunit = "ECO_REG" or "subdivision"
  datbio <- subset(datbio,datbio$countgrid >= 6)
  datbio$ER_log <- log10((datbio$Catch_sqkm /datbio$biomass))
  datbio$biomass <- datbio$biomass / 1000 # get tonnes/km2
  datbio$z_prod <- datbio$lz_prod + datbio$mz_prod
  
  mod1 <- lm(biomass ~ tlw + ER_log + SST_time +  z_prod,dat=datbio)
  #plot(mod1) # residuals in most runs reasonable  
  datbio$residuals <- residuals(mod1)
  datbio$residuals <- ifelse(datbio$residuals > 50, 50,datbio$residuals)
  datbio$residuals <- ifelse(datbio$residuals < -50, -50,datbio$residuals)
  
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
    scale_fill_viridis_c(name="Residual biomass \n (MT/km2)",limits=c(-50,50),
                         labels = c(TeX("$\\leq$-50"),"-25","0","25",TeX("$\\geq$50")),
                         breaks=c(-50,-25,0,25,50)) +
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
