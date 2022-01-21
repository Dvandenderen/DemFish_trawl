
rm(list=ls())

### load libraries
library(sp)
library(maptools)
library(sf)
library(gridExtra)
library(viridis)
library(ggplot2)

##################
load("cleaned data/surveyed_grid.RData") # get grid information
load("cleaned data/211216_biomass_grid.RData") # get biomass per grid cell and year

cpue_final <- subset(cpue_good,cpue_good$year >= 2000 & cpue_good$year <= 2010)
cpue_final <-  aggregate(list(cpue_final$biomass,cpue_final$tlw,cpue_final$tl_95,
                              cpue_final$Catch_sqkm,cpue_final$Catch_pel_sqkm),
                         by=list(cpue_final$uni_cell),FUN=mean,na.rm=T)
colnames(cpue_final) <- c("uni_cell","biomass","tlw","tl_95","Catch_sqkm","Catch_pel_sqkm")

grid_master <- cbind(grid_master,cpue_final[match(grid_master@data$uni_cell,cpue_final$uni_cell),c(2:6)])
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

nco <- sf::st_as_sf(ncoords)

# plot map
figmap <- ggplot(nco) + 
  geom_sf( aes(fill=dembio), colour = NA ) + 
  scale_fill_viridis(name="Tonnes / km2") +
    geom_sf(data = ctrys, fill="grey",colour=NA) 

figmap <-  figmap +  theme(plot.background=element_blank(),
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

## now add ER
ncoords$ER <- ncoords$Catch_sqkm/1000/ncoords$biomass
quat<-c(-1,0.01,0.02,0.04,0.06,0.1,0.15,100)
label_all <- c("<0.01","0.01-0.02", "0.02-0.04","0.04-0.06","0.06-0.10","0.10-0.15",">0.15")
ncoords$cat<- as.factor(cut(ncoords$ER,quat,right=FALSE))
nco <- sf::st_as_sf(ncoords)

figmap2 <- ggplot(nco) + 
  geom_sf( aes(fill=as.factor(cat)), colour = NA , lwd = 1) + 
  scale_fill_manual(values = c("#5A2995", "#952EA0","#D44292", "#F66D7A","#F6A97A",
                               "#EDD9A3","#fff7bc"),labels=label_all,name  ="Exploit. rate")+
  geom_sf(data = ctrys, fill="grey",colour=NA) 

# scale_fill_gradientn(colours=color_pallet_function(30)) +

figmap2 <-  figmap2 +  theme(plot.background=element_blank(),
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


pdf("figures/biomass_ER_map.pdf",width=10,height=6.9) 
grid.arrange(figmap,figmap2, nrow = 2,
             left = "Latitude",bottom = "Longitude")
dev.off()
