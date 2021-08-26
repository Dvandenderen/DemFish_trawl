
# get libraries
library(dplyr)
library(raster)
library(sp)
library(rgdal)
library(ggplot2)

# get cleaned data
source("scripts - data processing/source_combine_all_surveys_after_cleaning.R")
trawl_backup <- trawl
# trawl <- trawl_backup

# select years from 2000
  trawl <- subset(trawl, trawl$year > 2000)

# get pel and dem separate 
  trawl_sep <- trawl %>% 
    group_by(haulid,region,gear,year,month,lon,lat,type) %>%
    summarize_at(.vars=c('wtcpue', 'wtcpue_q'), .funs = function(x) sum(x)) %>% 
    dplyr::select(haulid,region,gear,year,month,lon,lat,type,wtcpue,wtcpue_q) %>%
    as.data.frame()

# now merge per staion and year
  trawl <- trawl %>% 
    group_by(haulid,region,gear,year,month,lon,lat) %>%
    summarize_at(.vars=c('wtcpue', 'wtcpue_q'), .funs = function(x) sum(x)) %>% 
    dplyr::select(haulid,region,gear,year,month,lon,lat,wtcpue,wtcpue_q) %>%
    as.data.frame()

# add dem 
  trawl_sep <- subset(trawl_sep,trawl_sep$type =="dem")
  colnames(trawl_sep)[which(colnames(trawl_sep) %in% c("wtcpue","wtcpue_q"))] <- c("wtcpue_Dem","wtcpue_q_Dem")
  
# tot-dem == pel
  trawl <- cbind(trawl,trawl_sep[match(trawl$haulid,trawl_sep$haulid), c("wtcpue_Dem","wtcpue_q_Dem")])
  trawl$wtcpue_Pel <- trawl$wtcpue - trawl$wtcpue_Dem
  trawl$wtcpue_q_Pel <- trawl$wtcpue_q - trawl$wtcpue_q_Dem

# remove lowest and highest 2% of wtcpue_q per survey
  high <- trawl %>%
    group_by(region)  %>%
       slice_max(wtcpue_q, prop = 0.02) 
  
  low <- trawl %>%
    group_by(region)  %>%
    slice_min(wtcpue_q, prop = 0.02) 
  
  LH_ends <- c(unique(high$haulid),unique(low$haulid))

  trawl <- trawl %>% 
    filter(!(haulid %in% LH_ends)) %>%
  as.data.frame()

# estimate kg per km2 on a 1 degree grid
  trawl$uniq <- paste(trawl$lon,trawl$lat,sep="_")
  coords_uni <- unique(trawl$uniq)
  t <- strsplit(coords_uni, "_")
  coords<- matrix(unlist(t), ncol=2, byrow=TRUE)
  coords <- as.data.frame(coords)
  coords[,1] <- as.numeric(as.character(coords[,1]))
  coords[,2] <- as.numeric(as.character(coords[,2]))
  coords[,3] <- coords_uni

# assign area of interest one degrees
  gt<-(GridTopology(c(-179.5, -89.5), c(1, 1), c(360, 180))) # c(long, lat), c(cellsize long, lat), c(nb of grids long, lat)
  grt<-SpatialGrid(gt, proj4string=CRS("+init=epsg:4326"))
  spix <- as(grt, "SpatialPixels")
  spol <- as(spix, "SpatialPolygons")
  rnames<-sapply(slot(spol, "polygons"), function(x) slot(x, "ID"))
  LOCUNI<-as.data.frame(seq(1,length(spix)))
  rownames(LOCUNI)<-rnames
  bargrid<-SpatialPolygonsDataFrame(spol, LOCUNI)
  bargrid@bbox # make sure "min" is a whole number
  bargrid@data$uni <- c(1:length(bargrid))

  coord <-data.frame(Longitude = coords[,1], Latitude = coords[,2])
  coordinates(coord)<- ~ Longitude + Latitude  
  crs(coord) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  bargrid <- spTransform(bargrid,CRS(proj4string(coord))) # make it similar to bargrid
  bargrid@proj4string # check coordinates reference system again
  tr <- over(coord,bargrid)
  coords$one_degrees <- tr$uni

# now get kg per km2 per 1 degrees grid
  trawl <- cbind(trawl,coords[match(trawl$uniq,coords[,3]),c("one_degrees")])
  colnames(trawl)[ncol(trawl)] <- "one_degrees"
  
  cpue <- trawl %>%
    group_by(one_degrees) %>%
    summarise_at (c("wtcpue_q","wtcpue_q_Dem","wtcpue_q_Pel"),mean, na.rm=T) %>%
    as.data.frame ()
  
  bargrid <- cbind(bargrid,cpue[match(bargrid@data$uni,cpue$one_degrees),c("wtcpue_q","wtcpue_q_Dem","wtcpue_q_Pel")])
  #colnames(bargrid@data)[ncol(bargrid@data)] <- "wtcpue"
  ncoords <- subset(bargrid,!(is.na(bargrid@data$wtcpue_q)))
  
  filedata <- data.frame(coordinates(ncoords))
  colnames(filedata) <- c("long","lat")

# Get the world map
  ctrys <- rnaturalearth::ne_countries(scale = 50, returnclass = "sf")
  minlong <- -180 #round(min(filedata$long)-1)
  maxlong <- 51
  minlat  <- 23
  maxlat  <- 83
  coordslim <- c(minlong,maxlong,minlat,maxlat)
  coordxmap <- round(seq(minlong,maxlong,length.out = 4))
  coordymap <- round(seq(minlat,maxlat,length.out = 4))
  
  idx <- which(colnames(ncoords@data) %in% c("wtcpue_q","wtcpue_q_Dem","wtcpue_q_Pel"))
  ncoords@data[,idx] <- ncoords@data[,idx]/1000 # kg per km2 to tonnes per km2
  ncoords$tot_plot <- ncoords$wtcpue_q
  ncoords$dem_plot <- ncoords$wtcpue_q_Dem
  ncoords$pel_plot <- ncoords$wtcpue_q_Pel
  
  roundUp <- function(x) 10^ceiling(log10(x)) # round to nearest 10
  ncoords$tot_plot[ncoords$tot_plot > pretty(quantile(ncoords$tot_plot, 0.95))[1]] <- pretty(quantile(ncoords$tot_plot, 0.95))[1]
  ncoords$dem_plot[ncoords$dem_plot > pretty(quantile(ncoords$dem_plot, 0.95))[1]] <- pretty(quantile(ncoords$dem_plot, 0.95))[1]
  ncoords$pel_plot[ncoords$pel_plot > pretty(quantile(ncoords$pel_plot, 0.95))[1]] <- pretty(quantile(ncoords$pel_plot, 0.95))[1]
  
  
  nco <- sf::st_as_sf(ncoords)

## make color scale
  sealand = c("#8C66FF", "#6A66FF", "#6684FF", "#66A7FF", 
              "#66CAFF", "#66ECFF", "#66FFF0", "#66FFCE", "#66FFAB", "#66FF88", 
              "#66FF66", "#88FF66", "#ABFF66", "#CEFF66", "#FFEEA6", "#FFD3A6", 
              "#FFB8A6", "#FFAAB0", "#FFB5CB", "#FFC0E1")
  
  color_pallet_function <- colorRampPalette(colors = sealand)
  
  figmap <- ggplot(nco) + 
    geom_sf( aes(fill=pel_plot), colour = NA ) + 
    scale_fill_gradientn(colours=color_pallet_function(30),name="Tonnes / km2") +
    geom_sf(data = ctrys, fill="dark grey",colour=NA) 
  
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

####### fisheries data

### load Regs database 
setwd("C:/Users/danie/Dropbox/Werk/Demersal fish and fisheries/Data analysis")
C0004 <- readRDS("Catch_all_2000_2004.rds")
C0509 <- readRDS("Catch_all_2005_2009.rds")
C1014 <- readRDS("Catch_all_2010_2014.rds")

Catch <- rbind(C0004,C0509,C1014)
Catch <- subset(Catch, Catch$Funcgroup %in% c(4,5,6,13,14,15,23,24))
Catch$Tot <- Catch$Reported + Catch$IUU + Catch$Discards
Catch <- aggregate(Catch$Tot, by= list(Catch$Cell,Catch$IYear),FUN= sum,na.rm=T)
Catch <- aggregate(Catch$x, by= list(Catch$Group.1),FUN= mean,na.rm=T)

### load Watson cells
setwd("C:/Users/danie/Dropbox/Werk/Demersal fish and fisheries/Data analysis")
cells <- read.csv(file="World_watson.csv",sep=";",header=T)
Catch <- cbind(Catch, cells[match(Catch$Group.1,cells$Cell), c(2:4)])
colnames(Catch) <- c("Cell","catch","long","lat","OceanAreasqkm")
Catch$catch  <- Catch$catch/ Catch$OceanAreasqkm # tonnes / cell --> tonnes per km2

## link to one degrees grid
catchcoord <-data.frame(Longitude = Catch$long , Latitude = Catch$lat)
coordinates(catchcoord)<- ~ Longitude + Latitude  
crs(catchcoord) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
tr <- over(catchcoord,ncoords)
Catch$one_degrees <- tr$uni
Catch <- subset(Catch,!(is.na(Catch$one_degrees)))

# get mean per one degrees 
TT <- aggregate(Catch$catch, by= list(Catch$one_degrees),FUN= mean,na.rm=T)
ncoords <- cbind(ncoords,TT[match(ncoords@data$uni,TT$Group.1),c("x")])
colnames(ncoords@data)[ncol(ncoords@data)] <- "catch"

ncoords$Landings_plot <- ncoords$Landings
ncoords$Landings_plot[ncoords$Landings_plot >3] <- 3

nco <- sf::st_as_sf(ncoords)

figmap <- ggplot(nco) + 
  geom_sf( aes(fill=Landings_plot), colour = NA ) + 
  scale_fill_gradient2(low = "blue",mid = "#ffffbf",high = "#d7191c", midpoint = 1.5, limits=c(0, 3),name="Tonnes km-2 y-1")+
  geom_sf(data = ctrys, fill="dark grey",colour=NA) 

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


# load marine ecoregions and calculate average biomass + catch per region 
library(raster)
library(sp)
library(rgdal)

shape <- readOGR(dsn = "C:/Users/danie/Dropbox/Werk/Demersal fish and fisheries/Data analysis/MEOW shapefiles" ,layer="meow_ecos")

coord <-data.frame(Longitude = coordinates(ncoords)[,1], Latitude = coordinates(ncoords)[,2])
coordinates(coord)<- ~ Longitude + Latitude  
crs(coord) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

shape <- spTransform(shape,CRS(proj4string(coord))) # make it similar to bargrid
shape@proj4string # check coordinates reference system again
tr <- over(coord,shape)
ncoords$ECO_CODE <- tr[,1]
ncoords$ECO_REG  <- tr[,2]

table(ncoords$ECO_REG)
ncoords_new <- subset(ncoords,!(ncoords$ECO_REG %in% c("Faroe Plateau", "North and East Iceland",
                                                       "Northern Grand Banks - Southern Labrador",
                                                       "Puget Trough/Georgia Basin",
                                                       "Southern Grand Banks - South Newfoundland")))
overview <- aggregate(ncoords_new$wtcpue_q_Dem,by=list(ncoords_new$ECO_REG),FUN=mean,na.rm=T)
overview <- cbind(overview,aggregate(ncoords_new$catch,by=list(ncoords_new$ECO_REG),FUN=mean,na.rm=T)[,2])
colnames(overview) <-c("Ecreg","Biomass","Catch")
overview$ER <- overview$Catch/ overview$Biomass
overview$B_units <- "tonnes/km2"
overview$C_units <- "tonnes/km2/Y"


############################## check below
ncoords$frac <- (ncoords$Landings/(ncoords$wtcpue)*100)
ncoords$frac[ncoords$frac >60] <- 60
nco <- sf::st_as_sf(ncoords)

figmap <- ggplot(nco) + 
  geom_sf( aes(fill=frac), colour = NA ) + 
  scale_fill_gradient2(low = "blue",mid = "#ffffbf",high = "#d7191c", midpoint = 30, limits=c(0, 60),name="")+
  geom_sf(data = ctrys, fill="dark grey",colour=NA) 

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


##### add FEISTY predictions
### download biomass
dataglob <- read.csv("C:/Users/danie/Documents/Online for git/Fish_foodwebs/Global model parfor/datglob.csv",header=F)
colnames(dataglob) <- c("BioZs","BioZl","BioBs","BioBl", "BioFf","BioMf","BioPf","BioBaP","BioDf")

# load input parameters
param <- read.csv("C:/Users/danie/Documents/Online for git/Fish_foodwebs/Global model parfor/input_parameters.csv",header=T)
outp <- cbind(param,dataglob)
outp$long <- ifelse(outp$long > 179.5,outp$long-360,outp$long)

modelcoord <-data.frame(Longitude = outp$long , Latitude = outp$lat)
coordinates(modelcoord)<- ~ Longitude + Latitude  
crs(modelcoord) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
tr <- over(modelcoord,ncoords)
outp$one_degrees <- tr$uni
outp <- subset(outp,!(is.na(outp$one_degrees)))

ncoords <- cbind(ncoords, outp[match(ncoords@data$uni,outp$one_degrees), c("BioDf")])
colnames(ncoords@data)[ncol(ncoords@data)] <- "Model"
ncoords$Model <- ncoords$Model # grams per m2 === tonnes per km2 

ncoords$Model[ncoords$Model >30] <- 30

nco <- sf::st_as_sf(ncoords)

figmap <- ggplot(nco) + 
  geom_sf( aes(fill=Model), colour = NA ) + 
  scale_fill_gradientn(colours=color_pallet_function(30),name="Tonnes / km2") +
  geom_sf(data = ctrys, fill="dark grey",colour=NA) 

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









traits <- read.csv("Traits_fish.csv",header=T,sep=";",row.names=NULL)

trawl <- cbind(trawl,traits[match(trawl$Species,traits$taxon),c("feeding.mode")])
colnames(trawl)[ncol(trawl)] <- "feeding"

pel_trawl <- subset(trawl,trawl$feeding =="planktivorous")
dem_trawl <- subset(trawl,!(trawl$feeding =="planktivorous"))

Demtot_trawl <- dem_trawl %>% 
  group_by(HaulID) %>%
  summarise_at (c("wgth"),sum, na.rm=T)
Demtot_trawl <- as.data.frame(Demtot_trawl)

Demtot_trawl <-  cbind(Demtot_trawl,dem_trawl[match(Demtot_trawl$HaulID,dem_trawl$HaulID),c("Year","one_degrees","Survey")])
Demtot_trawl <- subset(Demtot_trawl,Demtot_trawl$Year > 1999)

Demtot_trawl2 <- Demtot_trawl %>% 
  group_by(Survey) %>%
  summarise_at (c("wgth"),mean, na.rm=T)
Demtot_trawl2 <- as.data.frame(Demtot_trawl2)

Demtot_trawl3 <- Demtot_trawl2 %>% 
  group_by(one_degrees) %>%
  summarise_at (c("wtcpue"),mean, na.rm=T)
Demtot_trawl3 <- as.data.frame(Demtot_trawl3)


outp <- cbind(outp,Demtot_trawl3[match(outp$one_degrees,Demtot_trawl3$one_degrees),c("wtcpue")])
colnames(outp)[ncol(outp)] <- "wtcpue_dem"

outp$wtcpue_dem <- outp$wtcpue_dem * outp$OceanAreasqkm /1000    # kg / km2 -> tonnes per cell

###
Peltot_trawl <- pel_trawl %>% 
  group_by(HaulID) %>%
  summarise_at (c("wtcpue"),sum, na.rm=T)
Peltot_trawl <- as.data.frame(Peltot_trawl)

Peltot_trawl <-  cbind(Peltot_trawl,pel_trawl[match(Peltot_trawl$HaulID,pel_trawl$HaulID),c("Year","one_degrees")])
Peltot_trawl <- subset(Peltot_trawl,Peltot_trawl$Year > 1999)

Peltot_trawl2 <- Peltot_trawl %>% 
  group_by(Year,one_degrees) %>%
  summarise_at (c("wtcpue"),mean, na.rm=T)
Peltot_trawl2 <- as.data.frame(Peltot_trawl2)

Peltot_trawl3 <- Peltot_trawl2 %>% 
  group_by(one_degrees) %>%
  summarise_at (c("wtcpue"),mean, na.rm=T)
Peltot_trawl3 <- as.data.frame(Peltot_trawl3)


outp <- cbind(outp,Peltot_trawl3[match(outp$one_degrees,Peltot_trawl3$one_degrees),c("wtcpue")])
colnames(outp)[ncol(outp)] <- "wtcpue_pel"

outp$wtcpue_pel <- outp$wtcpue_pel * outp$OceanAreasqkm /1000    # kg / km2 -> tonnes per cell



