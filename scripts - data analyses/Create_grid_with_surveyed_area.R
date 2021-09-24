
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

###########
### load all survey data and create spatial object of the area surveyed
### calculate area of each grid cell on land/sea (takes some time)
###########

source("scripts - data processing/source_combine_all_surveys_after_cleaning.R")

# get unique lon x lat
trawl$uniq <- paste(trawl$lon,trawl$lat,sep="_")
coords_uni <- unique(trawl$uniq)
t <- strsplit(coords_uni, "_")
coords<- matrix(unlist(t), ncol=2, byrow=TRUE)
coords <- as.data.frame(coords)
coords[,1] <- as.numeric(as.character(coords[,1]))
coords[,2] <- as.numeric(as.character(coords[,2]))
coords[,3] <- coords_uni

# assign area of interest one degrees
gt<-(GridTopology(c(-179.5, -89.75), c(1, .5), c(360*1, 180*2))) # c(long, lat), c(cellsize long, lat), c(nb of grids long, lat)
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

# select all with survey data
grid_master <- subset(bargrid,bargrid@data$uni %in% tr$uni)
grid_master <- grid_master[,-1]
grid_master$area_sqkm <- area(grid_master)/10^6

# get fraction per grid cell on land
land <- rnaturalearth::ne_countries(scale = 10, returnclass = "sf")
sf_grid_master <- st_as_sf(grid_master)
tt2 <- sf::st_intersection(sf_grid_master, land) 
land_over <- data.frame(uni = tt2$uni, land_sqkm = st_area(tt2)) 
land_over$land_sqkm <- land_over$land_sqkm / 10^6 # m2 to km2
land_over$land_sqkm <- unclass(land_over$land_sqkm)
grid_master <- cbind(grid_master,land_over[match(grid_master@data$uni,land_over$uni), c(2)])
colnames(grid_master@data)[ncol(grid_master@data)] <- "land_sqkm"
grid_master$land_sqkm <- ifelse(is.na(grid_master$land_sqkm), 0,grid_master$land_sqkm)

# get ocean area
grid_master$ocean_sqkm <- grid_master$area_sqkm - grid_master$land_sqkm

# link the grid cells to ecoregions
shape <- readOGR(dsn = "C:/Users/danie/Dropbox/Werk/Demersal fish and fisheries/Data analysis/MEOW shapefiles" ,layer="meow_ecos")

coord <-data.frame(Longitude = coordinates(grid_master)[,1], Latitude = coordinates(grid_master)[,2])
coordinates(coord)<- ~ Longitude + Latitude  
crs(coord) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

shape <- spTransform(shape,CRS(proj4string(coord))) # make it similar to bargrid
shape@proj4string # check coordinates reference system again
tr <- over(coord,shape)
grid_master$ECO_CODE <- tr[,1]
grid_master$ECO_REG  <- tr[,2]

grid_master <- subset(grid_master,!(grid_master@data$ECO_REG %in% c("Faroe Plateau", "North and East Iceland",
                                                                    "Northern Grand Banks - Southern Labrador",
                                                                    "Puget Trough/Georgia Basin",
                                                                    "Southern Grand Banks - South Newfoundland","Greater Antilles")))
shape <- subset(shape,shape@data$ECOREGION %in% grid_master$ECO_REG)

save(grid_master,file="cleaned data/surveyed_grid.RData")

# now load Env conditions and link to surveyed grid
lon <- seq(from = -180+1/12,to = 180-1/12,length.out = 2160)
lat <- seq(from = 90-1/12,to = -90+1/12,length.out = 1080)

# get all coords 
coords <- merge(lon,lat)
coords <- subset(coords,coords$x < 55)
coords <- subset(coords,coords$y > 20)

coord <-data.frame(Longitude = coords[,1], Latitude = coords[,2])
coordinates(coord)<- ~ Longitude + Latitude  
crs(coord) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
tr <- over(coord,grid_master)
coords <- cbind(coords,tr)
coords <- subset(coords,!(is.na(coords$uni)))

# load environmental conditions
load(paste(getwd(),"_noGIT/Environmental data/SST/sst_average_2005_2010.RData",sep=""))
load(paste(getwd(),"_noGIT/Environmental data/NPP/NPP_Cafe_average_2005_2010.RData",sep=""))
load(paste(getwd(),"_noGIT/Environmental data/ChlA_modis/chl_average_2005_2010.RData",sep=""))
load(paste(getwd(),"_noGIT/Environmental data/ChlA_seawiffs/chlo-seawifs.RData",sep=""))
load(paste(getwd(),"_noGIT/Environmental data/Depth/ETOPO_1over6-grid_depth.RData",sep=""))

Depth_output[[3]][Depth_output[[3]] >0] <- NA
chlA_output[[3]][chlA_output[[3]] == 0] <- NA
chlA_output[[5]][chlA_output[[5]] == 0] <- NA
NPP_output[[3]][NPP_output[[3]] == 0]   <- NA
chl_output[[3]][chl_output[[3]] == 0]   <- NA

coords$Depth <- NA
coords$NPP <- NA
coords$SST <- NA
coords$chla <- NA
coords$chla_wiffs <- NA
coords$maxchla <- NA

for(j in 1:nrow(coords)){
  idx_lon <-  which(coords[j,1] == lon)
  idx_lat <-  which(coords[j,2] == lat)
  coords$Depth[j] <- Depth_output[[3]][idx_lat,idx_lon]
  coords$NPP[j]   <- NPP_output[[3]][idx_lat,idx_lon]
  coords$SST[j]   <- sst_output[[3]][idx_lat,idx_lon]
  coords$chla[j] <- chl_output[[3]][idx_lat,idx_lon]
  coords$chla_wiffs[j] <- chlA_output[[3]][idx_lat,idx_lon]
  coords$maxchla[j] <- chlA_output[[5]][idx_lat,idx_lon]
  
}

tnew <- aggregate(list(coords$Depth,coords$SST,coords$NPP,coords$chla,coords$chla_wiffs,coords$maxchla),by=list(coords$uni), FUN= mean,na.rm=T)
colnames(tnew) <- c("uni","Depth","SST","NPP","chla","chla_wiffs","maxchla")

grid_master <- cbind(grid_master,tnew[match(grid_master@data$uni,tnew$uni), c(2:7)])

cobalt <- read.csv(paste(getwd(),"_noGIT/Environmental data/Cobalt/Cobalt_output.csv",sep=""),sep=",")
cobalt$long <- ifelse(cobalt$long >180, cobalt$long - 360, cobalt$long)
lonlat <- coordinates(grid_master)
lonlat <- as.data.frame(lonlat)
colnames(lonlat) <- c("xlong", "ylat")
lonlat$uni <- paste(lonlat$xlong,lonlat$ylat)

lats <- unique(cobalt$lat)

grid_master$lz_prod <- NA
grid_master$mz_prod <- NA
grid_master$ben_prod <- NA
grid_master$lz_bio <- NA
grid_master$mz_bio <- NA

for(j in 1:nrow(grid_master@data)){
  idx_lat <- which(abs(lats-lonlat[j,2])==min(abs(lats-lonlat[j,2])))
  idx_row <- which(cobalt$long == lonlat[j,1] & cobalt$lat == lats[idx_lat])

  if (length(idx_row) >0){
    grid_master$lz_prod[j] <- cobalt$lz_prod[idx_row]
    grid_master$mz_prod[j]   <- cobalt$mz_prod[idx_row]
    grid_master$ben_prod[j]   <- cobalt$ben_prod[idx_row]
    grid_master$lz_bio[j] <- cobalt$lz_bio[idx_row]
    grid_master$mz_bio[j] <- cobalt$mz_bio[idx_row]
}}

save(grid_master,file="cleaned data/surveyed_grid.RData")

# check the data
#library(leaflet)
#library(htmlwidgets)

#mfs <- leaflet() %>%
#  addProviderTiles(providers$Esri.WorldImagery) %>%
  
  # grid
  #  addPolygons(data = grid_master, group="grid",
  #            stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,fillColor = "red",) %>%
  
  # region boundaries
  #   addPolygons(data = shape, group="Ecoregions",
  #               stroke = TRUE, fillOpacity = 0, smoothFactor = 0.5, opacity = 0.5, weight = 1, color = "white") %>%
 
  # Layers control
#  addLayersControl(
#    overlayGroups = c("grid","Ecoregions"),
#    options = layersControlOptions(collapsed = FALSE)
#  )

