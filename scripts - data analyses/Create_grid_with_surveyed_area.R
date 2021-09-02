
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
grid_master$land_sqkm <- 0

# get fraction per grid cell on land
land <- rnaturalearth::ne_countries(scale = 10, returnclass = "sf")
sf_grid_master <- st_as_sf(grid_master)
tt <- sf::st_intersects(sf_grid_master, land)
tt2 <- sf::st_area(sf::st_intersection(sf_grid_master, land))
land_over <- data.frame(as.data.frame(tt),tt2)
land_over <- aggregate(land_over$tt2,by=list(land_over$row.id),sum)
grid_master$land_sqkm[land_over$Group.1] <- land_over$x/10^6 # m2 to km2

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

