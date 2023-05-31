
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

# assign area of interest - equal area size
  sfc = st_sfc(st_polygon(list(rbind(c(-180,-50), c(53,-50), c(53,90), c(-180,90), c(-180,-50)))))
  st_crs(sfc) = 4326
  sfc <- st_transform(sfc, crs=st_crs(5070))
  cellarea <- 6200 * (1e+6)
  cellsize <- 2 * sqrt(cellarea/((3*sqrt(3)/2))) * sqrt(3)/2
  hexa <- st_make_grid(x = sfc, cellsize = cellsize, square = FALSE)
  hexa <- st_transform(hexa, crs=st_crs(4326))
  hexa <- st_make_valid(hexa)
  coord <- coords %>%
        st_as_sf(coords = c("V1", "V2"), crs=4326)

# overlay grid with trawl coordinates
  newgrid     <- st_intersects(hexa,coord) 
  # warning is OK: some degrees > -180 for Aleutian Is. -- not included to keep it manageable
  overp       <- as.data.frame(newgrid)
  grid_master <- hexa[unique(overp$row.id)] 

# re-box grid to cut at -180 longitude - make sure it is well boxed
  box_grid <- st_sfc(st_polygon(list(rbind(c(-180,-50), c(55,-50), c(55,90), c(-180,90), c(-180,-50)))))
  st_crs(box_grid) = 4326
  grid_master <- sf::st_difference(x = grid_master , y = box_grid)
  grid_master <- as_Spatial(grid_master)

# get information per cell
  grid_master$uni_cell <- paste("ID",paste(round(coordinates(grid_master)[,1],digits = 2),
                                           round(coordinates(grid_master)[,2],digits=2),sep="_"),sep="_")
  grid_master$area_sqkm <- area(grid_master)/10^6

# get area of grid cell on land
  land <- rnaturalearth::ne_countries(scale = 10, returnclass = "sf")
  sf_grid_master <- st_as_sf(grid_master)
  tt2 <- st_intersection(sf_grid_master, land,) 
  land_over <- data.frame(uni = tt2$uni_cell, land_sqkm = st_area(tt2)) 
  land_over$land_sqkm <- land_over$land_sqkm / 10^6 # m2 to km2
  land_over$land_sqkm <- unclass(land_over$land_sqkm)
  grid_master <- cbind(grid_master,land_over[match(grid_master@data$uni,land_over$uni), c(2)])
  colnames(grid_master@data)[ncol(grid_master@data)] <- "land_sqkm"
  grid_master$land_sqkm <- ifelse(is.na(grid_master$land_sqkm), 0,grid_master$land_sqkm)

# get ocean area
  grid_master$ocean_sqkm <- grid_master$area_sqkm - grid_master$land_sqkm

# link the grid cells to ecoregions
  shape <- readOGR(dsn = "data/MEOW shapefiles" ,layer="meow_ecos")
  coord <-data.frame(Longitude = coordinates(grid_master)[,1], Latitude = coordinates(grid_master)[,2])
  coordinates(coord)<- ~ Longitude + Latitude  
  crs(coord) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  shape <- spTransform(shape,CRS(proj4string(coord))) # make it similar to bargrid
  shape@proj4string # check coordinates reference system again
  tr <- over(coord,shape)
  grid_master$ECO_CODE <- tr[,1]
  grid_master$ECO_REG  <- tr[,2]

# add some cells to other regions 
  grid_master@data$ECO_REG <- ifelse(grid_master@data$ECO_REG == "Southern Gulf of Mexico","Floridian",grid_master@data$ECO_REG)
  grid_master@data$ECO_REG <- ifelse(grid_master@data$ECO_REG == "White Sea","North and East Barents Sea",grid_master@data$ECO_REG)
  grid_master@data$ECO_REG <- ifelse(grid_master@data$uni_cell == "ID_-5.1_60.38","Celtic Seas",grid_master@data$ECO_REG)
  grid_master@data$ECO_REG <- ifelse(grid_master@data$uni_cell == "ID_-125.11_50.19","Puget Trough/Georgia Basin",grid_master@data$ECO_REG)
  grid_master@data$ECO_REG <- ifelse(grid_master@data$ECO_REG == "Southern Grand Banks - South Newfoundland","Gulf of St. Lawrence - Eastern Scotian Shelf",grid_master@data$ECO_REG)

# remove cells from isolated regions / NA regions
  grid_master <- subset(grid_master,!(grid_master@data$ECO_REG %in% c("Faroe Plateau", "North and East Iceland",
                                                                      "Puget Trough/Georgia Basin",
                                                                      "Northern Grand Banks - Southern Labrador")))
  grid_master <- subset(grid_master,!(is.na(grid_master@data$ECO_REG)))

# save grid
  save(grid_master,file="processed data/surveyed_grid.RData")

###########
###  now load Env conditions and link to surveyed grid
###########
# get all coords 1/12 grid (resolution of env conditions)
  lon <- seq(from = -180+1/12,to = 180-1/12,length.out = 2160)
  lat <- seq(from = 90-1/12,to = -90+1/12,length.out = 1080)
  coords <- merge(lon,lat)
  coords <- subset(coords,coords$x < 55)
  coords <- subset(coords,coords$y > 20)

  coord <-data.frame(Longitude = coords[,1], Latitude = coords[,2])
  coordinates(coord)<- ~ Longitude + Latitude  
  crs(coord) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  tr <- over(coord,grid_master)
  coords <- cbind(coords,tr)
  coords <- subset(coords,!(is.na(coords$uni_cell)))

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

  coords$Depth_map  <- NA
  coords$NPP        <- NA
  coords$SST        <- NA
  coords$chla       <- NA
  coords$chla_wiffs <- NA
  coords$maxchla    <- NA

  for(j in 1:nrow(coords)){
    idx_lon <-  which(coords[j,1] == lon)
    idx_lat <-  which(coords[j,2] == lat)
    coords$Depth_map[j] <- Depth_output[[3]][idx_lat,idx_lon]
    coords$NPP[j]   <- NPP_output[[3]][idx_lat,idx_lon]
    coords$SST[j]   <- sst_output[[3]][idx_lat,idx_lon]
    coords$chla[j] <- chl_output[[3]][idx_lat,idx_lon]
    coords$chla_wiffs[j] <- chlA_output[[3]][idx_lat,idx_lon]
    coords$maxchla[j] <- chlA_output[[5]][idx_lat,idx_lon]
    
  }

  tnew <- aggregate(list(coords$Depth,coords$SST,coords$NPP,coords$chla,coords$chla_wiffs,coords$maxchla),by=list(coords$uni_cell), FUN= mean,na.rm=T)
  colnames(tnew) <- c("uni_cell","Depth_map","SST","NPP","chla","chla_wiffs","maxchla")
  grid_master <- cbind(grid_master,tnew[match(grid_master@data$uni_cell,tnew$uni_cell), c(2:7)])

###########
### load model prediction from COBALT and bottom temperature from WOA
###########
  cobalt <- read.csv(paste(getwd(),"_noGIT/Environmental data/Cobalt/Cobalt_output.csv",sep=""),sep=",")
  cobalt$long <- ifelse(cobalt$long >180, cobalt$long - 360, cobalt$long)
  coord <-data.frame(Longitude = cobalt$long, Latitude = cobalt$lat)
  coordinates(coord)<- ~ Longitude + Latitude  
  crs(coord) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  cds <- coordinates(grid_master)

  grid_master$lz_prod <- NA
  grid_master$mz_prod <- NA
  grid_master$ben_prod <- NA
  grid_master$lz_bio <- NA
  grid_master$mz_bio <- NA
  grid_master$WOAbottomtemp <- NA

  p1 = st_as_sf(coord)           
  st_crs(p1) = 4326
  p1 <- st_transform(p1, 5880)
  
  for (j in 1:nrow(cds)){
    p2 = st_sfc(st_point(c(cds[j,1],cds[j,2])))
    st_crs(p2) = 4326
    p2 <- st_transform(p2, 5880)
  
    dist_matrix   <- st_distance(p2,p1) 
    idx <- which(dist_matrix[1,]==min(dist_matrix))
  
    grid_master$lz_prod[j] <- cobalt$lz_prod[idx]
    grid_master$mz_prod[j]   <- cobalt$mz_prod[idx]
    grid_master$ben_prod[j]   <- cobalt$ben_prod[idx]
    grid_master$lz_bio[j] <- cobalt$lz_bio[idx]
    grid_master$mz_bio[j] <- cobalt$mz_bio[idx]
    grid_master$WOAbottomtemp[j] <- cobalt$WOAbottomtemp[idx]
  }
    
  grid_master@data$long <- cds[,1]
  grid_master@data$lat <- cds[,2]
  
  save(grid_master,file="processed data/surveyed_grid.RData")

###########
### get subdivisions 
###########
  source("scripts - data analyses/Subdivision selection.R")
  save(grid_master,file="processed data/surveyed_grid.RData")

###########
### check the data
###########
 library(leaflet)
 library(htmlwidgets)

  subdiv <- unionSpatialPolygons(grid_master, grid_master$subdivision)
  EcoReg <- unionSpatialPolygons(grid_master, grid_master$ECO_REG)

  mfs <- leaflet() %>%
    addProviderTiles(providers$Esri.WorldImagery) %>%
    # grid
    addPolygons(data = grid_master, group="grid",
              stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,fillColor = "red",) %>%
    # region boundaries
     addPolygons(data = subdiv, group="subdiv",
                 stroke = TRUE, fillOpacity = 0, smoothFactor = 0.5, opacity = 0.5, weight = 1, color = "white") %>%
     addPolygons(data = EcoReg, group="ecoreg",
                stroke = TRUE, fillOpacity = 0, smoothFactor = 0.5, opacity = 0.5, weight = 1, color = "white") %>%
    # Layers control
    addLayersControl(overlayGroups = c("grid","subdiv","ecoreg"),options = layersControlOptions(collapsed = FALSE)
    )
  mfs