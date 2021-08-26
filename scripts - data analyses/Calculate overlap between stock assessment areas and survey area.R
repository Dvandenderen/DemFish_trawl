
rm(list=ls())

### load libraries
library(rgdal)
library(raster)
library(sp)
library(rgeos)
library(maptools)
library(dplyr)
library(sf)

###########
### load all survey data and create spatial object of the area surveyed
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
survey_area <- subset(bargrid,bargrid@data$uni %in% tr$uni)

# aggregate to one polygon
survey_area <- aggregate(survey_area)

###########
### load shapefiles from James Rising that match stock assessment area
###########
shape <- readOGR(dsn = paste(getwd(),'_noGIT/James Rising data/shapes',sep="") ,layer="ram")
crs(shape) <- crs(survey_area)
#plot(shape)

### load regions (IDs in shapefile match rownumber in csv file)
regions<-read.csv(paste(getwd(),'_noGIT/James Rising data/latlon.csv',sep = ""),header=T)
regions$SP_ID<-c(1:232)

# combine to get the region name -- note: the csv name order match the polygon order 
shape@data$Regions <- regions$name

###########
### examine for each stock assessment area the fraction of overlap with surveyed area
###########

# there are a few errors in the polygons, now using an old data cleaning script 
# (can probably be done better)
# note sf::st_make_valid does not solve all issues

# some polygons on land, calculate also that fraction
land <- rnaturalearth::ne_countries(scale = 110, returnclass = "sp")
land <- aggregate(land)

final<-as.data.frame(matrix(data=100,ncol=3,nrow=1))

for (j in 1:232){
  shape1 <- shape[shape$SP_ID ==j,]
  
  if (j==11){
    pls3<-Polygons(list(shape1@polygons[[1]]@Polygons[[2]],shape1@polygons[[1]]@Polygons[[3]],shape1@polygons[[1]]@Polygons[[4]]),10)
    sps = SpatialPolygons(list(pls3))
    proj4string(sps)<-CRS("+init=epsg:4326")
    slot(sps, "polygons") <- lapply(slot(sps, "polygons"), checkPolygonsHoles)
    shape1<-sps
  }
  
  if (j==36){
    shape1@polygons[[1]]@Polygons[[1]]@coords<-shape1@polygons[[1]]@Polygons[[1]]@coords[-c(16,40,45),]
    shape1@polygons[[1]]@Polygons[[4]]@coords<-shape1@polygons[[1]]@Polygons[[4]]@coords[-c(4),]
    pls3<-Polygons(list(shape1@polygons[[1]]@Polygons[[1]],shape1@polygons[[1]]@Polygons[[2]],
                        shape1@polygons[[1]]@Polygons[[3]],shape1@polygons[[1]]@Polygons[[4]]),35)
    sps = SpatialPolygons(list(pls3))
    proj4string(sps)<-CRS("+init=epsg:4326")
    slot(sps, "polygons") <- lapply(slot(sps, "polygons"), checkPolygonsHoles)
    shape1<-sps
  }
  
  if (j==45){
    shape1@polygons[[1]]@Polygons[[2]]@coords<-shape1@polygons[[1]]@Polygons[[2]]@coords[-c(2,4,6),]
    pls3<-Polygons(list(shape1@polygons[[1]]@Polygons[[1]],shape1@polygons[[1]]@Polygons[[2]]),44)
    sps = SpatialPolygons(list(pls3))
    proj4string(sps)<-CRS("+init=epsg:4326")
    slot(sps, "polygons") <- lapply(slot(sps, "polygons"), checkPolygonsHoles)
    shape1<-sps
  }
  
  if (j==63){
    pls3<-Polygons(list(shape1@polygons[[1]]@Polygons[[1]],shape1@polygons[[1]]@Polygons[[4]]),62)
    sps = SpatialPolygons(list(pls3))
    proj4string(sps)<-CRS("+init=epsg:4326")
    slot(sps, "polygons") <- lapply(slot(sps, "polygons"), checkPolygonsHoles)
    shape1<-sps
  }
  
  if (j==75){
    pls3<-Polygons(list(shape1@polygons[[1]]@Polygons[[1]]),74)
    sps = SpatialPolygons(list(pls3))
    proj4string(sps)<-CRS("+init=epsg:4326")
    slot(sps, "polygons") <- lapply(slot(sps, "polygons"), checkPolygonsHoles)
    shape1<-sps
  }
  
  if (j==129){
    shape1@polygons[[1]]@Polygons[[1]]@coords<-shape1@polygons[[1]]@Polygons[[1]]@coords[-c(124),]
    pls3<-Polygons(list(shape1@polygons[[1]]@Polygons[[1]],shape1@polygons[[1]]@Polygons[[2]]),128)
    sps = SpatialPolygons(list(pls3))
    proj4string(sps)<-CRS("+init=epsg:4326")
    slot(sps, "polygons") <- lapply(slot(sps, "polygons"), checkPolygonsHoles)
    shape1<-sps}
  
  if (j==130){
    shape1@polygons[[1]]@Polygons[[1]]@coords<-shape1@polygons[[1]]@Polygons[[1]]@coords[-c(29,71),]
    # shape1@polygons[[1]]@Polygons[[1]]@coords<-shape1@polygons[[1]]@Polygons[[4]]@coords[-c(4),]
    pls3<-Polygons(list(shape1@polygons[[1]]@Polygons[[1]],shape1@polygons[[1]]@Polygons[[3]],
                        shape1@polygons[[1]]@Polygons[[5]]),129)
    sps = SpatialPolygons(list(pls3))
    proj4string(sps)<-CRS("+init=epsg:4326")
    slot(sps, "polygons") <- lapply(slot(sps, "polygons"), checkPolygonsHoles)
    shape1<-sps
  }
  
  if (j==134){
    shape1@polygons[[1]]@Polygons[[1]]@coords<-shape1@polygons[[1]]@Polygons[[1]]@coords[-c(54),]
    pls3<-Polygons(list(shape1@polygons[[1]]@Polygons[[1]],shape1@polygons[[1]]@Polygons[[2]]),133)
    sps = SpatialPolygons(list(pls3))
    proj4string(sps)<-CRS("+init=epsg:4326")
    slot(sps, "polygons") <- lapply(slot(sps, "polygons"), checkPolygonsHoles)
    shape1<-sps
  }
  
  if (j==135){
    pls3<-Polygons(list(shape1@polygons[[1]]@Polygons[[1]],shape1@polygons[[1]]@Polygons[[3]],
                        shape1@polygons[[1]]@Polygons[[4]],shape1@polygons[[1]]@Polygons[[5]],
                        shape1@polygons[[1]]@Polygons[[6]],shape1@polygons[[1]]@Polygons[[7]],
                        shape1@polygons[[1]]@Polygons[[8]],shape1@polygons[[1]]@Polygons[[9]],
                        shape1@polygons[[1]]@Polygons[[11]]),134)
    sps = SpatialPolygons(list(pls3))
    proj4string(sps)<-CRS("+init=epsg:4326")
    slot(sps, "polygons") <- lapply(slot(sps, "polygons"), checkPolygonsHoles)
    shape1<-sps
  }
  
  if (j==138){
    shape1@polygons[[1]]@Polygons[[1]]@coords<-shape1@polygons[[1]]@Polygons[[1]]@coords[-c(39,143),]
    pls3<-Polygons(list(shape1@polygons[[1]]@Polygons[[1]]),137)
    sps = SpatialPolygons(list(pls3))
    proj4string(sps)<-CRS("+init=epsg:4326")
    slot(sps, "polygons") <- lapply(slot(sps, "polygons"), checkPolygonsHoles)
    shape1<-sps
  }
  
  if (j==142){
    shape1@polygons[[1]]@Polygons[[1]]@coords<-shape1@polygons[[1]]@Polygons[[1]]@coords[-c(117),]
    pls3<-Polygons(list(shape1@polygons[[1]]@Polygons[[1]]),141)
    sps = SpatialPolygons(list(pls3))
    proj4string(sps)<-CRS("+init=epsg:4326")
    slot(sps, "polygons") <- lapply(slot(sps, "polygons"), checkPolygonsHoles)
    shape1<-sps
  }
  
  if (j==144){
    shape1@polygons[[1]]@Polygons[[1]]@coords<-shape1@polygons[[1]]@Polygons[[1]]@coords[-c(306,521,523,566,583,590,719),]
    pls3<-Polygons(list(shape1@polygons[[1]]@Polygons[[1]],shape1@polygons[[1]]@Polygons[[4]],
                        shape1@polygons[[1]]@Polygons[[16]]),143)
    sps = SpatialPolygons(list(pls3))
    proj4string(sps)<-CRS("+init=epsg:4326")
    slot(sps, "polygons") <- lapply(slot(sps, "polygons"), checkPolygonsHoles)
    shape1<-sps
  }
  
  if (j==146){
    shape1@polygons[[1]]@Polygons[[2]]@coords<-shape1@polygons[[1]]@Polygons[[2]]@coords[-c(113:115,263),]
    pls3<-Polygons(list(shape1@polygons[[1]]@Polygons[[1]],shape1@polygons[[1]]@Polygons[[2]]),145)
    sps = SpatialPolygons(list(pls3))
    proj4string(sps)<-CRS("+init=epsg:4326")
    slot(sps, "polygons") <- lapply(slot(sps, "polygons"), checkPolygonsHoles)
    shape1<-sps
  }
  
  if (j==147){
    shape1@polygons[[1]]@Polygons[[1]]@coords<-shape1@polygons[[1]]@Polygons[[1]]@coords[-c(117),]
    pls3<-Polygons(list(shape1@polygons[[1]]@Polygons[[1]]),146)
    sps = SpatialPolygons(list(pls3))
    proj4string(sps)<-CRS("+init=epsg:4326")
    slot(sps, "polygons") <- lapply(slot(sps, "polygons"), checkPolygonsHoles)
    shape1<-sps
  }
  
  if (j==148){
    shape1@polygons[[1]]@Polygons[[1]]@coords<-shape1@polygons[[1]]@Polygons[[1]]@coords[-c(38:150),]
    pls3<-Polygons(list(shape1@polygons[[1]]@Polygons[[1]]),147)
    sps = SpatialPolygons(list(pls3))
    proj4string(sps)<-CRS("+init=epsg:4326")
    slot(sps, "polygons") <- lapply(slot(sps, "polygons"), checkPolygonsHoles)
    shape1<-sps
  }
  
  if (j==150){
    shape1@polygons[[1]]@Polygons[[2]]@coords<-shape1@polygons[[1]]@Polygons[[2]]@coords[-c(328,331),]
    pls3<-Polygons(list(shape1@polygons[[1]]@Polygons[[1]],shape1@polygons[[1]]@Polygons[[2]],
                        shape1@polygons[[1]]@Polygons[[7]]),149)
    sps = SpatialPolygons(list(pls3))
    proj4string(sps)<-CRS("+init=epsg:4326")
    slot(sps, "polygons") <- lapply(slot(sps, "polygons"), checkPolygonsHoles)
    shape1<-sps
  }
  
  if (j==151){
    shape1@polygons[[1]]@Polygons[[1]]@coords<-shape1@polygons[[1]]@Polygons[[1]]@coords[-c(14,20,21),]
    
    pls3<-Polygons(list(shape1@polygons[[1]]@Polygons[[1]],shape1@polygons[[1]]@Polygons[[2]],
                        shape1@polygons[[1]]@Polygons[[3]],shape1@polygons[[1]]@Polygons[[4]],
                        shape1@polygons[[1]]@Polygons[[5]],shape1@polygons[[1]]@Polygons[[6]]),150)
    sps = SpatialPolygons(list(pls3))
    proj4string(sps)<-CRS("+init=epsg:4326")
    slot(sps, "polygons") <- lapply(slot(sps, "polygons"), checkPolygonsHoles)
    shape1<-sps
  }
  
  if (j==156){
    shape1@polygons[[1]]@Polygons[[1]]@coords<-shape1@polygons[[1]]@Polygons[[1]]@coords[-c(26,27),]
    pls3<-Polygons(list(shape1@polygons[[1]]@Polygons[[1]]),155)
    sps = SpatialPolygons(list(pls3))
    proj4string(sps)<-CRS("+init=epsg:4326")
    slot(sps, "polygons") <- lapply(slot(sps, "polygons"), checkPolygonsHoles)
    shape1<-sps
  }
  
  if (j==197){
    shape1@polygons[[1]]@Polygons[[1]]@coords<-shape1@polygons[[1]]@Polygons[[1]]@coords[-c(45,46),]
    pls3<-Polygons(list(shape1@polygons[[1]]@Polygons[[1]]),196)
    sps = SpatialPolygons(list(pls3))
    proj4string(sps)<-CRS("+init=epsg:4326")
    slot(sps, "polygons") <- lapply(slot(sps, "polygons"), checkPolygonsHoles)
    shape1<-sps
  }
  
  if (j==198){
    pls3<-Polygons(list(shape1@polygons[[1]]@Polygons[[2]]),197)
    sps = SpatialPolygons(list(pls3))
    proj4string(sps)<-CRS("+init=epsg:4326")
    slot(sps, "polygons") <- lapply(slot(sps, "polygons"), checkPolygonsHoles)
    shape1<-sps
  }
  
  if (j==199){
    pls3<-Polygons(list(shape1@polygons[[1]]@Polygons[[1]],shape1@polygons[[1]]@Polygons[[3]]),198)
    sps = SpatialPolygons(list(pls3))
    proj4string(sps)<-CRS("+init=epsg:4326")
    slot(sps, "polygons") <- lapply(slot(sps, "polygons"), checkPolygonsHoles)
    shape1<-sps
  }
  
  if (j==42 | j== 46 | j==67 | j==68 | j==69 | j==70| j==74| j==114| j==139| j==140| j==141| j==143
      | j==149 | j==152| j==154| j==158| j==165| j==168| j==179| j==180| j==182){
    pls3<-shape1
    slot(pls3, "polygons") <- lapply(slot(pls3, "polygons"), checkPolygonsHoles)
    shape1<-pls3
  }
  
  pi<-gIntersection(shape1,survey_area,byid = TRUE)
  if(length(pi)>0){
    if (class(pi)=="SpatialPolygons"){
      frac  <-(area(pi) / 1000000)/(area(shape1) / 1000000)
      pi2<-gIntersection(shape1,land,byid = TRUE)
      frac2 <- ifelse(length(pi2) == 0, 0, (area(pi2) / 1000000)/(area(shape1) / 1000000))
      final<-rbind(final,c(j,frac,frac2))
    }}
  print(c(j))}

# few errors but okay as all in New Zealand (not needed anyway)

# rename columns
colnames(final)<-c("assess_area","fraction of assess_area in survey_area","fraction on land")
final<-final[-1,]
final[,2]<-replace(final[,2], final[,2]>1, 1)
final[,3]<-replace(final[,3], final[,3]>1, 1)

# and get assessment region
final<-cbind(final,regions[match(final[,"assess_area"],regions[,"SP_ID"]),c(1)])
colnames(final)[4]<-c("assess_area_name")

# check - looks good
#plot(shape[19,]) #94% overlap
#plot(survey_area,add=T,col="blue")
#plot(shape[19,],add=T,col="red")

save(final,file="cleaned data/overlap_stockass_survey_area.RData")

rm(list=setdiff(ls(), c("final","trawl")))




