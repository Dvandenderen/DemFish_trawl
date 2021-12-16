
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

# load surveyed grid
load("cleaned data/surveyed_grid.RData")

# aggregate to one polygon
survey_area <- aggregate(grid_master)
survey_area <- st_as_sf(survey_area)

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
### examine for each stock assessment area the fraction of overlap with surveyed area in the ocean
###########

# get land and remove the land part
land <- rnaturalearth::ne_countries(scale = 110, returnclass = "sp")
land <- aggregate(land)
proj4string(land)<-CRS("+init=epsg:4326")
landsf <- st_as_sf(land)

final<-as.data.frame(matrix(data=100,ncol=2,nrow=1))

for (j in 1:232){
  shape1 <- shape[shape$SP_ID ==j,]
  
  # some polygons are excluded (give error) - some are split at -180 long (OK to remove) 
  if (!(j == 43| j== 45 | j== 46 | j== 114  | j== 132 | j== 138 | j== 144  | j== 149| j== 179| j== 180)){
  
  shape1 <- st_as_sf(shape1)
  shape1 <- st_make_valid(shape1)
  shape1 <- st_difference(shape1,landsf)
  
  pi<- st_intersection(shape1,survey_area)
  if(nrow(pi)>0){
    pi  <- st_make_valid(pi)  
    frac  <- st_area(pi) / st_area(shape1)
    final<-rbind(final,c(j,frac))
  }}
  print(c(j))
  }

# rename columns
colnames(final)<-c("assess_area","ocean fraction of assess_area in survey_area")
final<-final[-1,]

# and get assessment region
final<-cbind(final,regions[match(final[,"assess_area"],regions[,"SP_ID"]),c(1)])
colnames(final)[3]<-c("assess_area_name")

# check - looks good
#plot(shape[19,]) #87% overlap
#plot(survey_area,add=T,col="blue")
#plot(shape[19,],add=T,col="red")

save(final,file="cleaned data/overlap_stockass_survey_area.RData")

rm(list=setdiff(ls(), c("final","trawl")))




