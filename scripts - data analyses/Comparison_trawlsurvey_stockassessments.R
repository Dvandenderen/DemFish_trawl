
library(ramlegacy)

# downloads current latest version 4.44 
# download_ramlegacy()

# find it 
#ram_dir(vers = "4.44")

# and add folder to specific location
ram <- readRDS(paste(getwd(),'_noGIT/ramlegacy/4.44/RLSADB v4.44/DB Files With Assessment Data/v4.44.rds',sep=""))

# get estimate of area overlap between stock assessments and survey area
load("cleaned data/overlap_stockass_survey_area.RData")
colnames(final) <- c("areanb","overlap","fr_land","name") # to simplify script

# now find all species that overlap a large part of their area in the ocean
final$ocean <- 1-final$fr_land
final$toanalyze <- final$ocean - final$overlap 
fin <- subset(final,final$toanalyze < 0.1)

# select all stocks
stock <- ram$stock
stock <- subset(stock,stock$areaid %in% fin$name)

# get their average biomass between 2005 and 2009
ramdata <- ram$timeseries_values_views
ramunits <- ram$timeseries_units_views
stock$SSB <- NA ;stock$SSB_units <- NA 
stock$TB  <- NA ;stock$TB_units <- NA 

for(j in 1:nrow(stock)){
  # get biomass
  spec <- subset(ramdata,ramdata$stockid == stock$stockid[j] & ramdata$year %in% c(2000:2014))
  stock$SSB[j] <- mean(spec$SSB,na.rm=T) 
  stock$TB[j]  <- mean(spec$TB,na.rm=T)
  
  # get units
  if (nrow(spec)>0){
  specunits <- subset(ramunits,ramunits$stockid == stock$stockid[j])
  stock$SSB_units[j] <- specunits$SSB
  stock$TB_units[j]  <- specunits$TB
}}

stock$SSB <- ifelse(stock$SSB_units =="relative", NA,stock$SSB) # all other values in metric tonnes (1000 kg)
stock$TB <- ifelse(stock$TB_units =="relative", NA,stock$TB) # all other values in metric tonnes (1000 kg)
stock$biomass <- ifelse(is.na(stock$TB),stock$SSB,stock$TB)
stock <- subset(stock,!(is.na(stock$biomass)))

#### now get one degrees grid for the surveyed area and calculate surface (minus land)

# get cleaned survey data
source("scripts - data processing/source_combine_all_surveys_after_cleaning.R")
trawl_backup <- trawl

# get unique location
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
colnames(bargrid@data)[1] <- "uni" 

coord <-data.frame(Longitude = coords[,1], Latitude = coords[,2])
coordinates(coord)<- ~ Longitude + Latitude  
crs(coord) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
bargrid <- spTransform(bargrid,CRS(proj4string(coord))) # make it similar to bargrid
bargrid@proj4string # check coordinates reference system again
tr <- over(coord,bargrid)
coords$one_degrees <- tr$uni

# select all with survey data
bargrid <- subset(bargrid,bargrid@data$uni %in% coords$one_degrees)
bargrid@data$kmsq <- area(bargrid)/1000000

land <- rnaturalearth::ne_countries(scale = 110, returnclass = "sp")
land <- aggregate(land)

pi<-gIntersection(bargrid,land,byid = TRUE)
res <- data.frame( ID = sapply(slot(pi, "polygons"), function(x) slot(x, "ID")), land = area(pi) / 1000000)
res$ID <- gsub("g", "", res$ID)
res$ID <- gsub(" 1", "", res$ID)

bargrid <- cbind(bargrid,res[match(bargrid$uni,res$ID),c(2)])
colnames(bargrid@data)[ncol(bargrid@data)] <- "land_kmsq"
bargrid@data$land_kmsq[is.na(bargrid@data$land_kmsq)] <- 0
bargrid@data$ocean_kmsq <- bargrid@data$kmsq - bargrid@data$land_kmsq

# select per species and assessment area the kg/km2, multiply with ocean area and sum over the assessment area
trawl <- subset(trawl, trawl$year %in% c(2000:2014))

# get coordinates
trawl$uniq <- paste(trawl$lon,trawl$lat,sep="_")
coords_uni <- unique(trawl$uniq)
t <- strsplit(coords_uni, "_")
coords<- matrix(unlist(t), ncol=2, byrow=TRUE)
coords <- as.data.frame(coords)
coords[,1] <- as.numeric(as.character(coords[,1]))
coords[,2] <- as.numeric(as.character(coords[,2]))
coords[,3] <- coords_uni
coord <-data.frame(Longitude = coords[,1], Latitude = coords[,2])
coordinates(coord)<- ~ Longitude + Latitude  
crs(coord) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
bargrid <- spTransform(bargrid,CRS(proj4string(coord))) # make it similar to bargrid

# open James Rising assessment polygons
shape <- readOGR(dsn = paste(getwd(),'_noGIT/James Rising data/shapes',sep="") ,layer="ram")
crs(shape) <- crs(bargrid)

### load regions (IDs in shapefile match rownumber in csv file)
regions<-read.csv(paste(getwd(),'_noGIT/James Rising data/latlon.csv',sep = ""),header=T)
regions$SP_ID<-c(1:232)

# combine to get the region name -- note: the csv name order match the polygon order 
shape@data$Regions <- regions$name

stock$wt <- NA
stock$wt_q <- NA

for (iStock in 1:nrow(stock)){
  reg <- bargrid
  coordmatch <- coords
  
  # get the grid cells that overlap with the assessment area
  ass_area <- stock$areaid[iStock]
  ass_nb   <- subset(regions,regions$name == ass_area)
  ass_nb   <- ass_nb$SP_ID
  source("scripts - data analyses/Clean_stock_assessment_polygons.R")
  pol <- shape1
  tt <- over(reg,pol)
  reg$tt <- tt$SP_ID
  reg <- subset(reg,reg@data$tt == ass_nb)
  
  # search the trawl coordinates that overlap with the grid cells
  tr <- over(coord,reg)
  coordmatch$one_degrees <- tr$uni
  coordmatch <- subset(coordmatch,!(is.na(coordmatch$one_degrees)))
  trawl_sub <- cbind(trawl,coordmatch[match(trawl$uniq,coordmatch$V3),c("one_degrees")])
  colnames(trawl_sub)[ncol(trawl_sub)] <- "one_degrees"
  trawl_sub <- subset(trawl_sub,!(is.na(trawl_sub$one_degrees)))
  
  # sum by haul and species
  trawl_sub <- trawl_sub %>% 
    group_by(haulid,year,one_degrees,spp) %>%
    summarize_at(.vars=c('wtcpue', 'wtcpue_q'), .funs = function(x) sum(x)) 
  
  # now get all unique hauls (zero fish should be part of the count)
  newdat <- data.frame(haulid = unique(trawl_sub$haulid))
  newdat <- cbind(newdat,trawl_sub[match(newdat$haulid,trawl_sub$haulid),c("year","one_degrees")])

  # now select the species and average kg/km2 per grid cell for each year
  trawl_sub <- subset(trawl_sub,trawl_sub$spp == stock$scientificname[iStock])
  
  newdat <- cbind(newdat,trawl_sub[match(newdat$haulid,trawl_sub$haulid),c("wtcpue","wtcpue_q")])
  newdat$wtcpue[is.na(newdat$wtcpue)] <- 0
  newdat$wtcpue_q[is.na(newdat$wtcpue_q)] <- 0
  
  newdat <- newdat %>% 
    group_by(year,one_degrees) %>%
    summarize_at(.vars=c('wtcpue', 'wtcpue_q'), .funs = function(x) mean(x)) %>%
    as.data.frame
    
  #now link to the ocean km2 area of the grid cell
  newdat <- cbind(newdat,reg@data[match(newdat$one_degrees,reg@data$uni),c("ocean_kmsq")])
  colnames(newdat)[ncol(newdat)] <- "ocean_kmsq"
  newdat$wt <- newdat$wtcpue * newdat$ocean_kmsq
  newdat$wt_q <- newdat$wtcpue_q * newdat$ocean_kmsq

  newdat <- newdat %>% 
    group_by(year) %>%
    summarize_at(.vars=c('wt', 'wt_q'), .funs = function(x) sum(x)) %>%
    as.data.frame
  
  stock$wt[iStock] <- mean(newdat$wt)/1000
  stock$wt_q[iStock] <-  mean(newdat$wt_q)/1000
}

stock <- subset(stock,stock$biomass > 2)
stock <- subset(stock,stock$wt_q < 10^100)

plot(log10(stock$biomass),log10(stock$wt_q),xlim=c(0,8),ylim=c(0,8))
abline(0,1)

mod1 <- lm(log10(stock$biomass)~ log10(stock$wt_q))
