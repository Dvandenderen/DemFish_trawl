
### script to obtain depths for all hauls where depth is not reported
# Bathymetric data (m) were extracted per 1/12° grid from the ETOPO1 Global
# Relief Model with sea ice cover

# citation: Amante, C. & Eakins, B. W. ETOPO1 1 Arc-Minute Global Relief Model:
# Procedures, Data Sources and Analysis NOAA Technical Memorandum
# NESDIS NGDC-24 (NOAA, Boulder, 2009)

## load data // dataset too large for github 
# data is list with 3 elements --> 
# [[1]] - vector of longitudes
# [[2]] - vector of latitudes
# [[3]] - matrix with depths[lat,long]

# select all hauls without depth info
trawl$depth <- ifelse(trawl$depth == -1, NA,trawl$depth)
nadepth <- subset(trawl,is.na(trawl$depth ))
tt <- nadepth[!duplicated(nadepth[ , c("haulid")]),]

# match with ETOPO information
for(i in 1:nrow(tt)){
  xlong <- which(abs(depth[[1]]-tt$lon[i])==min(abs(depth[[1]]-tt$lon[i])))[1]
  ylat <- which(abs(depth[[2]]-tt$lat[i])==min(abs(depth[[2]]-tt$lat[i])))[1]
  tt$depth[i] <- depth[[3]][ylat,xlong]
}

# save ETOPO depths
depth_haul <- tt[,c(1,9)]
depth_haul$depth <- abs(depth_haul$depth)
save(depth_haul,file="cleaned data/Depth_hauls_NA.RData")
