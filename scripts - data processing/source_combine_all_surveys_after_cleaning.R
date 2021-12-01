
# script to combine all surveys and get weigth cpue and weight corrected cpue per species; all in kg per km2
# last column shows whether the species is pelagic based on family-level classification using Watson species data table

library(dplyr)

#########################
# load Datras data #
######################## 
load("cleaned data/ICESsurveys10Aug_withq.RData")
datras <- survey3

# rename corrected data based on gear efficiency q's
colnames(datras)[which(names(datras)=="wgtlencpue_q")] <- "wtcpue_q" 
colnames(datras)[which(names(datras)=="wgtlenh_q")]    <- "wgth_q" 

# get for all unreported weights, weight based on length   
datras$wgth   <- ifelse(is.na(datras$wgth),datras$wgtlenh,datras$wgth)
datras$wtcpue <- ifelse(is.na(datras$wtcpue),datras$wgtlencpue,datras$wtcpue)

# remove hauls where cpue data is not available
noCpue <- subset(datras,datras$wgth >0 & is.na(datras$wtcpue))
noCpue <- unique(noCpue$HaulID)

datras <- datras %>%
  filter(!(HaulID %in% noCpue)) %>%
  dplyr::select(HaulID,Survey,Gear,Year,Month,ShootLong,ShootLat,Area.swept,Depth,Family,Species,wgth,wtcpue,wgth_q,wtcpue_q) 

datras <- subset(datras,!(datras$Survey == "Can-Mar"))

#########################
# load Norway data #
########################
load("cleaned data/NORBTS10Aug_withq.RData")
norw   <- norw_dat

# rename corrected data based on gear efficiency q's
colnames(norw)[which(names(norw)=="wgtlencpue_q")] <- "wtcpue_q" 
colnames(norw)[which(names(norw)=="wgtlenh_q")]    <- "wgth_q"

# get for all unreported weights, weight based on length   
norw$wgth   <- ifelse(is.na(norw$wgth),norw$wgtlenh,norw$wgth)
norw$wtcpue <- ifelse(is.na(norw$wtcpue),norw$wgtlencpue,norw$wtcpue)

# remove hauls where cpue data is not available
noCpue <- subset(norw,norw$wgth >0 & is.na(norw$wtcpue))
noCpue <- unique(noCpue$HaulID)

norw <- norw %>%
  filter(!(HaulID %in% noCpue)) %>%
  dplyr::select(HaulID,Survey,Gear,Year,Month,ShootLong,ShootLat,Area.swept,Depth,Family,Species,wgth,wtcpue,wgth_q,wtcpue_q) 

# combine datras with norw 
trawl <- rbind(datras,norw)

#########################
# load oceanadapt data #
########################
adapt <- readRDS("cleaned data/all-regions-full-oceanadapt.rds")
load("C:/Users/danie/Dropbox/Werk/Demersal fish and fisheries/Gear and catches/oceanadapt_species_qvalues.RData")

adapt <- cbind(adapt,gd_new[match(adapt$spp,gd_new$spec),c("family","Efficiency")])
adapt <- subset(adapt,!(is.na(adapt$Efficiency))) # all others are invertebrates
adapt$wtcpue_q <- adapt$wtcpue/adapt$Efficiency 

# convert all to kg/km2
adapt$wtcpue   <- adapt$wtcpue * 100 #kg/HA to kg/km2
adapt$wtcpue_q <- adapt$wtcpue_q * 100 #kg/HA to kg/km2

# get extra column (some/most columns can be included with real data, if needed)
adapt$gear <- NA
adapt$month <- NA
adapt$area_swept <- NA
adapt$wgth <- NA
adapt$wgth_q <- NA

adapt <- adapt %>%
  dplyr::select(haulid,region,gear,year,month,lon,lat,area_swept,depth,family,spp,wgth,wtcpue,wgth_q,wtcpue_q) 

# combine with other surveys
colnames(trawl) <- colnames(adapt)
trawl <- rbind(trawl,adapt)

# there are a few stations with INF numbers, remove
INF_nb <- subset(trawl,trawl$wtcpue_q > 10^100)

trawl <- trawl %>% 
  filter(!(haulid %in% unique(INF_nb$haulid))) %>%
  as.data.frame()

############## 
### get trait information
##############

# select "pelagic" fish 
pel_family <- c("Clupeidae" , "Osmeridae",  "Exocoetidae" , "Atherinidae" , "Engraulidae",
                "Hemiramphidae", "Inermiidae","Belonidae","Scomberesocidae", "Echeneidae",
                "Carangidae","Bramidae","Scombridae","Centrolophidae","Istiophoridae","Ammodytidae")

trawl$type <- ifelse(trawl$family %in% pel_family,"pel","dem")

# select "large" fish species -- Linf > 80 cm 
traits <- read.csv("traits and species/Beukhofetal_2019/Traits_fish.csv",header=T,sep=";",row.names=NULL)
traits_large <- subset(traits,traits$length.infinity > 80)
large <- unique(traits_large$taxon)
trawl$size <- ifelse(trawl$spp %in% large,"large","small")

trawl <- cbind(trawl,traits[match(trawl$spp,traits$taxon),c("tl")])
colnames(trawl)[ncol(trawl)] <- "tl"

###############
#### add depths for the hauls with no information using ETOPO ICE depth information
###############
load("cleaned data/Depth_hauls_NA.RData")
trawl <- cbind(trawl,depth_haul[match(trawl$haulid,depth_haul$haulid),c("depth")])
colnames(trawl)[ncol(trawl)] <- "depth2"
trawl$depth <- ifelse(is.na(trawl$depth),trawl$depth2,trawl$depth)
trawl <- trawl[,-ncol(trawl)] 

rm(pel_family,adapt,norw,noCpue,norw_dat,datras,survey3,gd_new,traits,traits_large,large,INF_nb,depth_haul)
