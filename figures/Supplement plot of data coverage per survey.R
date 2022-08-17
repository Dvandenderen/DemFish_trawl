# make supplement plot of data coverage per survey

adapt <- readRDS("cleaned data/all-regions-full-oceanadapt.rds")

library(ggplot2)
ctrys <- rnaturalearth::ne_countries(scale = 50, returnclass = "sf")

figmap <- ggplot() + geom_sf(data = ctrys, fill="grey",colour=NA) + 
  theme(plot.background=element_blank(),
        panel.background=element_blank(),
        axis.text.y   = element_text(size=9),
        axis.text.x   = element_text(size=9),
        axis.title.y  = element_blank(),
        axis.title.x  = element_blank(),
        panel.border  = element_rect(colour = "grey", size=.5,fill=NA),
        legend.text   = element_text(size=11),
        legend.title  = element_text(size=11))

dat <- subset(adapt,adapt$region %in% c("Aleutian Islands")) 
dat <- dat[!duplicated(dat$haulid), ]

minlong <- min(dat$lon)
maxlong <- max(dat$lon)+5
minlat  <- min(dat$lat)-5
maxlat  <- max(dat$lat)+5
coordslim <- c(minlong,maxlong,minlat,maxlat)
coordxmap <- c(-179,-170,-160)
coordymap <- round(seq(minlat,maxlat,length.out = 4))

 figmap +  coord_sf(xlim = c(minlong, maxlong), ylim = c(minlat, maxlat)) +
  scale_x_continuous(breaks=coordxmap)  +
  scale_y_continuous(breaks=coordymap) + geom_point(data=dat,aes(x=lon,y=lat),size=0.25) 
 
## Canadian Pacific
 dat <- subset(adapt,adapt$region %in% c("Canadian Pacific")) 
 dat <- dat[!duplicated(dat$haulid), ]
 
 minlong <- min(dat$lon)-5
 maxlong <- max(dat$lon)+5
 minlat  <- min(dat$lat)-5
 maxlat  <- max(dat$lat)+5
 coordslim <- c(minlong,maxlong,minlat,maxlat)
 coordxmap <- round(seq(minlong,maxlong,length.out = 4))
 coordymap <- round(seq(minlat,maxlat,length.out = 4))

 figmap +  coord_sf(xlim = c(minlong, maxlong), ylim = c(minlat, maxlat)) +
   scale_x_continuous(breaks=coordxmap)  +
   scale_y_continuous(breaks=coordymap) + geom_point(data=dat,aes(x=lon,y=lat),size=0.25) 

 ## Eastern Bering Sea
 dat <- subset(adapt,adapt$region %in% c("Eastern Bering Sea")) 
 dat <- dat[!duplicated(dat$haulid), ]
 
 minlong <- min(dat$lon)-5
 maxlong <- max(dat$lon)+5
 minlat  <- min(dat$lat)-5
 maxlat  <- max(dat$lat)+5
 coordslim <- c(minlong,maxlong,minlat,maxlat)
 coordxmap <- round(seq(minlong,maxlong,length.out = 4))
 coordymap <- round(seq(minlat,maxlat,length.out = 4))
 
 figmap +  coord_sf(xlim = c(minlong, maxlong), ylim = c(minlat, maxlat))  +
      scale_x_continuous(breaks=coordxmap)  +
   scale_y_continuous(breaks=coordymap) + geom_point(data=dat,aes(x=lon,y=lat),size=0.25)  
 
 ## Gulf of Alaska
 dat <- subset(adapt,adapt$region %in% c("Gulf of Alaska")) 
 dat <- dat[!duplicated(dat$haulid), ]
 
 minlong <- min(dat$lon)-5
 maxlong <- max(dat$lon)+5
 minlat  <- min(dat$lat)-5
 maxlat  <- max(dat$lat)+5
 coordslim <- c(minlong,maxlong,minlat,maxlat)
 coordxmap <- round(seq(minlong,maxlong,length.out = 4))
 coordymap <- round(seq(minlat,maxlat,length.out = 4))
 
 figmap +  coord_sf(xlim = c(minlong, maxlong), ylim = c(minlat, maxlat))  +
   scale_x_continuous(breaks=coordxmap)  +
   scale_y_continuous(breaks=coordymap) + geom_point(data=dat,aes(x=lon,y=lat),size=0.25)  
 
 ## West coast US
 dat <- subset(adapt,adapt$region %in% c("West Coast Annual","West Coast Triennial")) 
 dat <- dat[!duplicated(dat$haulid), ]
 
 minlong <- min(dat$lon)-5
 maxlong <- max(dat$lon)+5
 minlat  <- min(dat$lat)-5
 maxlat  <- max(dat$lat)+5
 coordslim <- c(minlong,maxlong,minlat,maxlat)
 coordxmap <- round(seq(minlong,maxlong,length.out = 4))
 coordymap <- round(seq(minlat,maxlat,length.out = 4))
 
 figmap +  coord_sf(xlim = c(minlong, maxlong), ylim = c(minlat, maxlat))  +
   scale_x_continuous(breaks=coordxmap)  +
   scale_y_continuous(breaks=coordymap) + geom_point(data=dat,aes(x=lon,y=lat),size=0.25)  
 
 ## Gulf of St. Lawrence North
 dat <- subset(adapt,adapt$region %in% c("Gulf of St. Lawrence North")) 
 dat <- dat[!duplicated(dat$haulid), ]
 
 minlong <- min(dat$lon)-5
 maxlong <- max(dat$lon)+5
 minlat  <- min(dat$lat)-5
 maxlat  <- max(dat$lat)+5
 coordslim <- c(minlong,maxlong,minlat,maxlat)
 coordxmap <- round(seq(minlong,maxlong,length.out = 4))
 coordymap <- round(seq(minlat,maxlat,length.out = 4))
 
 figmap +  coord_sf(xlim = c(minlong, maxlong), ylim = c(minlat, maxlat))  +
   scale_x_continuous(breaks=coordxmap)  +
   scale_y_continuous(breaks=coordymap) + geom_point(data=dat,aes(x=lon,y=lat),size=0.25)  
 
 ## Gulf of St. Lawrence South
 dat <- subset(adapt,adapt$region %in% c("Gulf of St. Lawrence South")) 
 dat <- dat[!duplicated(dat$haulid), ]
 
 minlong <- min(dat$lon)-5
 maxlong <- max(dat$lon)+5
 minlat  <- min(dat$lat)-5
 maxlat  <- max(dat$lat)+5
 coordslim <- c(minlong,maxlong,minlat,maxlat)
 coordxmap <- round(seq(minlong,maxlong,length.out = 4))
 coordymap <- round(seq(minlat,maxlat,length.out = 4))
 
 figmap +  coord_sf(xlim = c(minlong, maxlong), ylim = c(minlat, maxlat))  +
   scale_x_continuous(breaks=coordxmap)  +
   scale_y_continuous(breaks=coordymap) + geom_point(data=dat,aes(x=lon,y=lat),size=0.25)  
 
 ## Maritimes Summer
 dat <- subset(adapt,adapt$region %in% c("Maritimes Summer")) 
 dat <- dat[!duplicated(dat$haulid), ]
 
 minlong <- min(dat$lon)-5
 maxlong <- max(dat$lon)+5
 minlat  <- min(dat$lat)-5
 maxlat  <- max(dat$lat)+5
 coordslim <- c(minlong,maxlong,minlat,maxlat)
 coordxmap <- round(seq(minlong,maxlong,length.out = 4))
 coordymap <- round(seq(minlat,maxlat,length.out = 4))
 
 figmap +  coord_sf(xlim = c(minlong, maxlong), ylim = c(minlat, maxlat))  +
   scale_x_continuous(breaks=coordxmap)  +
   scale_y_continuous(breaks=coordymap) + geom_point(data=dat,aes(x=lon,y=lat),size=0.25)  
 
 ## Northeast US 
 dat <- subset(adapt,adapt$region %in% c("Northeast US Fall","Northeast US Spring")) 
 dat <- dat[!duplicated(dat$haulid), ]
 
 minlong <- min(dat$lon)-5
 maxlong <- max(dat$lon)+5
 minlat  <- min(dat$lat)-5
 maxlat  <- max(dat$lat)+5
 coordslim <- c(minlong,maxlong,minlat,maxlat)
 coordxmap <- round(seq(minlong,maxlong,length.out = 4))
 coordymap <- round(seq(minlat,maxlat,length.out = 4))
 
 figmap +  coord_sf(xlim = c(minlong, maxlong), ylim = c(minlat, maxlat))  +
   scale_x_continuous(breaks=coordxmap)  +
   scale_y_continuous(breaks=coordymap) + geom_point(data=dat,aes(x=lon,y=lat),size=0.25)
 
 ## Southeast US 
 dat <- subset(adapt,adapt$region %in% c("Southeast US Spring","Southeast US Fall","Southeast US Summer")) 
 dat <- dat[!duplicated(dat$haulid), ]
 
 minlong <- min(dat$lon)-5
 maxlong <- max(dat$lon)+5
 minlat  <- min(dat$lat)-5
 maxlat  <- max(dat$lat)+5
 coordslim <- c(minlong,maxlong,minlat,maxlat)
 coordxmap <- round(seq(minlong,maxlong,length.out = 4))
 coordymap <- round(seq(minlat,maxlat,length.out = 4))
 
 figmap +  coord_sf(xlim = c(minlong, maxlong), ylim = c(minlat, maxlat))  +
   scale_x_continuous(breaks=coordxmap)  +
   scale_y_continuous(breaks=coordymap) + geom_point(data=dat,aes(x=lon,y=lat),size=0.25)
 
 ## Gulf of Mexico
 dat <- subset(adapt,adapt$region %in% c("Gulf of Mexico")) 
 dat <- dat[!duplicated(dat$haulid), ]
 
 minlong <- min(dat$lon)-5
 maxlong <- max(dat$lon)+5
 minlat  <- min(dat$lat)-5
 maxlat  <- max(dat$lat)+5
 coordslim <- c(minlong,maxlong,minlat,maxlat)
 coordxmap <- round(seq(minlong,maxlong,length.out = 4))
 coordymap <- round(seq(minlat,maxlat,length.out = 4))
 
 figmap +  coord_sf(xlim = c(minlong, maxlong), ylim = c(minlat, maxlat))  +
   scale_x_continuous(breaks=coordxmap)  +
   scale_y_continuous(breaks=coordymap) + geom_point(data=dat,aes(x=lon,y=lat),size=0.25)
 
 #### now get the ICES surveys
 load("cleaned data/ICESsurveys10Aug_withq.RData")

 dat <- subset(survey3,survey3$Survey %in% c("NS-IBTS")) 
 dat <- dat[!duplicated(dat$HaulID), ]
 
 minlong <- min(dat$ShootLong)-5
 maxlong <- max(dat$ShootLong)+5
 minlat  <- min(dat$ShootLat)-5
 maxlat  <- max(dat$ShootLat)+5
 coordslim <- c(minlong,maxlong,minlat,maxlat)
 coordxmap <- round(seq(minlong,maxlong,length.out = 4))
 coordymap <- round(seq(minlat,maxlat,length.out = 4))
 
 figmap +  coord_sf(xlim = c(minlong, maxlong), ylim = c(minlat, maxlat)) +
    scale_x_continuous(breaks=coordxmap)  +
    scale_y_continuous(breaks=coordymap) + geom_point(data=dat,aes(x=ShootLong,y=ShootLat),size=0.25) 
 
 ### 
 dat <- subset(survey3,survey3$Survey %in% c("FR-CGFS")) 
 dat <- dat[!duplicated(dat$HaulID), ]
 
 minlong <- min(dat$ShootLong)-5
 maxlong <- max(dat$ShootLong)+5
 minlat  <- min(dat$ShootLat)-5
 maxlat  <- max(dat$ShootLat)+5
 coordslim <- c(minlong,maxlong,minlat,maxlat)
 coordxmap <- round(seq(minlong,maxlong,length.out = 4))
 coordymap <- round(seq(minlat,maxlat,length.out = 4))
 
 figmap +  coord_sf(xlim = c(minlong, maxlong), ylim = c(minlat, maxlat)) +
    scale_x_continuous(breaks=coordxmap)  +
    scale_y_continuous(breaks=coordymap) + geom_point(data=dat,aes(x=ShootLong,y=ShootLat),size=0.25) 
 
 ### 
 dat <- subset(survey3,survey3$Survey %in% c("BITS")) 
 dat <- dat[!duplicated(dat$HaulID), ]
 
 minlong <- min(dat$ShootLong)-5
 maxlong <- max(dat$ShootLong)+5
 minlat  <- min(dat$ShootLat)-5
 maxlat  <- max(dat$ShootLat)+5
 coordslim <- c(minlong,maxlong,minlat,maxlat)
 coordxmap <- round(seq(minlong,maxlong,length.out = 4))
 coordymap <- round(seq(minlat,maxlat,length.out = 4))
 
 figmap +  coord_sf(xlim = c(minlong, maxlong), ylim = c(minlat, maxlat)) +
    scale_x_continuous(breaks=coordxmap)  +
    scale_y_continuous(breaks=coordymap) + geom_point(data=dat,aes(x=ShootLong,y=ShootLat),size=0.25) 
 
 ### 
 dat <- subset(survey3,survey3$Survey %in% c("IE-IGFS","NIGFS","SWC-IBTS","ROCKALL")) 
 dat <- dat[!duplicated(dat$HaulID), ]
 
 minlong <- min(dat$ShootLong)-5
 maxlong <- max(dat$ShootLong)+5
 minlat  <- min(dat$ShootLat)-5
 maxlat  <- max(dat$ShootLat)+5
 coordslim <- c(minlong,maxlong,minlat,maxlat)
 coordxmap <- round(seq(minlong,maxlong,length.out = 4))
 coordymap <- round(seq(minlat,maxlat,length.out = 4))
 
 figmap +  coord_sf(xlim = c(minlong, maxlong), ylim = c(minlat, maxlat)) +
    scale_x_continuous(breaks=coordxmap)  +
    scale_y_continuous(breaks=coordymap) + geom_point(data=dat,aes(x=ShootLong,y=ShootLat),size=0.25) 
 
 ### 
 dat <- subset(survey3,survey3$Survey %in% c("EVHOE")) 
 dat <- dat[!duplicated(dat$HaulID), ]
 
 minlong <- min(dat$ShootLong)-5
 maxlong <- max(dat$ShootLong)+5
 minlat  <- min(dat$ShootLat)-5
 maxlat  <- max(dat$ShootLat)+5
 coordslim <- c(minlong,maxlong,minlat,maxlat)
 coordxmap <- round(seq(minlong,maxlong,length.out = 4))
 coordymap <- round(seq(minlat,maxlat,length.out = 4))
 
 figmap +  coord_sf(xlim = c(minlong, maxlong), ylim = c(minlat, maxlat)) +
    scale_x_continuous(breaks=coordxmap)  +
    scale_y_continuous(breaks=coordymap) + geom_point(data=dat,aes(x=ShootLong,y=ShootLat),size=0.25) 
 
 ### 
 dat <- subset(survey3,survey3$Survey %in% c("PT-IBTS")) 
 dat <- dat[!duplicated(dat$HaulID), ]
 
 minlong <- min(dat$ShootLong)-5
 maxlong <- max(dat$ShootLong)+5
 minlat  <- min(dat$ShootLat)-5
 maxlat  <- max(dat$ShootLat)+5
 coordslim <- c(minlong,maxlong,minlat,maxlat)
 coordxmap <- round(seq(minlong,maxlong,length.out = 4))
 coordymap <- round(seq(minlat,maxlat,length.out = 4))
 
 figmap +  coord_sf(xlim = c(minlong, maxlong), ylim = c(minlat, maxlat)) +
    scale_x_continuous(breaks=coordxmap)  +
    scale_y_continuous(breaks=coordymap) + geom_point(data=dat,aes(x=ShootLong,y=ShootLat),size=0.25) 
 
 #### now get norwegian surveys
 load("cleaned data/NORBTSJuly2022_withq.RData")
 norw   <- norw_dat
 
 # rename corrected data based on gear efficiency q's
 colnames(norw)[which(names(norw)=="wgtlencpue_q")] <- "wtcpue_q" 
 colnames(norw)[which(names(norw)=="wgtlenh_q")]    <- "wgth_q"
 
 # get for all unreported weights, weight based on length
 # (not used in the analysis as it based on q corrected weights following length-classes
 norw$wgth   <- ifelse(is.na(norw$wgth),norw$wgtlenh,norw$wgth)
 norw$wtcpue <- ifelse(is.na(norw$wtcpue),norw$wgtlencpue,norw$wtcpue)
 
 # some species had no length information but wtcpue is available
 geareff <- read.csv(file = "data/Walkeretal_2017_supp/EfficiencyTab.csv",sep=",",header = T)
 norw <- cbind(norw,geareff[match(norw$Code,geareff$Code),c("Group")])
 norw$Code <- norw[,ncol(norw)]; norw <- norw[,-(ncol(norw))]
 
 norw$wtcpue_q <- ifelse(is.na(norw$wtcpue_q) & norw$Code =="GRP1", norw$wtcpue /0.01856618,norw$wtcpue_q)
 norw$wtcpue_q <- ifelse(is.na(norw$wtcpue_q) & norw$Code =="GRP2", norw$wtcpue /0.22182677,norw$wtcpue_q)
 norw$wtcpue_q <- ifelse(is.na(norw$wtcpue_q) & norw$Code =="GRP3", norw$wtcpue /0.29919480,norw$wtcpue_q)
 norw$wtcpue_q <- ifelse(is.na(norw$wtcpue_q) & norw$Code =="GRP4", norw$wtcpue /0.39701479,norw$wtcpue_q)
 norw$wtcpue_q <- ifelse(is.na(norw$wtcpue_q) & norw$Code =="GRP5", norw$wtcpue /0.25488862,norw$wtcpue_q)
 norw$wtcpue_q <- ifelse(is.na(norw$wtcpue_q) & norw$Code =="GRP6", norw$wtcpue /0.22871213,norw$wtcpue_q)
 norw$wtcpue_q <- ifelse(is.na(norw$wtcpue_q) & norw$Code =="GRP7", norw$wtcpue /0.30596754,norw$wtcpue_q)
 
 # remove hauls where cpue data is not available (note: many hauls before 1989)
 noCpue <- subset(norw,norw$wgth >0 & is.na(norw$wtcpue))
 noCpue <- unique(noCpue$HaulID)
 
 # remove all within shallow North Sea - already (partly) included in DATRAS
 norinNS <- subset(norw,norw$ShootLat < 61.89 & norw$Depth <200)
 norinNS <- unique(norinNS$HaulID)
 
 norw <- norw %>%
   filter(!(HaulID %in% c(noCpue,norinNS))) %>%
   dplyr::select(HaulID,Survey,Gear,Year,Month,ShootLong,ShootLat,Area.swept,Depth,Family,Species,wgth,wtcpue,wgth_q,wtcpue_q) 
 
 
 minlong <- min(norw$ShootLong)-5
 maxlong <- max(norw$ShootLong)+5
 minlat  <- min(norw$ShootLat)-5
 maxlat  <- max(norw$ShootLat)+5
 coordslim <- c(minlong,maxlong,minlat,maxlat)
 coordxmap <- round(seq(minlong,maxlong,length.out = 4))
 coordymap <- round(seq(minlat,maxlat,length.out = 4))
 
 figmap +  coord_sf(xlim = c(minlong, maxlong), ylim = c(minlat, maxlat)) +
    scale_x_continuous(breaks=coordxmap)  +
    scale_y_continuous(breaks=coordymap) + geom_point(data=norw,aes(x=ShootLong,y=ShootLat),size=0.25) 
 