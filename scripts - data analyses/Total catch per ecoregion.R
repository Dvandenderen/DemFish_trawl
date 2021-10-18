
#########
### add fisheries landings information
########

# load Regs fisheries database (v4.0)
C8084 <- readRDS("C:/Users/danie/Dropbox/Werk/Demersal fish and fisheries/Data analysis/Catch_all_1980_1984.rds")
C8589 <- readRDS("C:/Users/danie/Dropbox/Werk/Demersal fish and fisheries/Data analysis/Catch_all_1985_1989.rds")
C9094 <- readRDS("C:/Users/danie/Dropbox/Werk/Demersal fish and fisheries/Data analysis/Catch_all_1990_1994.rds")
C9599 <- readRDS("C:/Users/danie/Dropbox/Werk/Demersal fish and fisheries/Data analysis/Catch_all_1995_1999.rds")
C0004 <- readRDS("C:/Users/danie/Dropbox/Werk/Demersal fish and fisheries/Data analysis/Catch_all_2000_2004.rds")
C0509 <- readRDS("C:/Users/danie/Dropbox/Werk/Demersal fish and fisheries/Data analysis/Catch_all_2005_2009.rds")
C1014 <- readRDS("C:/Users/danie/Dropbox/Werk/Demersal fish and fisheries/Data analysis/Catch_all_2010_2014.rds")
C1515 <- readRDS("C:/Users/danie/Dropbox/Werk/Demersal fish and fisheries/Data analysis/Catch_all_2015_2015.rds")

### total catch for all fish
Catch <- rbind(C8084,C8589,C9094,C9599,C0004,C0509,C1014,C1515)
Catch$group <- "other"
Catch$group <- ifelse(Catch$Funcgroup %in% c(4,5,6,10:24), "Dem",Catch$group)
Catch$group <- ifelse(Catch$Funcgroup %in% c(1:2), "Pel",Catch$group)
Catch$group <- ifelse(Catch$Funcgroup %in% c(3), "LPel",Catch$group)
Catch$Tot <- Catch$Reported + Catch$IUU + Catch$Discards
Catch <- aggregate(Catch$Tot, by= list(Catch$Cell,Catch$IYear,Catch$group),FUN= sum,na.rm=T)
colnames(Catch) <- c("Cell","Year","Group","catch")
Catch$uni <- paste(Catch$Cell,Catch$Year)

load("C:/Users/danie/Dropbox/Werk/Demersal fish and fisheries/Data analysis/Cells_in_EcoReg.RData")
Catch <- subset(Catch, Catch$Cell %in% subcells$Cell)
Catch <- cbind(Catch, subcells[match(Catch$Cell,subcells$Cell), c(4:6)]) 
Catch <- as.data.frame(Catch)
Fisheries <- aggregate(Catch$catch, by=list(Catch$ECO_REG,Catch$Year,Catch$Group),FUN= sum)
colnames(Fisheries) <- c("Region","Year","Group","catch")

regions <- c("North and East Barents Sea" , "Northern Norway and Finnmark", "Southern Norway",                             
              "Eastern Bering Sea" , "North Sea"  , "Gulf of Alaska" , "Celtic Seas"  ,  "Baltic Sea",
             "North American Pacific Fijordland", "Aleutian Islands" , "Gulf of St. Lawrence - Eastern Scotian Shelf" ,
             "Oregon, Washington, Vancouver Coast and Shelf", "South European Atlantic Shelf"  , "Gulf of Maine/Bay of Fundy" , "Scotian Shelf",                                
             "Virginian" ,    "Northern California"  , "Carolinian" , "Southern California Bight" , "Northern Gulf of Mexico",
             "Floridian")                                    
Fisheries <- subset(Fisheries,Fisheries$Region %in% regions)

tt <- Fisheries
tt <- subset(tt,tt$Year %in% c(2000:2010))
tt <- aggregate(tt$catch, by=list(tt$Region,tt$Group),FUN= mean)

ttfin <- subset(tt,tt$Group.2 =="Dem")
ttpel <- subset(tt,tt$Group.2 =="Pel")
ttLpel <- subset(tt,tt$Group.2 =="LPel")
ttot <- subset(tt,tt$Group.2 =="other")

ttfin <- cbind(ttfin, ttpel[match(ttfin$Group.1,ttpel$Group.1), c(3)]) 
ttfin <- cbind(ttfin, ttLpel[match(ttfin$Group.1,ttLpel$Group.1), c(3)])   
ttfin <- cbind(ttfin, ttot[match(ttfin$Group.1,ttot$Group.1), c(3)])   
ttfin <- ttfin[,-2]
colnames(ttfin) <- c("Region","Dem","Pel","LPel","other")
ttfin$tot <- ttfin$Dem + ttfin$Pel + ttfin$other + ttfin$LPel

load("C:/Users/danie/Desktop/dataRegions.Rdata")
load("C:/Users/danie/Desktop/dataRegions_peldem.Rdata")
colnames(peldem) <- c("EcReg","bio_pd","catch_pd","tl_pd","ER_pd")

ttfin <- cbind(ttfin, timeser[match(ttfin$Region,timeser$EcReg), c(4:19)])   

ttfin <- cbind(ttfin, peldem[match(ttfin$Region,peldem$EcReg), c(2:5)]) 
