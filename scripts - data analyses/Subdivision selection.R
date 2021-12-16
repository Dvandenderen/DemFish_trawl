
load("cleaned data/surveyed_grid.RData")

xdat <- data.frame(grid_master@data$uni_cell,grid_master@data$ECO_REG,coordinates(grid_master))
colnames(xdat) <- c("uni","ECO_REG","long","lat")

pac <- subset(xdat,xdat$long < -100)
plot(pac$long,pac$lat ,col="blue")

conception <- subset(pac,pac$lat > 31.5 & pac$lat <36)
points(conception$long,conception$lat ,col="red")

Monterey <- subset(pac,pac$lat > 36 & pac$lat <40.3)
points(Monterey$long,Monterey$lat ,col="red")

Eureka <- subset(pac,pac$lat > 40.3 & pac$lat < 43)
points(Eureka$long,Eureka$lat ,col="red")

Columbia <- subset(pac,pac$lat > 43 & pac$lat < 47.3)
points(Columbia$long,Columbia$lat ,col="red")

Vancouver <- subset(pac,pac$lat > 47.3 & pac$lat < 50.3)
points(Vancouver$long,Vancouver$lat ,col="red")

Charlotte <- subset(pac,pac$lat > 50.3 & pac$lat < 54.2 & pac$long > -140)
points(Charlotte$long,Charlotte$lat ,col="red")

Southeastern <- subset(pac,pac$lat > 54.2 & pac$lat < 60 & pac$long > -137)
points(Southeastern$long,Southeastern$lat ,col="red")

Yakutat <- subset(pac,pac$lat > 54.3 & pac$lat < 61 & pac$long < -137 & pac$long > -147)
points(Yakutat$long,Yakutat$lat ,col="red")

Kodiak <- subset(pac,pac$lat > 54.3 & pac$lat < 61 & pac$long < -147 & pac$long > -154)
points(Kodiak$long,Kodiak$lat ,col="red")

Chirikof <- subset(pac,pac$lat > 51.3 & pac$lat < 61 & pac$long < -154 & pac$long > -159 & pac$ECO_REG =="Gulf of Alaska")
points(Chirikof$long,Chirikof$lat ,col="red")

Shumagin <- subset(pac,pac$lat > 50 & pac$lat < 61 & pac$long < -159 & pac$long > -170 & 
                     pac$ECO_REG %in% c("Gulf of Alaska","Aleutian Islands" ))
points(Shumagin$long,Shumagin$lat ,col="red")

Alislands <- subset(pac,pac$lat > 50 & pac$lat < 61 & pac$long < -170 & pac$long > -180 & 
                     pac$ECO_REG %in% c("Gulf of Alaska","Aleutian Islands" ))
points(Alislands$long,Alislands$lat ,col="red")

EBS1 <- subset(pac,pac$lat > 50 & pac$lat < 65  &  pac$long > -164 &
                 pac$ECO_REG %in% c("Eastern Bering Sea" ))
points(EBS1$long,EBS1$lat ,col="green")

EBS2 <- subset(pac,pac$lat > 50 & pac$lat < 65  &  pac$long < -164 &  pac$long  > -170 &
                 pac$ECO_REG %in% c("Eastern Bering Sea" ))
points(EBS2$long,EBS2$lat ,col="orange")

EBS3 <- subset(pac,pac$lat > 50 & pac$lat < 65  &  pac$long < -170 &
                 pac$ECO_REG %in% c("Eastern Bering Sea" ))
points(EBS3$long,EBS3$lat ,col="pink")

grid_master@data$subdivision <- NA
grid_master@data$subdivision <- ifelse(grid_master@data$uni %in% conception$uni,"Conception",grid_master@data$subdivision)
grid_master@data$subdivision <- ifelse(grid_master@data$uni %in% Monterey$uni,"Monterey",grid_master@data$subdivision)
grid_master@data$subdivision <- ifelse(grid_master@data$uni %in% Eureka$uni,"Eureka",grid_master@data$subdivision)
grid_master@data$subdivision <- ifelse(grid_master@data$uni %in% Columbia$uni,"Columbia",grid_master@data$subdivision)
grid_master@data$subdivision <- ifelse(grid_master@data$uni %in% Charlotte$uni,"Charlotte",grid_master@data$subdivision)
grid_master@data$subdivision <- ifelse(grid_master@data$uni %in% Vancouver$uni,"Vancouver",grid_master@data$subdivision)
grid_master@data$subdivision <- ifelse(grid_master@data$uni %in% Southeastern$uni,"Southeastern",grid_master@data$subdivision)
grid_master@data$subdivision <- ifelse(grid_master@data$uni %in% Yakutat$uni,"Yakutat",grid_master@data$subdivision)
grid_master@data$subdivision <- ifelse(grid_master@data$uni %in% Kodiak$uni,"Kodiak",grid_master@data$subdivision)
grid_master@data$subdivision <- ifelse(grid_master@data$uni %in% Chirikof$uni,"Chirikof",grid_master@data$subdivision)
grid_master@data$subdivision <- ifelse(grid_master@data$uni %in% Shumagin$uni,"Shumagin",grid_master@data$subdivision)
grid_master@data$subdivision <- ifelse(grid_master@data$uni %in% Alislands$uni,"Alislands",grid_master@data$subdivision)
grid_master@data$subdivision <- ifelse(grid_master@data$uni %in% EBS1$uni,"EBS1",grid_master@data$subdivision)
grid_master@data$subdivision <- ifelse(grid_master@data$uni %in% EBS2$uni,"EBS2",grid_master@data$subdivision)
grid_master@data$subdivision <- ifelse(grid_master@data$uni %in% EBS3$uni,"EBS3",grid_master@data$subdivision)

##### now northwest Atlantic
NWA <- subset(xdat,xdat$long > -100 & xdat$long < -25)
plot(NWA$long,NWA$lat ,col="blue")

GuM1 <- subset(NWA,NWA$long < -90)
points(GuM1$long,GuM1$lat ,col="red")

GuM2 <- subset(NWA,NWA$long < -82 & NWA$long >-91 & NWA$ECO_REG =="Northern Gulf of Mexico")
points(GuM2$long,GuM2$lat ,col="red")

Floridian <- subset(NWA,NWA$ECO_REG =="Floridian" )
points(Floridian$long,Floridian$lat ,col="red")

Carolinian <- subset(NWA,NWA$ECO_REG =="Carolinian" & NWA$lat <33)
points(Carolinian$long,Carolinian$lat ,col="red")

Carvir <- subset(NWA,NWA$lat >33 & NWA$lat < 37)
points(Carvir$long,Carvir$lat ,col="red")

Virginian <- subset(NWA,NWA$lat >37 & NWA$long < -71 & NWA$ECO_REG =="Virginian")
points(Virginian$long,Virginian$lat ,col="red")

GeorgesB <- subset(NWA,NWA$lat <42 &NWA$lat >37)
points(GeorgesB$long,GeorgesB$lat ,col="red")

GMaine <- subset(NWA,NWA$lat >42 & NWA$long < -68 & NWA$ECO_REG =="Gulf of Maine/Bay of Fundy")
points(GMaine$long,GMaine$lat ,col="red")

WestScotian <- subset(NWA,NWA$lat >42 & NWA$lat <45.7 & NWA$long > -68 & NWA$long < -63 )
points(WestScotian$long,WestScotian$lat ,col="orange")

EastScotian <- subset(NWA,NWA$long > -63 & NWA$ECO_REG %in% c("Scotian Shelf","Gulf of St. Lawrence - Eastern Scotian Shelf") & NWA$lat <45.5)
points(EastScotian$long,EastScotian$lat ,col="black")

ESGLaw <- subset(NWA,NWA$lat < 48 & NWA$lat >45.5 & NWA$long > -67 & NWA$ECO_REG =="Gulf of St. Lawrence - Eastern Scotian Shelf")
points(ESGLaw$long,ESGLaw$lat ,col="orange")

GulfLaw <- subset(NWA, !(NWA$uni %in% ESGLaw$uni) &!(NWA$uni %in% EastScotian$uni) & NWA$ECO_REG =="Gulf of St. Lawrence - Eastern Scotian Shelf")
points(GulfLaw$long,GulfLaw$lat ,col="blue")

grid_master@data$subdivision <- ifelse(grid_master@data$uni %in% GuM1$uni,"GuM1",grid_master@data$subdivision)
grid_master@data$subdivision <- ifelse(grid_master@data$uni %in% GuM2$uni,"GuM2",grid_master@data$subdivision)
grid_master@data$subdivision <- ifelse(grid_master@data$uni %in% Floridian$uni,"Floridian",grid_master@data$subdivision)
grid_master@data$subdivision <- ifelse(grid_master@data$uni %in% Carolinian$uni,"Carolinian",grid_master@data$subdivision)
grid_master@data$subdivision <- ifelse(grid_master@data$uni %in% Carvir$uni,"Carvir",grid_master@data$subdivision)
grid_master@data$subdivision <- ifelse(grid_master@data$uni %in% Virginian$uni,"Virginian",grid_master@data$subdivision)
grid_master@data$subdivision <- ifelse(grid_master@data$uni %in% GeorgesB$uni,"GeorgesB",grid_master@data$subdivision)
grid_master@data$subdivision <- ifelse(grid_master@data$uni %in% GMaine$uni,"GMaine",grid_master@data$subdivision)
grid_master@data$subdivision <- ifelse(grid_master@data$uni %in% WestScotian$uni,"WestScotian",grid_master@data$subdivision)
grid_master@data$subdivision <- ifelse(grid_master@data$uni %in% EastScotian$uni,"EastScotian",grid_master@data$subdivision)
grid_master@data$subdivision <- ifelse(grid_master@data$uni %in% ESGLaw$uni,"ESGLaw",grid_master@data$subdivision)
grid_master@data$subdivision <- ifelse(grid_master@data$uni %in% GulfLaw$uni,"GulfLaw",grid_master@data$subdivision)

##### now Europe/ Norway 
NEA <- subset(xdat,xdat$long > -25 )
plot(NEA$long,NEA$lat ,col="blue")

Iberianc <- subset(NEA,NEA$lat < 43 & NEA$ECO_REG =="South European Atlantic Shelf")
points(Iberianc$long,Iberianc$lat ,col="red")

Biscay <- subset(NEA,NEA$lat > 43 & NEA$ECO_REG =="South European Atlantic Shelf")
points(Biscay$long,Biscay$lat ,col="red")

southCS <- subset(NEA,NEA$lat < 51.5 & NEA$long < -5 & NEA$ECO_REG =="Celtic Seas")
southCS2 <- subset(NEA,NEA$uni == "ID_1074")
southCS <- rbind(southCS,southCS2)
points(southCS$long,southCS$lat ,col="red")

IrishS <- subset(NEA,NEA$lat > 51.5 & NEA$lat < 56 & NEA$long > -8 & NEA$ECO_REG =="Celtic Seas")
IrishS2 <- subset(NEA,NEA$uni == "ID_1032")
IrishS <- rbind(IrishS,IrishS2)
points(IrishS$long,IrishS$lat ,col="red")

midCS <- subset(NEA,NEA$lat > 51.5 & NEA$lat < 56 & NEA$long < -8 & NEA$ECO_REG =="Celtic Seas")
points(midCS$long,midCS$lat ,col="red")

northCS <- subset(NEA,NEA$lat > 56 & NEA$long & NEA$ECO_REG =="Celtic Seas")
points(northCS$long,northCS$lat ,col="red")

channel <- subset(NEA,NEA$lat < 52 & NEA$long & NEA$ECO_REG =="North Sea")
channel2 <- subset(NEA,NEA$uni %in% c("ID_1075"))
channel <- rbind(channel,channel2)
points(channel$long,channel$lat ,col="red")

southNS <- subset(NEA,NEA$lat < 56.5 & NEA$lat > 52 & NEA$long < 9 & NEA$ECO_REG =="North Sea")
points(southNS$long,southNS$lat ,col="red")

northNS <- subset(NEA,NEA$lat > 56.5  & NEA$long < 9 & NEA$ECO_REG =="North Sea")
points(northNS$long,northNS$lat ,col="red")

katskag <- subset(NEA,NEA$long > 9 & NEA$ECO_REG =="North Sea")
points(katskag$long,katskag$lat ,col="red")

Southnor <- subset(NEA,NEA$lat < 62.5 & NEA$ECO_REG =="Southern Norway")
points(Southnor$long,Southnor$lat ,col="red")

Southnor2 <- subset(NEA,NEA$lat > 62.5 & NEA$ECO_REG =="Southern Norway")
points(Southnor2$long,Southnor2$lat ,col="red")

Baltic <- subset(NEA, NEA$ECO_REG =="Baltic Sea")
points(Baltic$long,Baltic$lat ,col="red")

Northnor <- subset(NEA,NEA$ECO_REG =="Northern Norway and Finnmark")
points(Northnor$long,Northnor$lat ,col="red")

Barents <- subset(NEA,NEA$ECO_REG =="North and East Barents Sea")
points(Barents$long,Barents$lat ,col="red")

grid_master@data$subdivision <- ifelse(grid_master@data$uni %in% Iberianc$uni,"Iberianc",grid_master@data$subdivision)
grid_master@data$subdivision <- ifelse(grid_master@data$uni %in% Biscay$uni,"Biscay",grid_master@data$subdivision)
grid_master@data$subdivision <- ifelse(grid_master@data$uni %in% southCS$uni,"southCS",grid_master@data$subdivision)
grid_master@data$subdivision <- ifelse(grid_master@data$uni %in% IrishS$uni,"IrishS",grid_master@data$subdivision)
grid_master@data$subdivision <- ifelse(grid_master@data$uni %in% midCS$uni,"midCS",grid_master@data$subdivision)
grid_master@data$subdivision <- ifelse(grid_master@data$uni %in% northCS$uni,"northCS",grid_master@data$subdivision)
grid_master@data$subdivision <- ifelse(grid_master@data$uni %in% channel$uni,"channel",grid_master@data$subdivision)
grid_master@data$subdivision <- ifelse(grid_master@data$uni %in% southNS$uni,"southNS",grid_master@data$subdivision)
grid_master@data$subdivision <- ifelse(grid_master@data$uni %in% northNS$uni,"northNS",grid_master@data$subdivision)
grid_master@data$subdivision <- ifelse(grid_master@data$uni %in% katskag$uni,"katskag",grid_master@data$subdivision)
grid_master@data$subdivision <- ifelse(grid_master@data$uni %in% Southnor$uni,"Southnor",grid_master@data$subdivision)
grid_master@data$subdivision <- ifelse(grid_master@data$uni %in% Southnor2$uni,"Southnor2",grid_master@data$subdivision)
grid_master@data$subdivision <- ifelse(grid_master@data$uni %in% Baltic$uni,"Baltic",grid_master@data$subdivision)
grid_master@data$subdivision <- ifelse(grid_master@data$uni %in% Northnor$uni,"Northnor",grid_master@data$subdivision)
grid_master@data$subdivision <- ifelse(grid_master@data$uni %in% Barents$uni,"Barents",grid_master@data$subdivision)


