
shape1 <- shape[shape$SP_ID == ass_nb,]

if (ass_nb==11){
  pls3<-Polygons(list(shape1@polygons[[1]]@Polygons[[2]],shape1@polygons[[1]]@Polygons[[3]],shape1@polygons[[1]]@Polygons[[4]]),10)
  sps = SpatialPolygons(list(pls3))
  proj4string(sps)<-CRS("+init=epsg:4326")
  slot(sps, "polygons") <- lapply(slot(sps, "polygons"), checkPolygonsHoles)
  shape1<-sps
}

if (ass_nb==36){
  shape1@polygons[[1]]@Polygons[[1]]@coords<-shape1@polygons[[1]]@Polygons[[1]]@coords[-c(16,40,45),]
  shape1@polygons[[1]]@Polygons[[4]]@coords<-shape1@polygons[[1]]@Polygons[[4]]@coords[-c(4),]
  pls3<-Polygons(list(shape1@polygons[[1]]@Polygons[[1]],shape1@polygons[[1]]@Polygons[[2]],
                      shape1@polygons[[1]]@Polygons[[3]],shape1@polygons[[1]]@Polygons[[4]]),35)
  sps = SpatialPolygons(list(pls3))
  proj4string(sps)<-CRS("+init=epsg:4326")
  slot(sps, "polygons") <- lapply(slot(sps, "polygons"), checkPolygonsHoles)
  shape1<-sps
}

if (ass_nb==45){
  shape1@polygons[[1]]@Polygons[[2]]@coords<-shape1@polygons[[1]]@Polygons[[2]]@coords[-c(2,4,6),]
  pls3<-Polygons(list(shape1@polygons[[1]]@Polygons[[1]],shape1@polygons[[1]]@Polygons[[2]]),44)
  sps = SpatialPolygons(list(pls3))
  proj4string(sps)<-CRS("+init=epsg:4326")
  slot(sps, "polygons") <- lapply(slot(sps, "polygons"), checkPolygonsHoles)
  shape1<-sps
}

if (ass_nb==63){
  pls3<-Polygons(list(shape1@polygons[[1]]@Polygons[[1]],shape1@polygons[[1]]@Polygons[[4]]),62)
  sps = SpatialPolygons(list(pls3))
  proj4string(sps)<-CRS("+init=epsg:4326")
  slot(sps, "polygons") <- lapply(slot(sps, "polygons"), checkPolygonsHoles)
  shape1<-sps
}

if (ass_nb==75){
  pls3<-Polygons(list(shape1@polygons[[1]]@Polygons[[1]]),74)
  sps = SpatialPolygons(list(pls3))
  proj4string(sps)<-CRS("+init=epsg:4326")
  slot(sps, "polygons") <- lapply(slot(sps, "polygons"), checkPolygonsHoles)
  shape1<-sps
}

if (ass_nb==129){
  shape1@polygons[[1]]@Polygons[[1]]@coords<-shape1@polygons[[1]]@Polygons[[1]]@coords[-c(124),]
  pls3<-Polygons(list(shape1@polygons[[1]]@Polygons[[1]],shape1@polygons[[1]]@Polygons[[2]]),128)
  sps = SpatialPolygons(list(pls3))
  proj4string(sps)<-CRS("+init=epsg:4326")
  slot(sps, "polygons") <- lapply(slot(sps, "polygons"), checkPolygonsHoles)
  shape1<-sps}

if (ass_nb==130){
  shape1@polygons[[1]]@Polygons[[1]]@coords<-shape1@polygons[[1]]@Polygons[[1]]@coords[-c(29,71),]
  # shape1@polygons[[1]]@Polygons[[1]]@coords<-shape1@polygons[[1]]@Polygons[[4]]@coords[-c(4),]
  pls3<-Polygons(list(shape1@polygons[[1]]@Polygons[[1]],shape1@polygons[[1]]@Polygons[[3]],
                      shape1@polygons[[1]]@Polygons[[5]]),129)
  sps = SpatialPolygons(list(pls3))
  proj4string(sps)<-CRS("+init=epsg:4326")
  slot(sps, "polygons") <- lapply(slot(sps, "polygons"), checkPolygonsHoles)
  shape1<-sps
}

if (ass_nb==134){
  shape1@polygons[[1]]@Polygons[[1]]@coords<-shape1@polygons[[1]]@Polygons[[1]]@coords[-c(54),]
  pls3<-Polygons(list(shape1@polygons[[1]]@Polygons[[1]],shape1@polygons[[1]]@Polygons[[2]]),133)
  sps = SpatialPolygons(list(pls3))
  proj4string(sps)<-CRS("+init=epsg:4326")
  slot(sps, "polygons") <- lapply(slot(sps, "polygons"), checkPolygonsHoles)
  shape1<-sps
}

if (ass_nb==135){
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

if (ass_nb==138){
  shape1@polygons[[1]]@Polygons[[1]]@coords<-shape1@polygons[[1]]@Polygons[[1]]@coords[-c(39,143),]
  pls3<-Polygons(list(shape1@polygons[[1]]@Polygons[[1]]),137)
  sps = SpatialPolygons(list(pls3))
  proj4string(sps)<-CRS("+init=epsg:4326")
  slot(sps, "polygons") <- lapply(slot(sps, "polygons"), checkPolygonsHoles)
  shape1<-sps
}

if (ass_nb==142){
  shape1@polygons[[1]]@Polygons[[1]]@coords<-shape1@polygons[[1]]@Polygons[[1]]@coords[-c(117),]
  pls3<-Polygons(list(shape1@polygons[[1]]@Polygons[[1]]),141)
  sps = SpatialPolygons(list(pls3))
  proj4string(sps)<-CRS("+init=epsg:4326")
  slot(sps, "polygons") <- lapply(slot(sps, "polygons"), checkPolygonsHoles)
  shape1<-sps
}

if (ass_nb==144){
  shape1@polygons[[1]]@Polygons[[1]]@coords<-shape1@polygons[[1]]@Polygons[[1]]@coords[-c(306,521,523,566,583,590,719),]
  pls3<-Polygons(list(shape1@polygons[[1]]@Polygons[[1]],shape1@polygons[[1]]@Polygons[[4]],
                      shape1@polygons[[1]]@Polygons[[16]]),143)
  sps = SpatialPolygons(list(pls3))
  proj4string(sps)<-CRS("+init=epsg:4326")
  slot(sps, "polygons") <- lapply(slot(sps, "polygons"), checkPolygonsHoles)
  shape1<-sps
}

if (ass_nb==146){
  shape1@polygons[[1]]@Polygons[[2]]@coords<-shape1@polygons[[1]]@Polygons[[2]]@coords[-c(113:115,263),]
  pls3<-Polygons(list(shape1@polygons[[1]]@Polygons[[1]],shape1@polygons[[1]]@Polygons[[2]]),145)
  sps = SpatialPolygons(list(pls3))
  proj4string(sps)<-CRS("+init=epsg:4326")
  slot(sps, "polygons") <- lapply(slot(sps, "polygons"), checkPolygonsHoles)
  shape1<-sps
}

if (ass_nb==147){
  shape1@polygons[[1]]@Polygons[[1]]@coords<-shape1@polygons[[1]]@Polygons[[1]]@coords[-c(117),]
  pls3<-Polygons(list(shape1@polygons[[1]]@Polygons[[1]]),146)
  sps = SpatialPolygons(list(pls3))
  proj4string(sps)<-CRS("+init=epsg:4326")
  slot(sps, "polygons") <- lapply(slot(sps, "polygons"), checkPolygonsHoles)
  shape1<-sps
}

if (ass_nb==148){
  shape1@polygons[[1]]@Polygons[[1]]@coords<-shape1@polygons[[1]]@Polygons[[1]]@coords[-c(38:150),]
  pls3<-Polygons(list(shape1@polygons[[1]]@Polygons[[1]]),147)
  sps = SpatialPolygons(list(pls3))
  proj4string(sps)<-CRS("+init=epsg:4326")
  slot(sps, "polygons") <- lapply(slot(sps, "polygons"), checkPolygonsHoles)
  shape1<-sps
}

if (ass_nb==150){
  shape1@polygons[[1]]@Polygons[[2]]@coords<-shape1@polygons[[1]]@Polygons[[2]]@coords[-c(328,331),]
  pls3<-Polygons(list(shape1@polygons[[1]]@Polygons[[1]],shape1@polygons[[1]]@Polygons[[2]],
                      shape1@polygons[[1]]@Polygons[[7]]),149)
  sps = SpatialPolygons(list(pls3))
  proj4string(sps)<-CRS("+init=epsg:4326")
  slot(sps, "polygons") <- lapply(slot(sps, "polygons"), checkPolygonsHoles)
  shape1<-sps
}

if (ass_nb==151){
  shape1@polygons[[1]]@Polygons[[1]]@coords<-shape1@polygons[[1]]@Polygons[[1]]@coords[-c(14,20,21),]
  
  pls3<-Polygons(list(shape1@polygons[[1]]@Polygons[[1]],shape1@polygons[[1]]@Polygons[[2]],
                      shape1@polygons[[1]]@Polygons[[3]],shape1@polygons[[1]]@Polygons[[4]],
                      shape1@polygons[[1]]@Polygons[[5]],shape1@polygons[[1]]@Polygons[[6]]),150)
  sps = SpatialPolygons(list(pls3))
  proj4string(sps)<-CRS("+init=epsg:4326")
  slot(sps, "polygons") <- lapply(slot(sps, "polygons"), checkPolygonsHoles)
  shape1<-sps
}

if (ass_nb==156){
  shape1@polygons[[1]]@Polygons[[1]]@coords<-shape1@polygons[[1]]@Polygons[[1]]@coords[-c(26,27),]
  pls3<-Polygons(list(shape1@polygons[[1]]@Polygons[[1]]),155)
  sps = SpatialPolygons(list(pls3))
  proj4string(sps)<-CRS("+init=epsg:4326")
  slot(sps, "polygons") <- lapply(slot(sps, "polygons"), checkPolygonsHoles)
  shape1<-sps
}

if (ass_nb==197){
  shape1@polygons[[1]]@Polygons[[1]]@coords<-shape1@polygons[[1]]@Polygons[[1]]@coords[-c(45,46),]
  pls3<-Polygons(list(shape1@polygons[[1]]@Polygons[[1]]),196)
  sps = SpatialPolygons(list(pls3))
  proj4string(sps)<-CRS("+init=epsg:4326")
  slot(sps, "polygons") <- lapply(slot(sps, "polygons"), checkPolygonsHoles)
  shape1<-sps
}

if (ass_nb==198){
  pls3<-Polygons(list(shape1@polygons[[1]]@Polygons[[2]]),197)
  sps = SpatialPolygons(list(pls3))
  proj4string(sps)<-CRS("+init=epsg:4326")
  slot(sps, "polygons") <- lapply(slot(sps, "polygons"), checkPolygonsHoles)
  shape1<-sps
}

if (ass_nb==199){
  pls3<-Polygons(list(shape1@polygons[[1]]@Polygons[[1]],shape1@polygons[[1]]@Polygons[[3]]),198)
  sps = SpatialPolygons(list(pls3))
  proj4string(sps)<-CRS("+init=epsg:4326")
  slot(sps, "polygons") <- lapply(slot(sps, "polygons"), checkPolygonsHoles)
  shape1<-sps
}

if (ass_nb==42 | ass_nb== 46 | ass_nb==67 | ass_nb==68 | ass_nb==69 | ass_nb==70| ass_nb==74| ass_nb==114| ass_nb==139| ass_nb==140| ass_nb==141| ass_nb==143
    | ass_nb==149 | ass_nb==152| ass_nb==154| ass_nb==158| ass_nb==165| ass_nb==168| ass_nb==179| ass_nb==180| ass_nb==182){
  pls3<-shape1
  slot(pls3, "polygons") <- lapply(slot(pls3, "polygons"), checkPolygonsHoles)
  shape1<-pls3
}