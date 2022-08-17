
get_bio<-function(filename,t_start, t_end, spatialunit){
  grid <- grid_master
  depthgr <- depth_grid
  sstcobe <- sstdat
  tglorys <- tdat
  
  #  subset t_start : t_end
  cpue_final <- subset(filename,filename$year >= t_start & filename$year <= t_end)
  freq_dat <- as.data.frame(table(cpue_final$uni_cell))
  freq_dat <- subset(freq_dat,freq_dat$Freq >= 1) # select all grid cells, or cells sampled multiple years
  cpue_final <- subset(cpue_final,cpue_final$uni_cell %in% freq_dat$Var1)
  
  # add depth 
  depthgr$uni <- paste(depthgr$uni_cell, depthgr$year)
  cpue_final <- cbind(cpue_final,depthgr[match(cpue_final$uni,depthgr$uni),c("depth")])
  colnames(cpue_final)[ncol(cpue_final)] <- "depth"
  
  #  now estimate average biomass/ catch etc. per cell
  cpue_final <-  aggregate(list(cpue_final$biomass,cpue_final$tlw,
                                cpue_final$Catch_sqkm,cpue_final$Catch_pel_sqkm,cpue_final$depth),
                           by=list(cpue_final$uni_cell),FUN=mean,na.rm=T)
  colnames(cpue_final) <- c("uni_cell","biomass","tlw","Catch_sqkm","Catch_pel_sqkm","depth")
  
  grid <- cbind(grid,cpue_final[match(grid@data$uni_cell,cpue_final$uni_cell),c(2:6)])
  grid <- subset(grid,!(is.na(grid@data$biomass))) # remove all cells without information (not sampled)
  
  # convert mapped depth to positive
  grid@data$Depth_map <- abs(grid@data$Depth_map)
  
  # add column to count grid cells per subdivision/ecoregion
  grid@data$countgrid <- 1
  
  # add SST based on COBE timeseries data
  sstcobe <- data.frame(uni_cell = sstcobe$uni_cell, SST_time = rowMeans(sstcobe[,which(colnames(sstcobe) %in% t_start:t_end)]))
  grid <- cbind(grid,sstcobe[match(grid@data$uni_cell,sstcobe$uni_cell),c(2)])
  colnames(grid@data)[ncol(grid@data)] <- "SST_time"
  
  t_start <- ifelse(t_start < 1993,1993,t_start)
  tglorys <- data.frame(uni_cell = tglorys$uni_cell, T_time = rowMeans(tglorys[,which(colnames(tglorys) %in% t_start:t_end)]))
  grid <- cbind(grid,tglorys[match(grid@data$uni_cell,tglorys$uni_cell),c(2)])
  colnames(grid@data)[ncol(grid@data)] <- "T_time"
  
  # now get estimate per ecoregion 
  ecodat <- grid@data
  ecodat[,c(7:18,22:26,29)]  <- ecodat[,c(7:18,22:26,29)] * ecodat$ocean_sqkm # weighted with the size of the ocean area
  
  tt <-  aggregate(list(ecodat[,c(4,7:18,22:27,29)]),
                   by=list(ecodat[,spatialunit]),FUN=sum,na.rm=T)
  cnam <- colnames(ecodat[,c(4,7:18,22:27,29)])
  colnames(tt) <- c(spatialunit,cnam)
  tt[,c(3:19,21)] <- tt[,c(3:19,21)]/tt$ocean_sqkm
  
  ## do the same for SST, but first remove NAs
  ecodat <- subset(ecodat,!(is.na(ecodat$SST_time)))
  ecodat[,c(28)]  <- ecodat[,c(28)] * ecodat$ocean_sqkm # weighted with the size of the ocean area
  tt2 <-  aggregate(list(ecodat[,c(4,28)]),
                    by=list(ecodat[,spatialunit]),FUN=sum,na.rm=T)
  cnam <- colnames(ecodat[,c(4,28)])
  colnames(tt2) <- c(spatialunit,cnam)
  tt2[,3] <- tt2[,3]/tt2$ocean_sqkm
  
  ## combine
  biodat <- cbind(tt,tt2[match(tt[,spatialunit],tt2[,spatialunit]),c("SST_time")])
  colnames(biodat)[ncol(biodat)]<-"SST_time"
  return(biodat)
}
