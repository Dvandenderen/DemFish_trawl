
##########
#### for two regions - part of the survey is in a different year and not all years are sampled
##########

# region 1 - North American Pacific Fijordland
degrees_sub <- c("ID_-128.19_50.22","ID_204","ID_-132.79_52.68","ID_-132.62_53.46")  # grid cells with biomass estimate in "wrong" year
years <- data.frame(year = c(2003,2005,2007,2009,2011,2013,2015,2017,2019) ) # years for which data is needed

# get for all points with data +- 1 year the average
for (ideg in 1:length(degrees_sub)){
  #print(ideg)
  cell <- subset(cpue,cpue$uni_cell == degrees_sub[ideg])
  res <- merge(cell,years,by.x='year',by.y='year',all.x=T,all.y=T)
  res <- res %>% fill(uni_cell,ECO_REG, .direction = "updown")
  res$uni <-paste(res$uni_cell,res$year)
  nb <- which(is.na(res$biomass))
  nb <- subset(nb,!(nb %in% c(1,nrow(res))))
  #print(nb)
  res$biomass[nb] <- (res$biomass[nb + 1] +  res$biomass[nb - 1]) / 2
  res$tlw[nb] <- (res$tlw[nb + 1] +  res$tlw[nb - 1]) / 2
  cpue <- subset(cpue,!(cpue$uni_cell == degrees_sub[ideg]))
  cpue <- rbind(cpue,res)
}

# region 2 - Aleutian Islands 
degrees_sub <- c("ID_-170.57_51.93", "ID_-169.71_52.48", "ID_-167.68_52.8",  
                 "ID_-166.78_53.34", "ID_-165.62_53.1",  "ID_-164.71_53.63")   # grid cells with biomass estimate in "wrong" year
years <- data.frame(year = c(1983,1986,1991,1994,1997,2000,2002,2004,2006,2010,2012,2014,2016,2018) ) # years for which data is needed

for (ideg in 1:length(degrees_sub)){
  #print(ideg)
  cell <- subset(cpue,cpue$uni_cell == degrees_sub[ideg])
  res <- merge(cell,years,by.x='year',by.y='year',all.x=T,all.y=T)
  res <- res %>% fill(uni_cell,ECO_REG, .direction = "updown")
  res$uni <-paste(res$uni_cell,res$year)
  nb <- which(is.na(res$biomass))
  nb <- subset(nb,!(nb %in% c(1,nrow(res))))
  #print(nb)
  res$biomass[nb] <- (res$biomass[nb + 1] +  res$biomass[nb - 1]) / 2
  res$tlw[nb] <- (res$tlw[nb + 1] +  res$tlw[nb - 1]) / 2
  cpue <- subset(cpue,!(cpue$uni_cell ==  degrees_sub[ideg]))
  cpue <- rbind(cpue,res)
}

cpue <- subset(cpue,!(is.na(cpue$biomass)))

###
### now that some time gaps are filled in, other spatial gaps will be filled 
### for all points with data +- 1 year based on the average
### for all other points based on information in the cell (the year before/after)
### and the annual increase/decline of the total area relative
### to the reference year

# first remove all data from years with insufficient data in certain years
cpue_good <- c()
for (iReg in 1:nrow(timeser)){
  regdat <- subset(cpue,cpue$ECO_REG == timeser$EcReg[iReg])
  year_series <- subset(timeser,timeser$EcReg == regdat$ECO_REG[1])
  
  if (regdat$ECO_REG[1] == "Gulf of Alaska"){
    years <- c(1984, 1987, 1990, 1993, 1996, 1999, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019)
    
  } else if(regdat$ECO_REG[1] == "Oregon, Washington, Vancouver Coast and Shelf") {
    years <- c(1980, 1983, 1986, 1989, 1992, 1995, 1998, 2001, 2003, 2004, 2005, 2006, 2007, 
               2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018) 
    
  } else if(regdat$ECO_REG[1] ==  "Northern California") {
    years <- c(1989, 1992, 1995, 1998, 2001, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010,
               2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)
    
  } else if(regdat$ECO_REG[1] == "Aleutian Islands") {
    years   <- c(1983,1986,1991,1994,1997,2000,2002,2004,2006,2010,2012,2014,2016,2018)
    
  } else if(regdat$ECO_REG[1] == "North American Pacific Fijordland") {
    
    years   <- c(2003,2005,2007,2009,2011,2013,2015,2017,2019)
    
  } else { years <- c(year_series[,2]:year_series[,3]) }
  
  regdat <- subset(regdat,regdat$year %in% years)  
  
  cpue_good <- rbind(cpue_good,regdat)  
}

### fill spatial gaps
for (ideg in 1:length(degrees)){
  cell <- subset(cpue_good,cpue_good$uni_cell == degrees[ideg]) # select 1 cell

  # get all years with data in the region
  years <- data.frame(year = c(sort(unique(cpue_good$year[cpue_good$ECO_REG ==cell$ECO_REG[1]])))) 
  
  #year_out <- years$year[which(!(years$year %in% cell$year))]  # check which years have no data in the cell
   year_in  <- years$year[which((years$year %in% cell$year))]  # check which years have data in the cell
  
  # get for all points with data +- 1 year the average
    res <- merge(cell,years,by.x='year',by.y='year',all.x=T,all.y=T)
    res <- res %>% fill(uni_cell,ECO_REG, .direction = "updown")
    res$uni <-paste(res$uni_cell,res$year)
    nb <- which(is.na(res$biomass))
    nb <- subset(nb,!(nb %in% c(1,nrow(res))))
    res$biomass[nb] <- (res$biomass[nb + 1] +  res$biomass[nb - 1]) / 2
    res$tlw[nb] <- (res$tlw[nb + 1] +  res$tlw[nb - 1]) / 2

    nb <- res$year[which(is.na(res$biomass))]
    if (length(nb) >0){
    for (iYear in 1:length(nb)){
      dd <- year_in[which(abs(year_in-nb[iYear])==min(abs(year_in-nb[iYear])))][1]
      corfactor_bio <-  mean(cpue_good$biomass[cpue_good$year == nb[iYear] & cpue_good$ECO_REG == cell$ECO_REG[1]]) /
                        mean(cpue_good$biomass[cpue_good$year == dd & cpue_good$ECO_REG == cell$ECO_REG[1]])
    
      corfactor_tlw <-  mean(cpue_good$tlw[cpue_good$year == nb[iYear] & cpue_good$ECO_REG == cell$ECO_REG[1]]) /
                        mean(cpue_good$tlw[cpue_good$year == dd & cpue_good$ECO_REG == cell$ECO_REG[1]])
      
      res$biomass[which(res$year == nb[iYear])] <- res$biomass[res$year == dd] * corfactor_bio
      res$tlw[which(res$year == nb[iYear])] <- res$tlw[res$year == dd] * corfactor_tlw
    }}
      
    cpue_good <- subset(cpue_good,!(cpue_good$uni_cell == degrees[ideg]))
    cpue_good <- rbind(cpue_good,res)
}
  ### add degrees here to remove all without
cpue_good <- subset(cpue_good,cpue_good$uni_cell  %in% degrees)

rm(list=setdiff(ls(), c("cpue","grid_master","depth_grid","cpue_good","timeser","degrees")))

  
 