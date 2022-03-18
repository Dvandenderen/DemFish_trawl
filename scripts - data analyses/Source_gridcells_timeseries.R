
# check per region the spatial consistency in trawl surveys
# aim to get all grid-cells with at least 50% observation in the time-series
# start with estimate of 

cpue <- cbind(cpue,grid_master@data[match(cpue$uni_cell,grid_master@data$uni_cell),c("ECO_REG")])
colnames(cpue)[ncol(cpue)] <- "ECO_REG" 

uni_reg <- unique(cpue$ECO_REG)
uni_reg <- subset(uni_reg,!(is.na(uni_reg)))
timeser <- data.frame(EcReg = uni_reg, start = NA, end= NA)
degrees <- c()

# "North and East Barents Sea"
#####  
    iReg <- which(uni_reg == "North and East Barents Sea")
    cell <- subset(cpue,cpue$ECO_REG == timeser[iReg,1])
    cell <- subset(cell,!(is.na(cell$biomass)))
    # table(cell$year)
   
    cell_sub <- subset(cell,cell$year %in% c(1989:2010))
    areaplot <- subset(grid_master,grid_master@data$ECO_REG==timeser[iReg,1])
    
    #par(mfrow = c(3, 4))
    # for (j in 1989:2010){
    # cell_plot <- subset(cell_sub,cell_sub$year == j)
    # tt <- subset(grid_master,grid_master@data$uni %in% cell_plot$uni_cell)  
    # plot(areaplot, main = j)
    # plot(tt,add=T, col="red")
    # }
   
    tall <- cell_sub[ cell_sub$uni_cell %in%  names(table(cell_sub$uni_cell))[table(cell_sub$uni_cell) >11] , ]
    tt <- subset(grid_master,grid_master@data$uni %in% tall$uni_cell)
    # plot(areaplot, main = "all")
    # plot(tt,add=T, col="red")
   
    # table(tall$year) # good coverage from 1989-2010
    timeser[iReg,2] <- 1989
    timeser[iReg,3] <- 2010
   
    tall <- subset(tall,tall$year %in% c(timeser[iReg,2]:timeser[iReg,3]))
    degrees <- c(degrees,unique(tall$uni_cell))

# "Northern Norway and Finnmark"
#####
    iReg <- which(uni_reg == "Northern Norway and Finnmark")
    cell <- subset(cpue,cpue$ECO_REG == timeser[iReg,1])
    cell <- subset(cell,!(is.na(cell$biomass)))
    # table(cell$year)
   
    cell_sub <- subset(cell,cell$year %in% c(1990:2017))
    areaplot <- subset(grid_master,grid_master@data$ECO_REG==timeser[iReg,1])
   
    #par(mfrow = c(3, 4))
    #for (j in 1990:2017){
    #  cell_plot <- subset(cell_sub,cell_sub$year == j)
    #  tt <- subset(grid_master,grid_master@data$uni %in% cell_plot$uni_cell)
    #  plot(areaplot, main = j)
    #  plot(tt,add=T, col="red")
    #}
   
    tall <- cell_sub[ cell_sub$uni_cell %in%  names(table(cell_sub$uni_cell))[table(cell_sub$uni_cell) >14] , ]
    tt <- subset(grid_master,grid_master@data$uni %in% tall$uni_cell)
    #par(mfrow = c(1, 1))
    #plot(areaplot, main = "all")
    #plot(tt,add=T, col="blue")
   
    # table(tall$year) # good coverage from 1990-2017
    timeser[iReg,2] <- 1990
    timeser[iReg,3] <- 2017
   
    tall <- subset(tall,tall$year %in% c(timeser[iReg,2]:timeser[iReg,3]))
    degrees <- c(degrees,unique(tall$uni_cell))

# "Southern Norway"
##### 
   iReg <-  which(uni_reg == "Southern Norway")
   cell <- subset(cpue,cpue$ECO_REG == timeser[iReg,1])
   cell <- subset(cell,!(is.na(cell$biomass)))
   # table(cell$year)
   
   cell_sub <- subset(cell,cell$year %in% c(2000:2017))
   areaplot <- subset(grid_master,grid_master@data$ECO_REG==timeser[iReg,1])
   
   # par(mfrow = c(3, 4))
   # for (j in 2000:2017){
   #  cell_plot <- subset(cell_sub,cell_sub$year == j)
   #  tt <- subset(grid_master,grid_master@data$uni %in% cell_plot$uni_cell)
   #   plot(areaplot, main = j)
   #   plot(tt,add=T, col="red")
   # }
   
   tall <- cell_sub[ cell_sub$uni_cell %in%  names(table(cell_sub$uni_cell))[table(cell_sub$uni_cell) >9] , ]
   tt <- subset(grid_master,grid_master@data$uni %in% tall$uni_cell)
   #par(mfrow = c(1, 1))
   #plot(areaplot, main = "all")
   #plot(tt,add=T, col="blue")
   
   # table(tall$year) # okay coverage from 2000-2017
   timeser[iReg,2] <- 2000
   timeser[iReg,3] <- 2017
   
   tall <- subset(tall,tall$year %in% c(timeser[iReg,2]:timeser[iReg,3]))
   degrees <- c(degrees,unique(tall$uni_cell))

# "Eastern Bering Sea"
#####
   iReg <-  which(uni_reg ==  "Eastern Bering Sea")
   cell <- subset(cpue,cpue$ECO_REG == timeser[iReg,1])
   cell <- subset(cell,!(is.na(cell$biomass)))
   # table(cell$year)
   
   cell_sub <- subset(cell,cell$year %in% c(1982:2019))
   areaplot <- subset(grid_master,grid_master@data$ECO_REG==timeser[iReg,1])
   
   #par(mfrow = c(3, 4))
   # for (j in 1982:2019){
   #  cell_plot <- subset(cell_sub,cell_sub$year == j)
   #   tt <- subset(grid_master,grid_master@data$uni %in% cell_plot$uni_cell)
   #   plot(areaplot, main = j)
   #   plot(tt,add=T, col="red")
   # }
   
   tall <- cell_sub[ cell_sub$uni_cell %in%  names(table(cell_sub$uni_cell))[table(cell_sub$uni_cell) > 19] , ]
   tt <- subset(grid_master,grid_master@data$uni %in% tall$uni_cell)
   #par(mfrow = c(1, 1))
   #plot(areaplot, main = "all")
   #plot(tt,add=T, col="blue")
   
   # table(tall$year) # good coverage from 1982-2019
   timeser[iReg,2] <- 1982
   timeser[iReg,3] <- 2019
   
   tall <- subset(tall,tall$year %in% c(timeser[iReg,2]:timeser[iReg,3]))
   degrees <- c(degrees,unique(tall$uni_cell))

### "North Sea"
   iReg <- which(uni_reg == "North Sea")
   cell <- subset(cpue,cpue$ECO_REG == timeser[iReg,1])
   cell <- subset(cell,!(is.na(cell$biomass)))
   # table(cell$year)
   
   cell_sub <- subset(cell,cell$year %in% c(1980:2019))
   areaplot <- subset(grid_master,grid_master@data$ECO_REG==timeser[iReg,1])
   
   # par(mfrow = c(3, 4))
   # for (j in 1980:2019){
   #   cell_plot <- subset(cell_sub,cell_sub$year == j)
   #   tt <- subset(grid_master,grid_master@data$uni %in% cell_plot$uni_cell)
   #   plot(areaplot, main = j)
   #   plot(tt,add=T, col="red")
   # }
   
   tall <- cell_sub[ cell_sub$uni_cell %in%  names(table(cell_sub$uni_cell))[table(cell_sub$uni_cell) > 20] , ]
   tt <- subset(grid_master,grid_master@data$uni %in% tall$uni_cell)
   #par(mfrow = c(1, 1))
   #plot(areaplot, main = "all")
   #plot(tt,add=T, col="blue")
   
   # table(tall$year) # good coverage from 1980-2019
   timeser[iReg,2] <- 1980
   timeser[iReg,3] <- 2019
   
   tall <- subset(tall,tall$year %in% c(timeser[iReg,2]:timeser[iReg,3]))
   degrees <- c(degrees,unique(tall$uni_cell))

# "Gulf of Alaska"
####
   iReg <- which(uni_reg ==  "Gulf of Alaska")
   cell <- subset(cpue,cpue$ECO_REG == timeser[iReg,1])
   cell <- subset(cell,!(is.na(cell$biomass)))
   # table(cell$year)
   
   cell_sub <- subset(cell,cell$year %in% c(1984:2019))
   areaplot <- subset(grid_master,grid_master@data$ECO_REG==timeser[iReg,1])
   
   # par(mfrow = c(3, 4))
   # for (j in 1984:2019){
   #   cell_plot <- subset(cell_sub,cell_sub$year == j)
   #   tt <- subset(grid_master,grid_master@data$uni %in% cell_plot$uni_cell)
   #   plot(areaplot, main = j)
   #   plot(tt,add=T, col="red")
   # }
   
   tall <- cell_sub[ cell_sub$uni_cell %in%  names(table(cell_sub$uni_cell))[table(cell_sub$uni_cell) > 8] , ]
   tt <- subset(grid_master,grid_master@data$uni %in% tall$uni_cell)
   #par(mfrow = c(1, 1))
   #plot(areaplot, main = "all")
   #plot(tt,add=T, col="blue")
   
   # table(tall$year) # good coverage from 1984-2019 (note data gaps)
   timeser[iReg,2] <- 1984
   timeser[iReg,3] <- 2019
   
   tall <- subset(tall,tall$year %in% c(timeser[iReg,2]:timeser[iReg,3]))
   degrees <- c(degrees,unique(tall$uni_cell))

# "Celtic Seas"
#####
   iReg <- which(uni_reg == "Celtic Seas")
   cell <- subset(cpue,cpue$ECO_REG == timeser[iReg,1])
   cell <- subset(cell,!(is.na(cell$biomass)))
   table(cell$year)
   
   cell_sub <- subset(cell,cell$year %in% c(1990:2019)) # irregular spatial sampling, 
   areaplot <- subset(grid_master,grid_master@data$ECO_REG==timeser[iReg,1])
   
   # par(mfrow = c(3, 4))
   # for (j in 1990:2019){
   #   cell_plot <- subset(cell_sub,cell_sub$year == j)
   #   tt <- subset(grid_master,grid_master@data$uni %in% cell_plot$uni_cell)
   #   plot(areaplot, main = j)
   #   plot(tt,add=T, col="red")
   # }
   
   tall <- cell_sub[ cell_sub$uni_cell %in%  names(table(cell_sub$uni_cell))[table(cell_sub$uni_cell) > 11] , ]
   tt <- subset(grid_master,grid_master@data$uni %in% tall$uni_cell)
   #par(mfrow = c(1, 1))
   #plot(areaplot, main = "all")
   #plot(tt,add=T, col="blue")
   
   # table(tall$year) # good coverage from 1998-2019 (note data gaps)
   timeser[iReg,2] <- 1998
   timeser[iReg,3] <- 2019
   
   tall <- subset(tall,tall$year %in% c(timeser[iReg,2]:timeser[iReg,3]))
   degrees <- c(degrees,unique(tall$uni_cell))

# "Baltic Sea"
#####
   iReg <- which(uni_reg == "Baltic Sea")
   cell <- subset(cpue,cpue$ECO_REG == timeser[iReg,1])
   cell <- subset(cell,!(is.na(cell$biomass)))
   table(cell$year)
   
   cell_sub <- subset(cell,cell$year %in% c(2001:2019))  
   areaplot <- subset(grid_master,grid_master@data$ECO_REG==timeser[iReg,1])
   
   # par(mfrow = c(3, 4))
   # for (j in 2001:2019){
   #   cell_plot <- subset(cell_sub,cell_sub$year == j)
   #   tt <- subset(grid_master,grid_master@data$uni %in% cell_plot$uni_cell)
   #   plot(areaplot, main = j)
   #   plot(tt,add=T, col="red")
   # }
   
   tall <- cell_sub[ cell_sub$uni_cell %in%  names(table(cell_sub$uni_cell))[table(cell_sub$uni_cell) > 9] , ]
   tt <- subset(grid_master,grid_master@data$uni %in% tall$uni_cell)
   #par(mfrow = c(1, 1))
   #plot(areaplot, main = "all")
   #plot(tt,add=T, col="blue")
   
   # table(tall$year) # good coverage from 2001-2019 (note data gaps)
   timeser[iReg,2] <- 2001
   timeser[iReg,3] <- 2019
   
   tall <- subset(tall,tall$year %in% c(timeser[iReg,2]:timeser[iReg,3]))
   degrees <- c(degrees,unique(tall$uni_cell))

#  "North American Pacific Fijordland" -- two seperate surveys 
#####
   iReg <- which(uni_reg == "North American Pacific Fijordland")
   cell <- subset(cpue,cpue$ECO_REG == timeser[iReg,1])
   cell <- subset(cell,!(is.na(cell$biomass)))
   table(cell$year)
   
   cell_sub <- subset(cell,cell$year %in% c(1984:2019))
   areaplot <- subset(grid_master,grid_master@data$ECO_REG==timeser[iReg,1])
   
   # par(mfrow = c(3, 4))
   # for (j in 1984:2019){
   #   cell_plot <- subset(cell_sub,cell_sub$year == j)
   #   tt <- subset(grid_master,grid_master@data$uni %in% cell_plot$uni_cell)
   #   plot(areaplot, main = j)
   #   plot(tt,add=T, col="red")
   # }
   
   tall <- cell_sub[ cell_sub$uni_cell %in%  names(table(cell_sub$uni_cell))[table(cell_sub$uni_cell) > 5] , ]
   tt <- subset(grid_master,grid_master@data$uni %in% tall$uni_cell)
   #par(mfrow = c(1, 1))
   #plot(areaplot, main = "all")
   #plot(tt,add=T, col="blue")
   
   # table(tall$year) # good coverage from 2003-2019 (note data gaps)
   timeser[iReg,2] <- 2003
   timeser[iReg,3] <- 2019
   
   tall <- subset(tall,tall$year %in% c(timeser[iReg,2]:timeser[iReg,3]))
   degrees <- c(degrees,unique(tall$uni_cell))

#  "Aleutian Islands" - two separate surveys
#####
   iReg <- which(uni_reg ==  "Aleutian Islands")
   cell <- subset(cpue,cpue$ECO_REG == timeser[iReg,1])
   cell <- subset(cell,!(is.na(cell$biomass)))
   table(cell$year)
   
   cell_sub <- subset(cell,cell$year %in% c(1983:2019))
   areaplot <- subset(grid_master,grid_master@data$ECO_REG==timeser[iReg,1])
   
   # par(mfrow = c(3, 4))
   # for (j in 1983:2019){
   #   cell_plot <- subset(cell_sub,cell_sub$year == j)
   #   tt <- subset(grid_master,grid_master@data$uni %in% cell_plot$uni_cell)
   #   plot(areaplot, main = j)
   #   plot(tt,add=T, col="red")
   # }
   
   tall <- cell_sub[ cell_sub$uni_cell %in%  names(table(cell_sub$uni_cell))[table(cell_sub$uni_cell) > 6] , ]
   tt <- subset(grid_master,grid_master@data$uni %in% tall$uni_cell)
   #par(mfrow = c(1, 1))
   #plot(areaplot, main = "all")
   #plot(tt,add=T, col="blue")
   #text(x= areaplot$long,y=areaplot$lat, areaplot$uni_cell)
   
   # table(tall$year) # good coverage from 1983-2019 (note data gaps)
   timeser[iReg,2] <- 1983
   timeser[iReg,3] <- 2019
   
   tall <- subset(tall,tall$year %in% c(timeser[iReg,2]:timeser[iReg,3]))
   degrees <- c(degrees,unique(tall$uni_cell))

#  "Gulf of St. Lawrence - Eastern Scotian Shelf" 
#####
   iReg <- which(uni_reg == "Gulf of St. Lawrence - Eastern Scotian Shelf")
   cell <- subset(cpue,cpue$ECO_REG == timeser[iReg,1])
   cell <- subset(cell,!(is.na(cell$biomass)))
   table(cell$year)
   
   cell_sub <- subset(cell,cell$year %in% c(1983:2019))
   areaplot <- subset(grid_master,grid_master@data$ECO_REG==timeser[iReg,1])
   
   # par(mfrow = c(3, 4))
   # for (j in 1983:2019){
   #   cell_plot <- subset(cell_sub,cell_sub$year == j)
   #   tt <- subset(grid_master,grid_master@data$uni %in% cell_plot$uni_cell)
   #   plot(areaplot, main = j)
   #   plot(tt,add=T, col="red")
   # }
   
   tall <- cell_sub[ cell_sub$uni_cell %in%  names(table(cell_sub$uni_cell))[table(cell_sub$uni_cell) > 19] , ]
   tt <- subset(grid_master,grid_master@data$uni %in% tall$uni_cell)
   #par(mfrow = c(1, 1))
   #plot(areaplot, main = "all")
   #plot(tt,add=T, col="blue")
   
   # table(tall$year) # good coverage from 1987-2019 
   timeser[iReg,2] <- 1987
   timeser[iReg,3] <- 2019
   
   tall <- subset(tall,tall$year %in% c(timeser[iReg,2]:timeser[iReg,3]))
   degrees <- c(degrees,unique(tall$uni_cell))

#  "Oregon, Washington, Vancouver Coast and Shelf" 
#####
   iReg <- which(uni_reg == "Oregon, Washington, Vancouver Coast and Shelf")
   cell <- subset(cpue,cpue$ECO_REG == timeser[iReg,1])
   cell <- subset(cell,!(is.na(cell$biomass)))
   table(cell$year)
   
   cell_sub <- subset(cell,cell$year %in% c(1977:2018))
   areaplot <- subset(grid_master,grid_master@data$ECO_REG==timeser[iReg,1])
   
   # par(mfrow = c(3, 4))
   # for (j in 1977:2018){
   #   cell_plot <- subset(cell_sub,cell_sub$year == j)
   #   tt <- subset(grid_master,grid_master@data$uni %in% cell_plot$uni_cell)
   #   plot(areaplot, main = j)
   #   plot(tt,add=T, col="red")
   # }
   
   tall <- cell_sub[ cell_sub$uni_cell %in%  names(table(cell_sub$uni_cell))[table(cell_sub$uni_cell) > 12] , ]
   tt <- subset(grid_master,grid_master@data$uni %in% tall$uni_cell)
   #par(mfrow = c(1, 1))
   #plot(areaplot, main = "all")
   #plot(tt,add=T, col="blue")
   
   # table(tall$year) # good coverage from 1980-2018 data gaps 
   timeser[iReg,2] <- 1980
   timeser[iReg,3] <- 2018
   
   tall <- subset(tall,tall$year %in% c(timeser[iReg,2]:timeser[iReg,3]))
   degrees <- c(degrees,unique(tall$uni_cell))

#  "South European Atlantic Shelf"
#####
   iReg <- which(uni_reg == "South European Atlantic Shelf")
   cell <- subset(cpue,cpue$ECO_REG == timeser[iReg,1])
   cell <- subset(cell,!(is.na(cell$biomass)))
   table(cell$year)
   
   cell_sub <- subset(cell,cell$year %in% c(1997:2014))
   areaplot <- subset(grid_master,grid_master@data$ECO_REG==timeser[iReg,1])
   
   # par(mfrow = c(3, 4))
   # for (j in 1997:2014){
   #   cell_plot <- subset(cell_sub,cell_sub$year == j)
   #   tt <- subset(grid_master,grid_master@data$uni %in% cell_plot$uni_cell)
   #   plot(areaplot, main = j)
   #   plot(tt,add=T, col="red")
   # }
   
   tall <- cell_sub[ cell_sub$uni_cell %in%  names(table(cell_sub$uni_cell))[table(cell_sub$uni_cell) > 9] , ]
   tt <- subset(grid_master,grid_master@data$uni %in% tall$uni_cell)
   #par(mfrow = c(1, 1))
   #plot(areaplot, main = "all")
   #plot(tt,add=T, col="blue")
   
   # table(tall$year) # good coverage from 1997-2014
   timeser[iReg,2] <- 1997
   timeser[iReg,3] <- 2014
   
   tall <- subset(tall,tall$year %in% c(timeser[iReg,2]:timeser[iReg,3]))
   degrees <- c(degrees,unique(tall$uni_cell))

#   "Gulf of Maine/Bay of Fundy"
#####
   iReg <- which(uni_reg == "Gulf of Maine/Bay of Fundy")
   cell <- subset(cpue,cpue$ECO_REG == timeser[iReg,1])
   cell <- subset(cell,!(is.na(cell$biomass)))
   table(cell$year)
   
   cell_sub <- subset(cell,cell$year %in% c(1970:2019))
   areaplot <- subset(grid_master,grid_master@data$ECO_REG==timeser[iReg,1])
   
   # par(mfrow = c(3, 4))
   # for (j in 1970:2019){
   #   cell_plot <- subset(cell_sub,cell_sub$year == j)
   #   tt <- subset(grid_master,grid_master@data$uni %in% cell_plot$uni_cell)
   #   plot(areaplot, main = j)
   #   plot(tt,add=T, col="red")
   # }
   
   tall <- cell_sub[ cell_sub$uni_cell %in%  names(table(cell_sub$uni_cell))[table(cell_sub$uni_cell) > 12] , ]
   tt <- subset(grid_master,grid_master@data$uni %in% tall$uni_cell)
   #par(mfrow = c(1, 1))
   #plot(areaplot, main = "all")
   #plot(tt,add=T, col="blue")
   
   # table(tall$year) # good coverage from 1970-2019
   timeser[iReg,2] <- 1970
   timeser[iReg,3] <- 2019
   
   tall <- subset(tall,tall$year %in% c(timeser[iReg,2]:timeser[iReg,3]))
   degrees <- c(degrees,unique(tall$uni_cell))

#  "Scotian Shelf"
#####
   iReg <- which(uni_reg == "Scotian Shelf")
   cell <- subset(cpue,cpue$ECO_REG == timeser[iReg,1])
   cell <- subset(cell,!(is.na(cell$biomass)))
   table(cell$year)
   
   cell_sub <- subset(cell,cell$year %in% c(1970:2020))
   areaplot <- subset(grid_master,grid_master@data$ECO_REG==timeser[iReg,1])
   
   # par(mfrow = c(3, 4))
   # for (j in 1970:2020){
   #   cell_plot <- subset(cell_sub,cell_sub$year == j)
   #   tt <- subset(grid_master,grid_master@data$uni %in% cell_plot$uni_cell)
   #   plot(areaplot, main = j)
   #   plot(tt,add=T, col="red")
   # }
   
   tall <- cell_sub[ cell_sub$uni_cell %in%  names(table(cell_sub$uni_cell))[table(cell_sub$uni_cell) > 25] , ]
   tt <- subset(grid_master,grid_master@data$uni %in% tall$uni_cell)
   #par(mfrow = c(1, 1))
   #plot(areaplot, main = "all")
   #plot(tt,add=T, col="blue")
   
   # table(tall$year) # good coverage from 1970-2019
   timeser[iReg,2] <- 1970
   timeser[iReg,3] <- 2019
   
   tall <- subset(tall,tall$year %in% c(timeser[iReg,2]:timeser[iReg,3]))
   degrees <- c(degrees,unique(tall$uni_cell))

#  "Virginian"
#####
   iReg <- which(uni_reg == "Virginian")
   cell <- subset(cpue,cpue$ECO_REG == timeser[iReg,1])
   cell <- subset(cell,!(is.na(cell$biomass)))
   table(cell$year)
   
   cell_sub <- subset(cell,cell$year %in% c(1970:2019))
   areaplot <- subset(grid_master,grid_master@data$ECO_REG==timeser[iReg,1])
   
   # par(mfrow = c(3, 4))
   # for (j in 1996:2019){
   #   cell_plot <- subset(cell_sub,cell_sub$year == j)
   #   tt <- subset(grid_master,grid_master@data$uni %in% cell_plot$uni_cell)
   #   plot(areaplot, main = j)
   #   plot(tt,add=T, col="red")
   # }
   
   tall <- cell_sub[ cell_sub$uni_cell %in%  names(table(cell_sub$uni_cell))[table(cell_sub$uni_cell) > 12] , ]
   tt <- subset(grid_master,grid_master@data$uni %in% tall$uni_cell)
   #par(mfrow = c(1, 1))
   #plot(areaplot, main = "all")
   #plot(tt,add=T, col="blue")
   
   # table(tall$year) # good coverage from 1974-2019
   timeser[iReg,2] <- 1974
   timeser[iReg,3] <- 2019
   
   tall <- subset(tall,tall$year %in% c(timeser[iReg,2]:timeser[iReg,3]))
   degrees <- c(degrees,unique(tall$uni_cell))

# "Northern California"
#####
   iReg <- which(uni_reg == "Northern California")
   cell <- subset(cpue,cpue$ECO_REG == timeser[iReg,1])
   cell <- subset(cell,!(is.na(cell$biomass)))
   table(cell$year)
   
   cell_sub <- subset(cell,cell$year %in% c(1989:2018))
   areaplot <- subset(grid_master,grid_master@data$ECO_REG==timeser[iReg,1])
   
   # par(mfrow = c(3, 4))
   # for (j in 1989:2018){
   #   cell_plot <- subset(cell_sub,cell_sub$year == j)
   #   tt <- subset(grid_master,grid_master@data$uni %in% cell_plot$uni_cell)
   #   plot(areaplot, main = j)
   #   plot(tt,add=T, col="red")
   # }
   
   tall <- cell_sub[ cell_sub$uni_cell %in%  names(table(cell_sub$uni_cell))[table(cell_sub$uni_cell) > 9] , ]
   tt <- subset(grid_master,grid_master@data$uni %in% tall$uni_cell)
   #par(mfrow = c(1, 1))
   #plot(areaplot, main = "all")
   #plot(tt,add=T, col="blue")
   
   # table(tall$year) # good coverage from 2001-2018 with gaps
   timeser[iReg,2] <- 2001
   timeser[iReg,3] <- 2018
   
   tall <- subset(tall,tall$year %in% c(timeser[iReg,2]:timeser[iReg,3]))
   degrees <- c(degrees,unique(tall$uni_cell))

# "Carolinian"
#####
   iReg <- which(uni_reg == "Carolinian")
   cell <- subset(cpue,cpue$ECO_REG == timeser[iReg,1])
   cell <- subset(cell,!(is.na(cell$biomass)))
   table(cell$year)
   
   cell_sub <- subset(cell,cell$year %in% c(1989:2019))
   areaplot <- subset(grid_master,grid_master@data$ECO_REG==timeser[iReg,1])
   
   #par(mfrow = c(3, 4))
   # for (j in 1989:2019){
   #   cell_plot <- subset(cell_sub,cell_sub$year == j)
   #   tt <- subset(grid_master,grid_master@data$uni %in% cell_plot$uni_cell)
   #   plot(areaplot, main = j)
   #   plot(tt,add=T, col="red")
   # }
   
   tall <- cell_sub[ cell_sub$uni_cell %in%  names(table(cell_sub$uni_cell))[table(cell_sub$uni_cell) > 15] , ]
   tt <- subset(grid_master,grid_master@data$uni %in% tall$uni_cell)
   #par(mfrow = c(1, 1))
   #plot(areaplot, main = "all")
   #plot(tt,add=T, col="blue")
   
   # table(tall$year) # good coverage from 1989-2019
   timeser[iReg,2] <- 1989
   timeser[iReg,3] <- 2019
   
   tall <- subset(tall,tall$year %in% c(timeser[iReg,2]:timeser[iReg,3]))
   degrees <- c(degrees,unique(tall$uni_cell))

# "Southern California Bight"
#####
   iReg <- which(uni_reg == "Southern California Bight")
   cell <- subset(cpue,cpue$ECO_REG == timeser[iReg,1])
   cell <- subset(cell,!(is.na(cell$biomass)))
   table(cell$year)
   
   cell_sub <- subset(cell,cell$year %in% c(2003:2018))
   areaplot <- subset(grid_master,grid_master@data$ECO_REG==timeser[iReg,1])
   
   # par(mfrow = c(3, 4))
   # for (j in 2003:2018){
   #  cell_plot <- subset(cell_sub,cell_sub$year == j)
   #   tt <- subset(grid_master,grid_master@data$uni %in% cell_plot$uni_cell)
   #   plot(areaplot, main = j)
   #   plot(tt,add=T, col="red")
   # }
   
   tall <- cell_sub[ cell_sub$uni_cell %in%  names(table(cell_sub$uni_cell))[table(cell_sub$uni_cell) > 8] , ]
   tt <- subset(grid_master,grid_master@data$uni %in% tall$uni_cell)
   #par(mfrow = c(1, 1))
   #plot(areaplot, main = "all")
   #plot(tt,add=T, col="blue")
   
   # table(tall$year) # good coverage from 2003-2018
   timeser[iReg,2] <- 2003
   timeser[iReg,3] <- 2018
   
   tall <- subset(tall,tall$year %in% c(timeser[iReg,2]:timeser[iReg,3]))
   degrees <- c(degrees,unique(tall$uni_cell))

# "Northern Gulf of Mexico"
#####
   iReg <- which(uni_reg == "Northern Gulf of Mexico")
   cell <- subset(cpue,cpue$ECO_REG == timeser[iReg,1])
   cell <- subset(cell,!(is.na(cell$biomass)))
   table(cell$year)
   
   cell_sub <- subset(cell,cell$year %in% c(1982:2019))
   areaplot <- subset(grid_master,grid_master@data$ECO_REG==timeser[iReg,1])
   
   # par(mfrow = c(3, 4))
   # for (j in 1982:2019){
   #   cell_plot <- subset(cell_sub,cell_sub$year == j)
   #   tt <- subset(grid_master,grid_master@data$uni %in% cell_plot$uni_cell)
   #   plot(areaplot, main = j)
   #   plot(tt,add=T, col="red")
   # }
   
   tall <- cell_sub[ cell_sub$uni_cell %in%  names(table(cell_sub$uni_cell))[table(cell_sub$uni_cell) > 19] , ]
   tt <- subset(grid_master,grid_master@data$uni %in% tall$uni_cell)
   #par(mfrow = c(1, 1))
   #plot(areaplot, main = "all")
   #plot(tt,add=T, col="blue")
   
   # table(tall$year) # good coverage from 1982-2019
   timeser[iReg,2] <- 1982
   timeser[iReg,3] <- 2019
   
   tall <- subset(tall,tall$year %in% c(timeser[iReg,2]:timeser[iReg,3]))
   degrees <- c(degrees,unique(tall$uni_cell))

# "Floridian"
#####
   iReg <- which(uni_reg ==  "Floridian")
   cell <- subset(cpue,cpue$ECO_REG == timeser[iReg,1])
   cell <- subset(cell,!(is.na(cell$biomass)))
   table(cell$year)
   
   cell_sub <- subset(cell,cell$year %in% c(2009:2019))
   areaplot <- subset(grid_master,grid_master@data$ECO_REG==timeser[iReg,1])
   
   # par(mfrow = c(3, 4))
   # for (j in 2009:2019){
   #   cell_plot <- subset(cell_sub,cell_sub$year == j)
   #   tt <- subset(grid_master,grid_master@data$uni %in% cell_plot$uni_cell)
   #   plot(areaplot, main = j)
   #   plot(tt,add=T, col="red")
   # }
   
   tall <- cell_sub[ cell_sub$uni_cell %in%  names(table(cell_sub$uni_cell))[table(cell_sub$uni_cell) > 6] , ]
   tt <- subset(grid_master,grid_master@data$uni %in% tall$uni_cell)
   #par(mfrow = c(1, 1))
   #plot(areaplot, main = "all")
   #plot(tt,add=T, col="blue")
   
   # table(tall$year) # good coverage from 2010-2019
   timeser[iReg,2] <- 2009
   timeser[iReg,3] <- 2019
   
   tall <- subset(tall,tall$year %in% c(timeser[iReg,2]:timeser[iReg,3]))
   degrees <- c(degrees,unique(tall$uni_cell))

# create plot with the time series area
#####
   # tt <- subset(grid_master,grid_master@data$uni %in% degrees)
   # plot(grid_master, main = "all")
   # plot(tt,add=T, col="blue")

rm(tt,tall,cell_sub,areaplot,iReg,cell,uni_reg)









