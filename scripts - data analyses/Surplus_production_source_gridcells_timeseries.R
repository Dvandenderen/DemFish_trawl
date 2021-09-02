
# check per region the spatial consistency in trawl surveys
uni_reg <- unique(cpue$ECO_REG)
uni_reg <- subset(uni_reg,!(is.na(uni_reg)))
timeser <- data.frame(EcReg = uni_reg, start = NA, end= NA)
degrees <- c()

# "North and East Barents Sea"
iReg <- 1
cell <- subset(cpue,cpue$ECO_REG == timeser[iReg,1])
cell <- subset(cell,!(is.na(cell$biomass)))
# table(cell$year)

cell_sub <- subset(cell,cell$year %in% c(2001:2010))
areaplot <- subset(grid_master,grid_master@data$ECO_REG==timeser[iReg,1])

# par(mfrow = c(3, 4))
# for (j in 2001:2010){
# cell_plot <- subset(cell_sub,cell_sub$year == j)

# tt <- subset(grid_master,grid_master@data$uni %in% cell_plot$one_degrees)
# plot(areaplot, main = j)
# plot(tt,add=T, col="red")
# }

tall <- cell_sub[ cell_sub$one_degrees %in%  names(table(cell_sub$one_degrees))[table(cell_sub$one_degrees) >4] , ]
tt <- subset(grid_master,grid_master@data$uni %in% tall$one_degrees)
# plot(areaplot, main = "all")
# plot(tt,add=T, col="blue")

# table(tall$year) # good coverage from 2004-2010
timeser[iReg,2] <- 2004
timeser[iReg,3] <- 2010

tall <- subset(tall,tall$year %in% c(timeser[iReg,2]:timeser[iReg,3]))
degrees <- c(degrees,unique(tall$one_degrees))

### "Northern Norway and Finnmark"
iReg <- 2
cell <- subset(cpue,cpue$ECO_REG == timeser[iReg,1])
cell <- subset(cell,!(is.na(cell$biomass)))
# table(cell$year)

cell_sub <- subset(cell,cell$year %in% c(2000:2017))
areaplot <- subset(grid_master,grid_master@data$ECO_REG==timeser[iReg,1])

# par(mfrow = c(3, 4))
# for (j in 2000:2017){
#   cell_plot <- subset(cell_sub,cell_sub$year == j)
#   
#   tt <- subset(grid_master,grid_master@data$uni %in% cell_plot$one_degrees)
#   plot(areaplot, main = j)
#   plot(tt,add=T, col="red")
# }

tall <- cell_sub[ cell_sub$one_degrees %in%  names(table(cell_sub$one_degrees))[table(cell_sub$one_degrees) >8] , ]
tt <- subset(grid_master,grid_master@data$uni %in% tall$one_degrees)
#plot(areaplot, main = "all")
#plot(tt,add=T, col="blue")

# table(tall$year) # good coverage from 2000-2017
timeser[iReg,2] <- 2000
timeser[iReg,3] <- 2017

tall <- subset(tall,tall$year %in% c(timeser[iReg,2]:timeser[iReg,3]))
degrees <- c(degrees,unique(tall$one_degrees))

### "Southern Norway"
iReg <- 3
cell <- subset(cpue,cpue$ECO_REG == timeser[iReg,1])
cell <- subset(cell,!(is.na(cell$biomass)))
# table(cell$year)

cell_sub <- subset(cell,cell$year %in% c(2000:2017))
areaplot <- subset(grid_master,grid_master@data$ECO_REG==timeser[iReg,1])

# par(mfrow = c(3, 4))
# for (j in 2000:2017){
#   cell_plot <- subset(cell_sub,cell_sub$year == j)
#   
#   tt <- subset(grid_master,grid_master@data$uni %in% cell_plot$one_degrees)
#   plot(areaplot, main = j)
#   plot(tt,add=T, col="red")
# }

tall <- cell_sub[ cell_sub$one_degrees %in%  names(table(cell_sub$one_degrees))[table(cell_sub$one_degrees) >8] , ]
tt <- subset(grid_master,grid_master@data$uni %in% tall$one_degrees)
#plot(areaplot, main = "all")
#plot(tt,add=T, col="blue")

# table(tall$year) # okay coverage from 2000-2017
timeser[iReg,2] <- 2000
timeser[iReg,3] <- 2017

tall <- subset(tall,tall$year %in% c(timeser[iReg,2]:timeser[iReg,3]))
degrees <- c(degrees,unique(tall$one_degrees))

### "Eastern Bering Sea"
iReg <- 4
cell <- subset(cpue,cpue$ECO_REG == timeser[iReg,1])
cell <- subset(cell,!(is.na(cell$biomass)))
# table(cell$year)

cell_sub <- subset(cell,cell$year %in% c(1982:2019))
areaplot <- subset(grid_master,grid_master@data$ECO_REG==timeser[iReg,1])

# par(mfrow = c(3, 4))
# for (j in 1982:2019){
#   cell_plot <- subset(cell_sub,cell_sub$year == j)
  
#   tt <- subset(grid_master,grid_master@data$uni %in% cell_plot$one_degrees)
#   plot(areaplot, main = j)
#   plot(tt,add=T, col="red")
# }

tall <- cell_sub[ cell_sub$one_degrees %in%  names(table(cell_sub$one_degrees))[table(cell_sub$one_degrees) > 30] , ]
tt <- subset(grid_master,grid_master@data$uni %in% tall$one_degrees)
# plot(areaplot, main = "all")
# plot(tt,add=T, col="blue")

# table(tall$year) # good coverage from 1982-2019
timeser[iReg,2] <- 1982
timeser[iReg,3] <- 2019

tall <- subset(tall,tall$year %in% c(timeser[iReg,2]:timeser[iReg,3]))
degrees <- c(degrees,unique(tall$one_degrees))

### "North Sea"
iReg <- 5
cell <- subset(cpue,cpue$ECO_REG == timeser[iReg,1])
cell <- subset(cell,!(is.na(cell$biomass)))
# table(cell$year)

cell_sub <- subset(cell,cell$year %in% c(1980:2019))
areaplot <- subset(grid_master,grid_master@data$ECO_REG==timeser[iReg,1])

# par(mfrow = c(3, 4))
# for (j in 1980:2019){
#   cell_plot <- subset(cell_sub,cell_sub$year == j)
#   
#   tt <- subset(grid_master,grid_master@data$uni %in% cell_plot$one_degrees)
#   plot(areaplot, main = j)
#   plot(tt,add=T, col="red")
# }

tall <- cell_sub[ cell_sub$one_degrees %in%  names(table(cell_sub$one_degrees))[table(cell_sub$one_degrees) > 30] , ]
tt <- subset(grid_master,grid_master@data$uni %in% tall$one_degrees)
# plot(areaplot, main = "all")
# plot(tt,add=T, col="blue")

# table(tall$year) # good coverage from 1985-2019
timeser[iReg,2] <- 1985
timeser[iReg,3] <- 2019

tall <- subset(tall,tall$year %in% c(timeser[iReg,2]:timeser[iReg,3]))
degrees <- c(degrees,unique(tall$one_degrees))

### "Gulf of Alaska"
iReg <- 6
cell <- subset(cpue,cpue$ECO_REG == timeser[iReg,1])
cell <- subset(cell,!(is.na(cell$biomass)))
# table(cell$year)

cell_sub <- subset(cell,cell$year %in% c(1984:2019))
areaplot <- subset(grid_master,grid_master@data$ECO_REG==timeser[iReg,1])

# par(mfrow = c(3, 4))
# for (j in 1984:2019){
#   cell_plot <- subset(cell_sub,cell_sub$year == j)
#   
#   tt <- subset(grid_master,grid_master@data$uni %in% cell_plot$one_degrees)
#   plot(areaplot, main = j)
#   plot(tt,add=T, col="red")
# }

tall <- cell_sub[ cell_sub$one_degrees %in%  names(table(cell_sub$one_degrees))[table(cell_sub$one_degrees) > 8] , ]
tt <- subset(grid_master,grid_master@data$uni %in% tall$one_degrees)
# plot(areaplot, main = "all")
# plot(tt,add=T, col="blue")

# table(tall$year) # good coverage from 1984-2019 (note data gaps)
timeser[iReg,2] <- 1984
timeser[iReg,3] <- 2019

tall <- subset(tall,tall$year %in% c(timeser[iReg,2]:timeser[iReg,3]))
degrees <- c(degrees,unique(tall$one_degrees))

### "Celtic Seas"
iReg <- 7
cell <- subset(cpue,cpue$ECO_REG == timeser[iReg,1])
cell <- subset(cell,!(is.na(cell$biomass)))
# table(cell$year)

cell_sub <- subset(cell,cell$year %in% c(2000:2019)) # irregular spatial sampling, 
areaplot <- subset(grid_master,grid_master@data$ECO_REG==timeser[iReg,1])

# par(mfrow = c(3, 4))
# for (j in 2000:2019){
#   cell_plot <- subset(cell_sub,cell_sub$year == j)
#   
#   tt <- subset(grid_master,grid_master@data$uni %in% cell_plot$one_degrees)
#   plot(areaplot, main = j)
#   plot(tt,add=T, col="red")
# }

tall <- cell_sub[ cell_sub$one_degrees %in%  names(table(cell_sub$one_degrees))[table(cell_sub$one_degrees) > 10] , ]
tt <- subset(grid_master,grid_master@data$uni %in% tall$one_degrees)
# plot(areaplot, main = "all")
# plot(tt,add=T, col="blue")

# table(tall$year) # good coverage from 2000-2019 (note data gaps)
timeser[iReg,2] <- 2000
timeser[iReg,3] <- 2019

tall <- subset(tall,tall$year %in% c(timeser[iReg,2]:timeser[iReg,3]))
degrees <- c(degrees,unique(tall$one_degrees))

### "Baltic Sea"
iReg <- 8
cell <- subset(cpue,cpue$ECO_REG == timeser[iReg,1])
cell <- subset(cell,!(is.na(cell$biomass)))
# table(cell$year)

cell_sub <- subset(cell,cell$year %in% c(2000:2019))  
areaplot <- subset(grid_master,grid_master@data$ECO_REG==timeser[iReg,1])

# par(mfrow = c(3, 4))
# for (j in 2000:2019){
#   cell_plot <- subset(cell_sub,cell_sub$year == j)
#   
#   tt <- subset(grid_master,grid_master@data$uni %in% cell_plot$one_degrees)
#   plot(areaplot, main = j)
#   plot(tt,add=T, col="red")
# }

tall <- cell_sub[ cell_sub$one_degrees %in%  names(table(cell_sub$one_degrees))[table(cell_sub$one_degrees) > 10] , ]
tt <- subset(grid_master,grid_master@data$uni %in% tall$one_degrees)
# plot(areaplot, main = "all")
# plot(tt,add=T, col="blue")

# table(tall$year) # good coverage from 1984-2019 (note data gaps)
timeser[iReg,2] <- 2000
timeser[iReg,3] <- 2019

tall <- subset(tall,tall$year %in% c(timeser[iReg,2]:timeser[iReg,3]))
degrees <- c(degrees,unique(tall$one_degrees))

###  "North American Pacific Fijordland" # two seperate surveys - make sure with production
iReg <- 9
cell <- subset(cpue,cpue$ECO_REG == timeser[iReg,1])
cell <- subset(cell,!(is.na(cell$biomass)))
# table(cell$year)

cell_sub <- subset(cell,cell$year %in% c(1984:2019))
areaplot <- subset(grid_master,grid_master@data$ECO_REG==timeser[iReg,1])

# par(mfrow = c(3, 4))
# for (j in 1984:2019){
#   cell_plot <- subset(cell_sub,cell_sub$year == j)
#   
#   tt <- subset(grid_master,grid_master@data$uni %in% cell_plot$one_degrees)
#   plot(areaplot, main = j)
#   plot(tt,add=T, col="red")
# }

tall <- cell_sub[ cell_sub$one_degrees %in%  names(table(cell_sub$one_degrees))[table(cell_sub$one_degrees) > 5] , ]
tt <- subset(grid_master,grid_master@data$uni %in% tall$one_degrees)
# plot(areaplot, main = "all")
# plot(tt,add=T, col="blue")

# table(tall$year) # good coverage from 2003-2019 (note data gaps)
timeser[iReg,2] <- 2003
timeser[iReg,3] <- 2019

tall <- subset(tall,tall$year %in% c(timeser[iReg,2]:timeser[iReg,3]))
degrees <- c(degrees,unique(tall$one_degrees))

###  "Aleutian Islands" # two seperate surveys - make sure with production
iReg <- 10
cell <- subset(cpue,cpue$ECO_REG == timeser[iReg,1])
cell <- subset(cell,!(is.na(cell$biomass)))
# table(cell$year)

cell_sub <- subset(cell,cell$year %in% c(1983:2019))
areaplot <- subset(grid_master,grid_master@data$ECO_REG==timeser[iReg,1])

# par(mfrow = c(3, 4))
# for (j in 1983:2019){
#   cell_plot <- subset(cell_sub,cell_sub$year == j)
#   
#   tt <- subset(grid_master,grid_master@data$uni %in% cell_plot$one_degrees)
#   plot(areaplot, main = j)
#   plot(tt,add=T, col="red")
# }

tall <- cell_sub[ cell_sub$one_degrees %in%  names(table(cell_sub$one_degrees))[table(cell_sub$one_degrees) > 5] , ]
tt <- subset(grid_master,grid_master@data$uni %in% tall$one_degrees)
# plot(areaplot, main = "all")
# plot(tt,add=T, col="blue")

# table(tall$year) # good coverage from 1983-2019 (note data gaps)
timeser[iReg,2] <- 1983
timeser[iReg,3] <- 2019

tall <- subset(tall,tall$year %in% c(timeser[iReg,2]:timeser[iReg,3]))
degrees <- c(degrees,unique(tall$one_degrees))

###  "Gulf of St. Lawrence - Eastern Scotian Shelf" 
iReg <- 11
cell <- subset(cpue,cpue$ECO_REG == timeser[iReg,1])
cell <- subset(cell,!(is.na(cell$biomass)))
# table(cell$year)

cell_sub <- subset(cell,cell$year %in% c(1983:2019))
areaplot <- subset(grid_master,grid_master@data$ECO_REG==timeser[iReg,1])

# par(mfrow = c(3, 4))
# for (j in 1983:2019){
#   cell_plot <- subset(cell_sub,cell_sub$year == j)
#   
#   tt <- subset(grid_master,grid_master@data$uni %in% cell_plot$one_degrees)
#   plot(areaplot, main = j)
#   plot(tt,add=T, col="red")
# }

tall <- cell_sub[ cell_sub$one_degrees %in%  names(table(cell_sub$one_degrees))[table(cell_sub$one_degrees) > 15] , ]
tt <- subset(grid_master,grid_master@data$uni %in% tall$one_degrees)
# plot(areaplot, main = "all")
# plot(tt,add=T, col="blue")

# table(tall$year) # good coverage from 1987-2019 
timeser[iReg,2] <- 1987
timeser[iReg,3] <- 2019

tall <- subset(tall,tall$year %in% c(timeser[iReg,2]:timeser[iReg,3]))
degrees <- c(degrees,unique(tall$one_degrees))

###  "Oregon, Washington, Vancouver Coast and Shelf" 
iReg <- 12
cell <- subset(cpue,cpue$ECO_REG == timeser[iReg,1])
cell <- subset(cell,!(is.na(cell$biomass)))
# table(cell$year)

cell_sub <- subset(cell,cell$year %in% c(1977:2018))
areaplot <- subset(grid_master,grid_master@data$ECO_REG==timeser[iReg,1])

# par(mfrow = c(3, 4))
# for (j in 1977:2018){
#   cell_plot <- subset(cell_sub,cell_sub$year == j)
#   
#   tt <- subset(grid_master,grid_master@data$uni %in% cell_plot$one_degrees)
#   plot(areaplot, main = j)
#   plot(tt,add=T, col="red")
# }

tall <- cell_sub[ cell_sub$one_degrees %in%  names(table(cell_sub$one_degrees))[table(cell_sub$one_degrees) > 10] , ]
tt <- subset(grid_master,grid_master@data$uni %in% tall$one_degrees)
# plot(areaplot, main = "all")
# plot(tt,add=T, col="blue")

# table(tall$year) # good coverage from 1980-2018 data gaps 
timeser[iReg,2] <- 1980
timeser[iReg,3] <- 2018

tall <- subset(tall,tall$year %in% c(timeser[iReg,2]:timeser[iReg,3]))
degrees <- c(degrees,unique(tall$one_degrees))

###  "South European Atlantic Shelf"
iReg <- 13
cell <- subset(cpue,cpue$ECO_REG == timeser[iReg,1])
cell <- subset(cell,!(is.na(cell$biomass)))
# table(cell$year)

cell_sub <- subset(cell,cell$year %in% c(1997:2014))
areaplot <- subset(grid_master,grid_master@data$ECO_REG==timeser[iReg,1])

# par(mfrow = c(3, 4))
# for (j in 1997:2014){
#   cell_plot <- subset(cell_sub,cell_sub$year == j)
#   
#   tt <- subset(grid_master,grid_master@data$uni %in% cell_plot$one_degrees)
#   plot(areaplot, main = j)
#   plot(tt,add=T, col="red")
# }

tall <- cell_sub[ cell_sub$one_degrees %in%  names(table(cell_sub$one_degrees))[table(cell_sub$one_degrees) > 8] , ]
tt <- subset(grid_master,grid_master@data$uni %in% tall$one_degrees)
# plot(areaplot, main = "all")
# plot(tt,add=T, col="blue")

# table(tall$year) # good coverage from 1997-2014
timeser[iReg,2] <- 1997
timeser[iReg,3] <- 2014

tall <- subset(tall,tall$year %in% c(timeser[iReg,2]:timeser[iReg,3]))
degrees <- c(degrees,unique(tall$one_degrees))

###   "Gulf of Maine/Bay of Fundy"
iReg <- 14
cell <- subset(cpue,cpue$ECO_REG == timeser[iReg,1])
cell <- subset(cell,!(is.na(cell$biomass)))
# table(cell$year)

cell_sub <- subset(cell,cell$year %in% c(1996:2019))
areaplot <- subset(grid_master,grid_master@data$ECO_REG==timeser[iReg,1])

# par(mfrow = c(3, 4))
# for (j in 1996:2019){
#   cell_plot <- subset(cell_sub,cell_sub$year == j)
#   
#   tt <- subset(grid_master,grid_master@data$uni %in% cell_plot$one_degrees)
#   plot(areaplot, main = j)
#   plot(tt,add=T, col="red")
# }

tall <- cell_sub[ cell_sub$one_degrees %in%  names(table(cell_sub$one_degrees))[table(cell_sub$one_degrees) > 6] , ]
tt <- subset(grid_master,grid_master@data$uni %in% tall$one_degrees)
# plot(areaplot, main = "all")
# plot(tt,add=T, col="blue")

# table(tall$year) # good coverage from 1996-2019
timeser[iReg,2] <- 1996
timeser[iReg,3] <- 2019

tall <- subset(tall,tall$year %in% c(timeser[iReg,2]:timeser[iReg,3]))
degrees <- c(degrees,unique(tall$one_degrees))

###  "Scotian Shelf"
iReg <- 15
cell <- subset(cpue,cpue$ECO_REG == timeser[iReg,1])
cell <- subset(cell,!(is.na(cell$biomass)))
# table(cell$year)

cell_sub <- subset(cell,cell$year %in% c(1970:2020))
areaplot <- subset(grid_master,grid_master@data$ECO_REG==timeser[iReg,1])

# par(mfrow = c(3, 4))
# for (j in 1970:2020){
#   cell_plot <- subset(cell_sub,cell_sub$year == j)
#   
#   tt <- subset(grid_master,grid_master@data$uni %in% cell_plot$one_degrees)
#   plot(areaplot, main = j)
#   plot(tt,add=T, col="red")
# }

tall <- cell_sub[ cell_sub$one_degrees %in%  names(table(cell_sub$one_degrees))[table(cell_sub$one_degrees) > 15] , ]
tt <- subset(grid_master,grid_master@data$uni %in% tall$one_degrees)
# plot(areaplot, main = "all")
# plot(tt,add=T, col="blue")

# table(tall$year) # good coverage from 1970-2019
timeser[iReg,2] <- 1970
timeser[iReg,3] <- 2019

tall <- subset(tall,tall$year %in% c(timeser[iReg,2]:timeser[iReg,3]))
degrees <- c(degrees,unique(tall$one_degrees))

###  "Virginian"
iReg <- 16
cell <- subset(cpue,cpue$ECO_REG == timeser[iReg,1])
cell <- subset(cell,!(is.na(cell$biomass)))
# table(cell$year)

cell_sub <- subset(cell,cell$year %in% c(1996:2019))
areaplot <- subset(grid_master,grid_master@data$ECO_REG==timeser[iReg,1])

# par(mfrow = c(3, 4))
# for (j in 1996:2019){
#   cell_plot <- subset(cell_sub,cell_sub$year == j)
#   
#   tt <- subset(grid_master,grid_master@data$uni %in% cell_plot$one_degrees)
#   plot(areaplot, main = j)
#   plot(tt,add=T, col="red")
# }

tall <- cell_sub[ cell_sub$one_degrees %in%  names(table(cell_sub$one_degrees))[table(cell_sub$one_degrees) > 8] , ]
tt <- subset(grid_master,grid_master@data$uni %in% tall$one_degrees)
# plot(areaplot, main = "all")
# plot(tt,add=T, col="blue")

# table(tall$year) # good coverage from 1996-2019
timeser[iReg,2] <- 1996
timeser[iReg,3] <- 2019

tall <- subset(tall,tall$year %in% c(timeser[iReg,2]:timeser[iReg,3]))
degrees <- c(degrees,unique(tall$one_degrees))

### "Northern California"
iReg <- 17
cell <- subset(cpue,cpue$ECO_REG == timeser[iReg,1])
cell <- subset(cell,!(is.na(cell$biomass)))
# table(cell$year)

cell_sub <- subset(cell,cell$year %in% c(1989:2018))
areaplot <- subset(grid_master,grid_master@data$ECO_REG==timeser[iReg,1])

# par(mfrow = c(3, 4))
# for (j in 1989:2018){
#   cell_plot <- subset(cell_sub,cell_sub$year == j)
#   
#   tt <- subset(grid_master,grid_master@data$uni %in% cell_plot$one_degrees)
#   plot(areaplot, main = j)
#   plot(tt,add=T, col="red")
# }

tall <- cell_sub[ cell_sub$one_degrees %in%  names(table(cell_sub$one_degrees))[table(cell_sub$one_degrees) > 12] , ]
tt <- subset(grid_master,grid_master@data$uni %in% tall$one_degrees)
# plot(areaplot, main = "all")
# plot(tt,add=T, col="blue")

# table(tall$year) # good coverage from 1989-2018 with gaps
timeser[iReg,2] <- 1989
timeser[iReg,3] <- 2018

tall <- subset(tall,tall$year %in% c(timeser[iReg,2]:timeser[iReg,3]))
degrees <- c(degrees,unique(tall$one_degrees))

### "Carolinian"
iReg <- 18
cell <- subset(cpue,cpue$ECO_REG == timeser[iReg,1])
cell <- subset(cell,!(is.na(cell$biomass)))
# table(cell$year)

cell_sub <- subset(cell,cell$year %in% c(1989:2019))
areaplot <- subset(grid_master,grid_master@data$ECO_REG==timeser[iReg,1])

# par(mfrow = c(3, 4))
# for (j in 1989:2019){
#   cell_plot <- subset(cell_sub,cell_sub$year == j)
#   
#   tt <- subset(grid_master,grid_master@data$uni %in% cell_plot$one_degrees)
#   plot(areaplot, main = j)
#   plot(tt,add=T, col="red")
# }

tall <- cell_sub[ cell_sub$one_degrees %in%  names(table(cell_sub$one_degrees))[table(cell_sub$one_degrees) > 12] , ]
tt <- subset(grid_master,grid_master@data$uni %in% tall$one_degrees)
# plot(areaplot, main = "all")
# plot(tt,add=T, col="blue")

# table(tall$year) # good coverage from 1989-2019
timeser[iReg,2] <- 1989
timeser[iReg,3] <- 2019

tall <- subset(tall,tall$year %in% c(timeser[iReg,2]:timeser[iReg,3]))
degrees <- c(degrees,unique(tall$one_degrees))

### "Southern California Bight"
iReg <- 19
cell <- subset(cpue,cpue$ECO_REG == timeser[iReg,1])
cell <- subset(cell,!(is.na(cell$biomass)))
# table(cell$year)

cell_sub <- subset(cell,cell$year %in% c(2003:2018))
areaplot <- subset(grid_master,grid_master@data$ECO_REG==timeser[iReg,1])

# par(mfrow = c(3, 4))
# for (j in 2003:2018){
#   cell_plot <- subset(cell_sub,cell_sub$year == j)
#   
#   tt <- subset(grid_master,grid_master@data$uni %in% cell_plot$one_degrees)
#   plot(areaplot, main = j)
#   plot(tt,add=T, col="red")
# }

tall <- cell_sub[ cell_sub$one_degrees %in%  names(table(cell_sub$one_degrees))[table(cell_sub$one_degrees) > 8] , ]
tt <- subset(grid_master,grid_master@data$uni %in% tall$one_degrees)
# plot(areaplot, main = "all")
# plot(tt,add=T, col="blue")

# table(tall$year) # good coverage from 2003-2018
timeser[iReg,2] <- 2003
timeser[iReg,3] <- 2018

tall <- subset(tall,tall$year %in% c(timeser[iReg,2]:timeser[iReg,3]))
degrees <- c(degrees,unique(tall$one_degrees))

### "Northern Gulf of Mexico"
iReg <- 20
cell <- subset(cpue,cpue$ECO_REG == timeser[iReg,1])
cell <- subset(cell,!(is.na(cell$biomass)))
# table(cell$year)

cell_sub <- subset(cell,cell$year %in% c(1982:2019))
areaplot <- subset(grid_master,grid_master@data$ECO_REG==timeser[iReg,1])

# par(mfrow = c(3, 4))
# for (j in 1982:2019){
#   cell_plot <- subset(cell_sub,cell_sub$year == j)
#   
#   tt <- subset(grid_master,grid_master@data$uni %in% cell_plot$one_degrees)
#   plot(areaplot, main = j)
#   plot(tt,add=T, col="red")
# }

tall <- cell_sub[ cell_sub$one_degrees %in%  names(table(cell_sub$one_degrees))[table(cell_sub$one_degrees) > 12] , ]
tt <- subset(grid_master,grid_master@data$uni %in% tall$one_degrees)
# plot(areaplot, main = "all")
# plot(tt,add=T, col="blue")

# table(tall$year) # good coverage from 1982-2019
timeser[iReg,2] <- 1982
timeser[iReg,3] <- 2019

tall <- subset(tall,tall$year %in% c(timeser[iReg,2]:timeser[iReg,3]))
degrees <- c(degrees,unique(tall$one_degrees))

### "Floridian"
iReg <- 21
cell <- subset(cpue,cpue$ECO_REG == timeser[iReg,1])
cell <- subset(cell,!(is.na(cell$biomass)))
# table(cell$year)

cell_sub <- subset(cell,cell$year %in% c(2010:2019))
areaplot <- subset(grid_master,grid_master@data$ECO_REG==timeser[iReg,1])

# par(mfrow = c(3, 4))
# for (j in 2010:2019){
#   cell_plot <- subset(cell_sub,cell_sub$year == j)
#  
#  tt <- subset(grid_master,grid_master@data$uni %in% cell_plot$one_degrees)
#   plot(areaplot, main = j)
#   plot(tt,add=T, col="red")
# }

tall <- cell_sub[ cell_sub$one_degrees %in%  names(table(cell_sub$one_degrees))[table(cell_sub$one_degrees) > 5] , ]
tt <- subset(grid_master,grid_master@data$uni %in% tall$one_degrees)
# plot(areaplot, main = "all")
# plot(tt,add=T, col="blue")

# table(tall$year) # good coverage from 2010-2019
timeser[iReg,2] <- 2010
timeser[iReg,3] <- 2019

tall <- subset(tall,tall$year %in% c(timeser[iReg,2]:timeser[iReg,3]))
degrees <- c(degrees,unique(tall$one_degrees))

# tt <- subset(grid_master,grid_master@data$uni %in% degrees)
# plot(grid_master, main = "all")
# plot(tt,add=T, col="blue")

rm(tt,tall,cell_sub,areaplot,iReg,cell,uni_reg)

