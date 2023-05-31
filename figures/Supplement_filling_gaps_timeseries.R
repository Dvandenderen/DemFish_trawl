
# --------------------------------------------------------------------------------
#  verifying the approach for filling in gaps of time series using data from 2000-2003
# --------------------------------------------------------------------------------

# run Obtain_data_for_regional_statistics_timeseries 
# up to "select all grid cells that are regularly sampled"

head(cpue)

cells <- subset(cpue,cpue$year %in% 2000:2003)
cells <- data.frame(uni_cell = unique(cells$uni_cell))

cells_y <- subset(cpue,cpue$year %in% 2000)
cells <- cbind(cells, cells_y[match(cells$uni_cell,cells_y$uni_cell), c("biomass")])    
colnames(cells)[ncol(cells)] <-"y_2000"

cells_y <- subset(cpue,cpue$year %in% 2001)
cells <- cbind(cells, cells_y[match(cells$uni_cell,cells_y$uni_cell), c("biomass")])    
colnames(cells)[ncol(cells)] <-"y_2001"

cells_y <- subset(cpue,cpue$year %in% 2002)
cells <- cbind(cells, cells_y[match(cells$uni_cell,cells_y$uni_cell), c("biomass")])    
colnames(cells)[ncol(cells)] <-"y_2002"

cells_y <- subset(cpue,cpue$year %in% 2003)
cells <- cbind(cells, cells_y[match(cells$uni_cell,cells_y$uni_cell), c("biomass")])    
colnames(cells)[ncol(cells)] <-"y_2003"

cells$onegap <- rowMeans(cells[,c(2,4)])

pdf("figures/Supplement_filling_gaps_timeseries.pdf",width=8,height=8) 

par(mfrow = c(2, 2))
plot(log10(cells$y_2001/1000),log10(cells$onegap/1000),
     xlab="log10(mean biomass 2000 and 2002)",ylab="log10(observed biomass in 2001)",
     las=1,ylim=c(-0.8,3.2),xlim=c(-0.8,3.2),main="One-year gap")
abline(0,1)
text(-0.5,3,"a)")

df <- cells[complete.cases(cells[ , c('y_2000', 'y_2002','y_2003')]), ] 
df <- cbind(df, grid_master@data[match(df$uni_cell,grid_master@data$uni_cell), c("ECO_REG")]) 
colnames(df)[ncol(df)] <- "ECO_REG"
df <- aggregate(list(df$y_2000,df$y_2002,df$y_2003),by=list(df$ECO_REG),FUN=mean)
colnames(df) <-c("ECO_REG",'y_2000av', 'y_2002av', 'y_2003av')

cells <- cbind(cells, grid_master@data[match(cells$uni_cell,grid_master@data$uni_cell), c("ECO_REG")]) 
colnames(cells)[ncol(cells)] <- "ECO_REG"
cells <- cbind(cells, df[match(cells$ECO_REG,df$ECO_REG), c(2:4)]) 

cells$twogap <- cells$y_2000 * cells$y_2000av/cells$y_2002av
cells$threegap <- cells$y_2000 * cells$y_2000av/cells$y_2003av

plot(log10(cells$y_2002/1000),log10(cells$twogap/1000),
     xlab="log10(inferred biomass in 2002 from 2000)",ylab="log10(observed biomass in 2002)",
     las=1,ylim=c(-0.8,3.2),xlim=c(-0.8,3.2),main="Two-year gap")
abline(0,1)
text(-0.5,3,"b)")

plot(log10(cells$y_2003/1000),log10(cells$threegap/1000),
     xlab="log10(inferred biomass in 2003 from 2000)",ylab="log10(observed biomass in 2003)",
     las=1,ylim=c(-0.8,3.2),xlim=c(-0.8,3.2),main="Three-year gap")
abline(0,1)
text(-0.5,3,"c)")

dev.off()
