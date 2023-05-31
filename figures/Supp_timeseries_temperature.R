
load("processed data/surveyed_grid.RData")
grid <- grid_master@data[,c(1,4,6)]
grid <- do.call("rbind", replicate(length(1993:2015), grid, simplify = FALSE))
grid$year <- rep(1993:2015, each=nrow(grid_master@data))

# --------------------------------------------------------------------------------
# add SST information
# --------------------------------------------------------------------------------

load("processed data/sstdat_1967_2018_COBE.RData")  # obtain SST

grid <- subset(grid,grid$uni_cell %in% sstdat$uni_cell)

grid$SST <- NA
for(j in 1:nrow(grid)){
  coll <- which(grid$year[j] == colnames(sstdat))
  row  <- which(grid$uni_cell[j] == sstdat$uni_cell)
  if (length(coll)>0){
    grid$SST[j] <- sstdat[row,coll]
  }}


# --------------------------------------------------------------------------------
# add Btemp information
# --------------------------------------------------------------------------------
load("processed data/tdat_1993_2016_Glorys.RData")  # obtain bottom temp

grid$SBT <- NA
for(j in 1:nrow(grid)){
  coll <- which(grid$year[j] == colnames(tdat))
  row  <- which(grid$uni_cell[j] == tdat$uni_cell)
  if (length(coll)>0){
    grid$SBT[j] <- tdat[row,coll]
  }}

# combine per ecoregion and year
grid[,c(5,6)]  <- grid[,c(5,6)] * grid$ocean_sqkm # weighted with the size of the ocean area
cnam         <- colnames(grid[,c(2,5,6)])
grid           <-  aggregate(list(grid[,c(2,5,6)]),
                           by=list(grid$ECO_REG,grid$year),FUN=sum,na.rm=T)
colnames(grid) <- c("ECO_REG","year",cnam)
grid[,4:5]     <- grid[,4:5]/grid$ocean_sqkm


# 
grid  <-  grid %>% group_by(ECO_REG) %>% 
  mutate(SST  = SST - mean(SST),
         SBT  = SBT - mean(SBT))
regs <- unique(grid$ECO_REG)
pdf("figures/Timeseries_temperature.pdf",width=8.3,height=11.27)

regs1 <- subset(grid, grid$ECO_REG %in% regs[1:11])
regs2 <- subset(grid, grid$ECO_REG %in% regs[12:21])

left <- ggplot() + 
   geom_line(data=regs1, aes(x = year, y=SST)) + 
  ylab("Temperature changes over time")+ xlab("Year")+
   geom_line(data=regs1, aes(x = year, y=SBT),col="red") + 
   facet_grid(rows = vars(ECO_REG))+
  theme(plot.background=element_blank(),
        panel.background=element_blank(),
        axis.text.y   = element_text(size=11),
        axis.text.x   = element_text(size=11),
        axis.title.y  = element_text(size=11),
        axis.title.x  = element_text(size=11),
        strip.text.y = element_text(size = 8))

right <- ggplot() + 
  geom_line(data=regs2, aes(x = year, y=SST)) + 
  ylab("")+ xlab("Year")+
  geom_line(data=regs2, aes(x = year, y=SBT),col="red") + 
  facet_grid(rows = vars(ECO_REG))+
  theme(plot.background=element_blank(),
        panel.background=element_blank(),
        axis.text.y   = element_text(size=11),
        axis.text.x   = element_text(size=11),
        axis.title.y  = element_text(size=11),
        axis.title.x  = element_text(size=11),
        strip.text.y = element_text(size = 8))

pdf("figures/Supp_timeseries_temperature.pdf",width=8.3,height=11.27)
cowplot::plot_grid(left,right,nrow = 1)
dev.off()

par(mfrow=c(4,4))
for (j in 1:length(regs)){
  tsub <- subset(grid,grid$ECO_REG == regs[j])
  plot(tsub$SST~tsub$year,type="l",ylim=c(-2,2),main=regs[j],xlab="",ylab="",yaxt="n",cex.main=0.8)
  axis(2,c(-2,-1,0,1,2),las=1)
  lines(tsub$SBT~tsub$year,col="red")
}
dev.off()
