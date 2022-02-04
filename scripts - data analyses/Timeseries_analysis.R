
rm(list=ls())

library(dplyr)
library(nlme)

load("cleaned data/final_timeseries_grid.RData")

## get weigthed mean based on size of ocean area
cpue_good[,c(3,4,8,9)]  <- cpue_good[,c(3,4,8,9)] * cpue_good$ocean_sqkm # weighted with the size of the ocean area

tt <-  aggregate(list(cpue_good[,c(7,3,4,8,9)]),
                 by=list(cpue_good$ECO_REG,cpue_good$year),FUN=sum,na.rm=T)
cnam <- colnames(cpue_good[,c(7,3,4,8,9)])
colnames(tt) <- c("ECO_REG","year",cnam)
tt[,4:7] <- tt[,4:7]/tt$ocean_sqkm

## do the same for SST, but first remove NAs
cpue_good <- subset(cpue_good,!(is.na(cpue_good$SST)))

cpue_good[,c(10)]  <- cpue_good[,c(10)] * cpue_good$ocean_sqkm # weighted with the size of the ocean area

tt2 <-  aggregate(list(cpue_good[,c(7,10)]),
                 by=list(cpue_good$ECO_REG,cpue_good$year),FUN=sum,na.rm=T)
cnam <- colnames(cpue_good[,c(7,10)])
colnames(tt2) <- c("ECO_REG","year",cnam)
tt2[,4] <- tt2[,4]/tt2$ocean_sqkm

## combine
tt$uni  <- paste(tt$ECO_REG,tt$year)
tt2$uni <-  paste(tt2$ECO_REG,tt2$year)
tt <- cbind(tt,tt2[match(tt$uni,tt2$uni),c("SST")])
colnames(tt)[ncol(tt)]<-"SST"

tt <- subset(tt,tt$year %in% c(1980:2015))
tt$ER <- tt$Catch_sqkm/tt$biomass

# get log 
tt$LLER  <- log10(tt$ER)

# get across species temperature (average per species)
temp <- tt %>% 
  group_by(ECO_REG) %>%
  summarise_at(c("tlw","SST","LLER"), mean, na.rm = TRUE)
temp <- as.data.frame(temp)
colnames(temp)[2:4] <- c("tlw_across","SST_across","LLER_across")

tt <- cbind(tt, temp[match(tt$ECO_REG,temp$ECO_REG), c(2:4)])

# get within species temperature difference (obs - average per species)
tt$tlw_within <- tt$tlw - tt$tlw_across
tt$SST_within <- tt$SST - tt$SST_across
tt$LLER_within <- tt$LLER - tt$LLER_across

library(lattice)
xyplot(tt$biomass ~ tt$year | tt$ECO_REG, col = 1, type="o")

# fit model with temporal autocorrelation nested within ecoregion
####################################################  

# parameter estimation of within and across region model  
fit1 <- lme(biomass ~ SST_within + SST_across +  tlw_within + tlw_across +  LLER_within + LLER_across, 
            random= ~ 1|ECO_REG, data=tt)

# parameter estimation of within and across region model with a residual auto-correlation structure
fit2 <- lme(biomass ~ SST_within + SST_across +  tlw_within + tlw_across +  LLER_within + LLER_across, 
                random= ~ 1|ECO_REG, 
                corAR1(form =~ year|ECO_REG), data=tt)

# parameter estimation of within and across region model with a residual auto-correlation structure representing 
# auto-regressive moving average (ARMA). the residuals at time s are modelled as a function of the 
# residuals of the p previous time points and white noise
# realise that all these p and q parameters have to be estimated from the data, and in our experience (Zuur et al),
# using values of p or q larger than 2 or 3 tend to give error messages related to
# convergence problems

cs1 <- corARMA(c(0.1,-0.1), p = 2, q = 0,form =~ year|ECO_REG)
fit3 <- lme(biomass ~ SST_within + SST_across +  tlw_within + tlw_across +  LLER_within + LLER_across, 
            random= ~ 1|ECO_REG, 
            correlation=cs1, data=tt)
AIC(fit1,fit2,fit3)

E <- residuals(fit3, type = "normalized")
acf(E)
    
# now see if there is statistical difference between slopes of within and between region model
fit3_contrast <- lme(biomass ~ SST + SST_across +  tlw + tlw_across +  LLER + LLER_across, 
                      random= ~ 1|ECO_REG, correlation=cs1, data=tt)

#constrast shows that fishing effect has same slope within and across
#SST has significant different slopw within and across

# see here:
# van der Pol 2009
# Since all these statistical models test for each
# effect while controlling for all other effects in the model, when this
# third model is assessing xij it is simply testing again for any within subject
# effect, because it is already controlling for any between subject
# effect caused by xj. The within-subject effects (bW) in the
# second and third models will therefore be identical. More importantly,
# the between-subject effect in this third model (xj) now
# actually represents the difference between the between- and
# within-subject effects (bB x bW) in the second model. Thus the
#estimate of bB x bW is expected to be close to zero and nonsignificant
# when the within- and between-subject effects are effectively
#the same. 

# now see if there is a random slope of SST 
fit4_a <- lme(biomass ~ SST_within + SST_across +  LLER, 
            random= ~ 1 |ECO_REG, correlation=cs1, data=tt)

fit4_b <- lme(biomass ~ SST_within + SST_across +  LLER, 
                     random= ~ 1 + SST_within|ECO_REG, correlation=cs1, data=tt)
rand <- ranef(fit4_b); rand$ECO_REG <- rownames(rand)

temp <- cbind(temp,rand[match(temp$ECO_REG,rand$ECO_REG),c("SST_within")])
colnames(temp)[5] <- "slope"
temp[order(temp$slope),]

# plot within ecoregion temperature variation
load("cleaned data/surveyed_grid.RData") # get grid information
grid_master <- cbind(grid_master,cpue_good[match(grid_master@data$uni_cell,cpue_good$uni_cell),c("biomass")])
colnames(grid_master@data)[ncol(grid_master@data)] <- "biomass"

grid_master <- cbind(grid_master,temp[match(grid_master@data$ECO_REG,temp$ECO_REG),c("slope")])
colnames(grid_master@data)[ncol(grid_master@data)] <- "slope"

ncoords <- subset(grid_master,!(is.na(grid_master@data$biomass)))

ctrys <- rnaturalearth::ne_countries(scale = 50, returnclass = "sf")
minlong <- -180 #round(min(filedata$long)-1)
maxlong <- 51
minlat  <- 23
maxlat  <- 83
coordslim <- c(minlong,maxlong,minlat,maxlat)
coordxmap <- round(seq(minlong,maxlong,length.out = 4))
coordymap <- round(seq(minlat,maxlat,length.out = 4))

nco <- sf::st_as_sf(ncoords)

shape <- readOGR(dsn = "C:/Users/danie/Dropbox/Werk/Demersal fish and fisheries/Data analysis/MEOW shapefiles" ,layer="meow_ecos")
shape <- subset(shape,shape@data$ECOREGION %in% temp$ECO_REG)

# plot map
figmap <- ggplot(nco) + 
  geom_sf( aes(fill=slope), colour = NA ) + 
  scale_fill_viridis(name="Temperature slope") +
  geom_sf(data = ctrys, fill="grey",colour=NA) 

figmap <-  figmap +  theme(plot.background=element_blank(),
                           panel.background=element_blank(),
                           axis.text.y   = element_text(size=11),
                           axis.text.x   = element_text(size=11),
                           axis.title.y  = element_blank(),
                           axis.title.x  = element_blank(),
                           panel.border  = element_rect(colour = "grey", size=.5,fill=NA),
                           legend.text   = element_text(size=11),
                           legend.title  = element_text(size=11))+
  coord_sf(xlim = c(-180, 51), ylim = c(20, 84), expand = FALSE) +
  scale_x_continuous(breaks=coordxmap)  +
  scale_y_continuous(breaks=coordymap)

figmap  <- figmap +  geom_polygon(data = shape, aes(x = long, y = lat, group = group),color="grey",fill=NA)

pdf("figures/biomass_within_region_slope.pdf",width=10,height=4) 
grid.arrange(figmap, nrow = 1,
             left = "Latitude",bottom = "Longitude")
dev.off()

# plot the predicted line
#Value Std.Error  DF    t-value p-value
#(Intercept)  23293.194  8228.554 419   2.830776  0.0049
#SST_within    2847.498  1318.665 419   2.159379  0.0314
#SST_across   -1626.190   569.450  19  -2.855722  0.0101
#LLER        -19302.316  1574.117 419 -12.262312  0.0000

sst_across <- c(min(temp$SST_across),max(temp$SST_across))
bio <- 23293.194 + -1626.190 *sst_across -19302.316 * log10(0.02) + 2847.498*0
plot(bio/1000~sst_across,type="l",lwd=2,ylim=c(0,110),xlab="Temperature",ylab="Biomass",las=1,col="blue")

loc <- c("p","n","e","n","p","e","p","e","e","p","n","n","p","e","n","p","e","n","p","n","e")

for ( j in 1:nrow(rand)){
  dd <- subset(tt,tt$ECO_REG == rand$ECO_REG[j])
  SST_within <- c(min(dd$SST_within),max(dd$SST_within))
  if (length(unique(SST_within)) > 1){
    T_across <- rep(dd$SST_across[1],2)
    LLER <- rep(log10(0.02),2)
    yn <- 23293.194  + rand$`(Intercept)`[j] +-1626.190 *T_across +
      2847.498*SST_within + rand$SST_within[j] * SST_within -19302.316*LLER 
    Tall <- c(min(dd$SST),max(dd$SST))
    if(loc[j] == "p"){
      lines(yn/1000~Tall,col="grey",lwd=1)
    } else {
      lines(yn/1000~Tall,col="grey",lwd=1)
    }
  }
}


######
# do data gaps matter for the within response?
# select all regions with good time series
# within effect seems to be consistent

ttnew <- subset(tt, tt$ECO_REG %in% c("North Sea", "Scotian Shelf","Eastern Bering Sea","Northern Gulf of Mexico",
                                      "Oregon, Washington, Vancouver Coast and Shelf","Carolinian") )

cs1 <- corARMA(c(0.1,-0.1), p = 2, q = 0,form =~ year|ECO_REG)
fit3 <- lme(biomass ~ SST_within + SST_across +  tlw_within + tlw_across +  LLER_within + LLER_across, 
            random= ~ 1|ECO_REG, 
            correlation=cs1, data=ttnew)

# now see if there is statistical difference between slopes of within and between region model
fit3_contrast <- lme(biomass ~ SST + SST_across +  tlw + tlw_across +  LLER + LLER_across, 
                     random= ~ 1|ECO_REG, correlation=cs1, data=ttnew)

#######################################################
######
# now can we fit a recurring model?
######
ttnew <- subset(tt, tt$ECO_REG %in% c("Eastern Bering Sea"))

trcFunc <- function(SST,MTL,Bio,Catch,a,g,b,d){((1+a-g*SST-d*MTL-b*Bio)*Bio - Catch)}

SST <- ttnew$SST_within[1:33] 
MTL <- ttnew$tlw[1:33]
Catch <-  ttnew$Catch_sqkm[1:33]
Bio <- ttnew$biomass[1:33]
y <- ttnew$biomass[2:34]

mfit <- nls(y~trcFunc(SST,MTL,Bio,Catch,a,g,b,d),start=list(a=1,b=0.1,g=0.1,d=0.1))

# with or without b term? 
trcFunc <- function(SST,MTL,Bio,Catch,a,g,d){((1+a-g*SST-d*MTL)*Bio - Catch)}
mfit2 <- nls(y~trcFunc(SST,MTL,Bio,Catch,a,g,d),start=list(a=1,g=0.1,d=0.1))

# make mixed model
ttnew <- subset(tt, tt$ECO_REG %in% c("North Sea","Eastern Bering Sea"))
ttnew <- subset(ttnew,ttnew$year > 1981)

ttnew <- ttnew[order(ttnew$ECO_REG, ttnew$year),]
ttnew$Bionext <- c(ttnew$biomass[2:34],NA,ttnew$biomass[36:68],NA)

library(nlme)

trcFunc <- function(SST,tlw,biomass,Catch_sqkm,a,g,b,d){((1+a-g*SST-d*tlw-b*biomass)*biomass - Catch_sqkm)}

ttnew <- ttnew[complete.cases(ttnew$Bionext), ]
ttnew$ECO_REG <- as.factor(ttnew$ECO_REG)

p20 = gnls(Bionext ~ trcFunc(SST,tlw,biomass,Catch_sqkm,a,g,b,d),
           params= list(a+g+d+b~ECO_REG),
           data = ttnew,
           start = list(a = c(-1,-1), b=c(.001,.001),g=c(.06,.06),d=c(-.6,-.6)))

p30 = nlme(Bionext ~ trcFunc(SST,tlw,biomass,Catch_sqkm,a,g,b,d),
           random = a+g+d+b~1|ECO_REG ,
           fixed = a+g+d+b ~ 1,
           data = ttnew,
           start = c(a = c(-1), b=c(.001),g=c(.06),d=c(-.6)))

trcFunc <- function(SST_within,biomass,Catch_sqkm,a,g){((a+g*SST_within)*biomass - Catch_sqkm)}
p30 = nlme(Bionext ~ trcFunc(SST_within,biomass,Catch_sqkm,a,g),
           random = a+g ~1|ECO_REG,
           fixed  = a+g ~ 1,
           data   = ttnew,
           start  = c(a = c(-1), g=c(0.1)))

p30 = nlme(Bionext ~ trcFunc(SST_within,biomass,Catch_sqkm,a,g),
           random = a+g~1| ECO_REG ,
           fixed  = a+g ~ 1,
           data   = ttnew,
           start  = c(a = c(-1), g=c(0.1)))
