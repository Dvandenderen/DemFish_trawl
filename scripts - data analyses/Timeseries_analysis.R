
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
tt$LLER  <- log(tt$ER)

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
xyplot(tt$biomass ~ tt$year | tt$ECO_REG, col = 1)

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
fit3_contrast <- lme(biomass ~ SST + SST_across +  tlw_within + tlw_across +  LLER + LLER_across, 
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
fit4_a <- lme(biomass ~ SST_within + SST_across +  LLER , 
            random= ~ 1 |ECO_REG, correlation=cs1, data=tt)

fit4_b <- lme(biomass ~ SST_within + SST_across +  LLER , 
                     random= ~ 1 + SST_within|ECO_REG, correlation=cs1, data=tt)
rand <- ranef(fit4_b); rand$ECO_REG <- rownames(rand)

temp <- cbind(temp,rand[match(temp$ECO_REG,rand$ECO_REG),c("SST_within")])
colnames(temp)[5] <- "slope"

temp$fact <- ifelse(temp$SST_across < 9, "C",NA )
temp$fact <- ifelse(temp$SST_across > 8 & temp$SST_across < 12, "C2",temp$fact)
temp$fact <- ifelse(temp$SST_across > 14, "W",temp$fact )
temp$fact <- ifelse(is.na(temp$fact),"T",temp$fact)

boxplot(temp$slope ~ as.factor(temp$fact))

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
AIC(fit1,fit2,fit3)


# now can we fit a recurring model?
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
