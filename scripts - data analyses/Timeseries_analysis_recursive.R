
rm(list=ls())

library(lattice)
library(dplyr)
library(nlme)

# -------------------------------------------------------------------------------
# load grid cell time series and estimate the average per ecoregion and year
# -------------------------------------------------------------------------------

 load("cleaned data/Biomass_timeseries_grid.RData")
 cpue_good <- subset(cpue_good,cpue_good$year %in% c(1980:2015))

 # get weighted mean based on size of ocean area
 cpue_good[,c(3,4,8,10,11)]  <- cpue_good[,c(3,4,8,10,11)] * cpue_good$ocean_sqkm # weighted with the size of the ocean area
 tt           <-  aggregate(list(cpue_good[,c(7,3,4,8,10,11)]),
                        by=list(cpue_good$ECO_REG,cpue_good$year),FUN=sum,na.rm=T)
 cnam         <- colnames(cpue_good[,c(7,3,4,8,10,11)])
 colnames(tt) <- c("ECO_REG","year",cnam)
 tt[,4:8]     <- tt[,4:8]/tt$ocean_sqkm
 

# -------------------------------------------------------------------------------
# add biomass smoother
# -------------------------------------------------------------------------------
 ecreg <- unique(tt$ECO_REG)
 full  <- c()
 for (j in 1:21){
   tnew       <- subset(tt,tt$ECO_REG == ecreg[j])
   if(j %in% c(19,21)){TBsmooth  <- smooth.spline(tnew$year, tnew$biomass, cv = TRUE)
   } else {TBsmooth  <- smooth.spline(tnew$year, tnew$biomass, cv = TRUE, nknots=round(nrow(tnew)/3))}
   Yfine      <- unique(tnew$year)
   tnew$Bio_s <- predict(TBsmooth, Yfine)$y
   full       <- rbind(full,tnew)
 }

 tt      <- full
 tt$ER   <- tt$Catch/tt$biomass 
 tt$ER_s <- tt$Catch/tt$Bio_s 
 
# -------------------------------------------------------------------------------
# plot timeseries per region
# -------------------------------------------------------------------------------
 tt$ECO_short <- tt$ECO_REG
 tt$ECO_short <- ifelse(tt$ECO_short == "Gulf of Maine/Bay of Fundy","G. Maine/ Bay Fundy",tt$ECO_short) 
 tt$ECO_short <- ifelse(tt$ECO_short == "Oregon, Washington, Vancouver Coast and Shelf","Ore. Wash. Vanc.",tt$ECO_short) 
 tt$ECO_short <- ifelse(tt$ECO_short == "Northern Gulf of Mexico","N. Gulf of Mexico",tt$ECO_short) 
 tt$ECO_short <- ifelse(tt$ECO_short == "Southern California Bight","S. California Bight",tt$ECO_short) 
 tt$ECO_short <- ifelse(tt$ECO_short == "Northern Norway and Finnmark","N. Norway & Finnm.",tt$ECO_short) 
 tt$ECO_short <- ifelse(tt$ECO_short == "South European Atlantic Shelf","S. Eur. Atl. Shelf",tt$ECO_short) 
 tt$ECO_short <- ifelse(tt$ECO_short == "Gulf of St. Lawrence - Eastern Scotian Shelf" ,"St. Lawr. & E. Scot." ,tt$ECO_short) 
 tt$ECO_short <- ifelse(tt$ECO_short == "North American Pacific Fijordland" ,"N.A. Pac. Fijordland"  ,tt$ECO_short) 
 tt$ECO_short <- ifelse(tt$ECO_short == "North and East Barents Sea" ,"N & E Barents Sea"  ,tt$ECO_short) 
 
  lattice::xyplot(tt$biomass/1000 ~ tt$year | tt$ECO_short, col = 1, 
                  type="o",cex.lab=0.5,xlab="Year",ylab="Tonnes per km2")

# -------------------------------------------------------------------------------
# prepare data for model fitting
# -------------------------------------------------------------------------------

  # simplify data and column names
  colnames(tt)[c(1,2,4)] <- c("Region","Year","Bio")
  
  # get biomass in the next year
  tt <- tt[order(tt$Region, tt$Year),]
  tt <- tt %>% group_by(Region) %>% 
                mutate(Bio_1y   = lead(Bio ,1),
                       Bio_1y_s = lead(Bio_s ,1))
  tt <- tt[complete.cases(tt$Bio_1y), ]
  
### get production, and standardize output to make it easier to fit
  tt$Prod   <- tt$Bio_1y   - tt$Bio   + tt$Bio*tt$ER
  tt$Prod_s <- tt$Bio_1y_s - tt$Bio_s + tt$Bio*tt$ER
  tt        <-  tt %>% group_by(Region) %>% 
                   mutate(Prod     = Prod     / max(Bio),
                          Prod_s   = Prod_s   / max(Bio_s),
                          Bio_1y   = Bio_1y   / max(Bio),
                          Bio_1y_s = Bio_1y_s / max(Bio_s),
                          Bio      = Bio      / max(Bio),
                          Bio_s    = Bio_s    / max(Bio_s),
                          SST      = SST - mean(SST))
  tt <- as.data.frame(tt)
  tt$Catch   <- tt$ER *tt$Bio
  tt$Catch_s <- tt$ER_s * tt$Bio_s
# -------------------------------------------------------------------------------
# fit recursive biomass model + additive error term
# -------------------------------------------------------------------------------
  
  # not all regions have the same years, some regions have gaps - ignored
  # but note that regions with nice time series do give the same result
  
  # model specification
  trcFunc <- function(SST,Bio,Catch,a,g,theta){
                     (a + theta*SST + g* Bio )* Bio  - Catch}
  
  # fit to obtain starting parameter for mixed model
  mfit   <- nls(Bio_1y ~ trcFunc(SST,Bio,Catch,a,g,theta),
                         start=list(a=1,g=1,theta=0.1),data = tt)
  
  p30 = nlme(Bio_1y ~ trcFunc(SST,Bio,Catch,a,g,theta),
             random = a ~ 1|Region,
             fixed  = a+theta+g ~ 1,
             data   = tt,
             start  = coef(mfit),
             control=(msMaxIter=10^10))
  
  summary(p30) # no indication that theta is important 
  
  # model specification
  trcFunc <- function(Bio,Catch,a,g){
    (a + g* Bio )* Bio  - Catch}
  
  # fit to obtain starting parameter for mixed model
  mfit   <- nls(Bio_1y ~ trcFunc(Bio,Catch,a,g),
                start=list(a=1,g=1),data = tt)
  
  p31 = nlme(Bio_1y ~ trcFunc(Bio,Catch,a,g),
             random = a ~ 1|Region,
             fixed  = a+g ~ 1,
             data   = tt,
             start  = coef(mfit),
             control=(msMaxIter=10^10))
  
  AIC(p30)-AIC(p31)

# -------------------------------------------------------------------------------
# fit recursive biomass model + additive error term - smoothed biomass
# -------------------------------------------------------------------------------

  # model specification
  trcFunc <- function(SST,Bio_s,Catch_s,a,g,theta){
    (a + theta*SST + g* Bio_s )* Bio_s  - Catch_s}
  
  # fit to obtain starting parameter for mixed model
  mfit   <- nls(Bio_1y_s ~ trcFunc(SST ,Bio_s,Catch_s,a,g,theta),
                start=list(a=1,g=1,theta=0.1),data = tt)
  
  p30 = nlme(Bio_1y_s ~ trcFunc(SST,Bio_s,Catch_s,a,g,theta),
             random = a ~ 1|Region,
             fixed  = a+theta+g ~ 1,
             data   = tt,
             start  = coef(mfit),
             control=(msMaxIter=10^10))
  
  summary(p30) # no indication that theta is important  

# -------------------------------------------------------------------------------
# fit recursive biomass model + multiplicative error term
# -------------------------------------------------------------------------------
  trcFunc <- function(SST, Bio, ER, a, g, theta){
    log(a - g * Bio - ER) + log(Bio) + theta*SST  }
  
  # fit to obtain starting parameter for mixed model
  mfit <- nls(log(Bio_1y) ~ trcFunc(SST,Bio,ER,a,g,theta),
              start=list(a=1,g=.1,theta=0.1),data = tt)
  
  p30 = nlme(log(Bio_1y) ~ trcFunc(SST,Bio,ER,a,g,theta),
             random = a ~ 1|Region,
             fixed = a+theta+g ~ 1,
             data = tt,
             start = c(0.8,coef(mfit)[2:3]),
             control=(msMaxIter=10^10))  
  
  summary(p30) # no indication that theta is important 
  
  trcFunc <- function(Bio, ER, a, g){
    log(a - g * Bio - ER) + log(Bio) }
  
  # fit to obtain starting parameter for mixed model
  mfit <- nls(log(Bio_1y) ~ trcFunc(Bio,ER,a,g),
              start=list(a=1,g=.1),data = tt)
  
  p31 = nlme(log(Bio_1y) ~ trcFunc(Bio,ER,a,g),
             random = a ~ 1|Region,
             fixed = a+g ~ 1,
             data = tt,
             start = c(0.8,coef(mfit)[2]),
             control=(msMaxIter=10^10))  
  
  AIC(p30)-AIC(p31)
  
  
# -------------------------------------------------------------------------------
# fit recursive biomass model + multiplicative error term - smoothed biomass
# -------------------------------------------------------------------------------

  trcFunc <- function(SST, Bio_s, ER_s, a, g, theta){
    log(a - g * Bio_s - ER_s) + log(Bio_s) + theta*SST  }
  
  # fit to obtain starting parameter for mixed model
  mfit <- nls(log(Bio_1y_s) ~ trcFunc(SST,Bio_s,ER_s,a,g,theta),
              start=list(a=1,g=.1,theta=0.1),data = tt)
  
  p30 = nlme(log(Bio_1y) ~ trcFunc(SST,Bio_s,ER_s,a,g,theta),
             random = a ~ 1|Region,
             fixed = a+theta+g ~ 1,
             data = tt,
             start = c(0.8,coef(mfit)[2:3]),
             control=(msMaxIter=10^10))  
  
  summary(p30) # no indication that theta is important 
  
# -------------------------------------------------------------------------------
# fit "ricker" model + multiplicative error term
# -------------------------------------------------------------------------------
  
  trcFunc <- function(SST,Bio,ER,a,g,theta){
    log(Bio) + a +  theta*SST - g*Bio - log(1-ER) }
  
  # fit to obtain starting parameter for mixed model
  mfit <- nls(log(Bio_1y) ~ trcFunc(SST,Bio,ER,a,g,theta),
              start=list(a=0.1,g=.1,theta=0.1),data = tt)
  
  p30 = nlme(log(Bio_1y) ~ trcFunc(SST,Bio,ER,a,g,theta),
             random = a+g ~ 1|Region,
             fixed = a+theta+g ~ 1,
             data = tt,
             start = coef(mfit),
             control=(msMaxIter=10^10))
  
  summary(p30) # theta is not important
  
  trcFunc <- function(Bio,ER,a,g){
    log(Bio) + a - g*Bio - log(1-ER) }
  
  # fit to obtain starting parameter for mixed model
  mfit <- nls(log(Bio_1y) ~ trcFunc(Bio,ER,a,g),
              start=list(a=0.1,g=.1),data = tt)
  
  p31 = nlme(log(Bio_1y) ~ trcFunc(Bio,ER,a,g),
             random = a+g ~ 1|Region,
             fixed = a+g ~ 1,
             data = tt,
             start = coef(mfit),
             control=(msMaxIter=10^10))
  
  AIC(p30)-AIC(p31)
# -------------------------------------------------------------------------------
# fit "ricker" model + multiplicative error term - smoothed biomass
# -------------------------------------------------------------------------------
  
  trcFunc <- function(SST,Bio_s,ER_s,a,g,theta){
    log(Bio_s) + a +  theta*SST - g*Bio_s - log(1-ER_s) }
  
  # fit to obtain starting parameter for mixed model
  mfit <- nls(log(Bio_1y_s) ~ trcFunc(SST,Bio_s,ER_s,a,g,theta),
              start=list(a=0.1,g=.1,theta=0.1),data = tt)
  
  p30 = nlme(log(Bio_1y_s) ~ trcFunc(SST,Bio_s,ER_s,a,g,theta),
             random = a ~ 1|Region,
             fixed = a+theta+g ~ 1,
             data = tt,
             start = coef(mfit),
             control=(msMaxIter=10^10))
  
  summary(p30) # theta is important
  
# -------------------------------------------------------------------------------
# fit logistic growth model + additive error term 
# based on Free et al. 2019 - Impacts of historical warming on marine fisheries production
# ------------------------------------------------------------------------------- 
  
  trcFunc <- function(Bio,SST,r,K,theta){
    (r*Bio*(1-Bio/K) * exp(SST * theta))}
  
  mfitT <- nls(Prod ~ trcFunc(Bio,SST,r,K,theta),
               start=list(r=1,K=.1,theta=0),data = tt) 
  
  p30 <- nlme(Prod ~ trcFunc(Bio,SST,r,K,theta),
               random = r ~ 1|Region , # r and K as random effect gives lowest AIC
               fixed = r+K+theta ~ 1,
               data = tt,
               start = coef(mfitT),
               control=(msMaxIter=10^12))
  
  summary(p30) # no indication that theta is important 
  
  trcFunc <- function(Bio,r,K){
    (r*Bio*(1-Bio/K))}
  
  mfitT <- nls(Prod ~ trcFunc(Bio,r,K),
               start=list(r=1,K=.1),data = tt) 
  
  p31 <- nlme(Prod ~ trcFunc(Bio,r,K),
               random = r ~ 1|Region , # r and K as random effect gives lowest AIC
               fixed = r+K ~ 1,
               data = tt,
               start = coef(mfitT),
               control=(msMaxIter=10^12))
  
  AIC(p30)-AIC(p31)
  
# -------------------------------------------------------------------------------
# fit logistic growth model + additive error term - smoothed biomass
# based on Free et al. 2019 - Impacts of historical warming on marine fisheries production
# ------------------------------------------------------------------------------- 
  
  trcFunc <- function(Bio_s,SST,r,K,theta){
    (r*Bio_s*(1-Bio_s/K) * exp(SST * theta))}
  
  mfitT <- nls(Prod_s ~ trcFunc(Bio_s,SST,r,K,theta),
               start=list(r=1,K=.1,theta=0),data = tt) 
  
  fixT <- nlme(Prod_s ~ trcFunc(Bio_s,SST,r,K,theta),
               random = K ~ 1|Region , # r and K as random effect gives lowest AIC
               fixed = r+K+theta ~ 1,
               data = tt,
               start = coef(mfitT),
               control=(msMaxIter=10^12))
  
  summary(fixT) # theta is important 
  
# -------------------------------------------------------------------------------
# fit logistic growth model + additive error term
# temperature effect only affects carrying capacity
# ------------------------------------------------------------------------------- 
  
  trcFunc <- function(Bio,SST,r,K,theta){
    (r*Bio*(1-Bio/(K * exp(SST * theta))))}
  
  mfitT <- nls(Prod ~ trcFunc(Bio,SST,r,K,theta),
               start=list(r=1,K=.1,theta=0),data = tt) 
  
  p30 <- nlme(Prod ~ trcFunc(Bio,SST,r,K,theta),
               random = r ~ 1|Region , # r and K as random effect gives lowest AIC
               fixed = r+K+theta ~ 1,
               data = tt,
               start = coef(mfitT),
               control=(msMaxIter=10^12))
  
  summary(p30) # no indication that theta is important  
  
  trcFunc <- function(Bio,r,K){
    (r*Bio*(1-Bio/K))}
  
  mfitT <- nls(Prod ~ trcFunc(Bio,r,K),
               start=list(r=1,K=.1),data = tt) 
  
  p31 <- nlme(Prod ~ trcFunc(Bio,r,K),
               random = r ~ 1|Region , # r and K as random effect gives lowest AIC
               fixed = r+K ~ 1,
               data = tt,
               start = coef(mfitT),
               control=(msMaxIter=10^12))
  
  AIC(p30)-AIC(p31)
  
# -------------------------------------------------------------------------------
# fit logistic growth model + additive error term - smoothed biomass
# temperature effect only affects carrying capacity
# ------------------------------------------------------------------------------- 
  
  trcFunc <- function(Bio_s,SST,r,K,theta){
    (r*Bio_s*(1-Bio_s/(K * exp(SST * theta))))}
  
  mfitT <- nls(Prod_s ~ trcFunc(Bio_s,SST,r,K,theta),
               start=list(r=1,K=.1,theta=0),data = tt) 
  
  fixT <- nlme(Prod_s ~ trcFunc(Bio_s,SST,r,K,theta),
               random = r ~ 1|Region , # r and K as random effect gives lowest AIC
               fixed = r+K+theta ~ 1,
               data = tt,
               start = coef(mfitT),
               control=(msMaxIter=10^12))
  
  summary(fixT) # no indication that theta is important 
  
  
# -------------------------------------------------------------------------------
# verify some of the results
# -------------------------------------------------------------------------------   
  
  tt <- subset(tt,!(tt$ECO_REG %in% c("Aleutian Island","Northern California",
                                      "Oregon, Washington, Vancouver Coast and Shelf",
                                      "Southern California Bight",
                                      "North American Pacific Fijordland")))
  
  