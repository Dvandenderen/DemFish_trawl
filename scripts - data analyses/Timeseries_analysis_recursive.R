
load("cleaned data/timeseries_Jeremy.RData")
full <- tt

library(lattice)
library(dplyr)
library(nlme)

### plot timeseries per region
  lattice::xyplot(tt$Bio_kgsqkm ~ tt$Year | tt$ECO_REG, col = 1, type="o")

### simplify data and column names
  tt <- tt[,1:9]
  colnames(tt) <- c("Region","Year","Area_sqkm","Bio","tl","Catch","uni","SST","ER")
  
### get biomass in the next year
  tt <- tt[order(tt$Region, tt$Year),]
  tt <- tt %>% group_by(Region) %>% 
                mutate(Bio_1y = lead(Bio ,1))
  tt <- tt[complete.cases(tt$Bio_1y), ]
  
### get production, and standardize output to make it easier to fit
  tt$Prod <- tt$Bio_1y - tt$Bio + tt$Catch
  tt <-  tt %>% group_by(Region) %>% 
           mutate(Prod_sd = Prod/max(Bio),
           Bio_sd  = Bio/max(Bio),
           Bio_1y_sd  = Bio_1y/max(Bio),
           Catch_sd = Catch/max(Bio),
           SST_sd  = SST - mean(SST))
  tt <- as.data.frame(tt)
  
### not all regions have the same years, some regions have gaps - ignored
  # but note that regions with nice time series do give the same result
  
  
### fit model Jeremy
  trcFunc <- function(SST_sd,Bio_sd,Catch_sd,a,g,theta){
                     ((1+ a + SST_sd * theta + g* Bio_sd )* Bio_sd  - Catch_sd)}
  
  # fit easy model to obtain starting parameter for mixed model
  mfit <- nls(Bio_1y_sd~trcFunc(SST_sd,Bio_sd,Catch_sd,a,g,theta),
                                    start=list(a=1,g=1,theta=0.1),data = tt)
  
  p30 = nlme(Bio_1y_sd ~ trcFunc(SST_sd,Bio_sd,Catch_sd,a,g,theta),
             random =theta ~ 1|Region,
             fixed = a+theta+g ~ 1,
             data = tt,
             start = coef(mfit),
             control=(msMaxIter=10^10))
  
  summary(p30) # no indication that theta is important 
  ranef(p30)   # random effect very small
  
### fit model with carrying capacity term
  # based on Free et al. 2019 - Impacts of historical warming on marine fisheries production
  # note without the exp() I get issues with initial parameter estimates
  trcFunc <- function(Bio_sd,SST_sd,r,K,theta){
    (r*Bio_sd*(1-Bio_sd/K) * exp(SST_sd * theta))}
  
  mfit <- nls(Prod_sd~trcFunc(Bio_sd,SST_sd,r,K,theta),
              start=list(r=1,K=.1,theta=0),data = tt) 
  
  p30 = nlme(Prod_sd ~ trcFunc(Bio_sd,SST_sd,r,K,theta),
             random = r + K  ~ 1|Region , # r and K as random effect gives lowest AIC
             fixed = r+K+theta ~ 1,
             data = tt,
             start = coef(mfit),
             control=(msMaxIter=10^12))
  summary(p30) # K is less than 1, suggests weak changes over time
  coef(p30)
  
  # could consider to add temporal auto on top (lower AIC, but no differences)
  #p31 = nlme(Prod_sd ~ trcFunc(Bio_sd,SST_sd,r,K,theta),
  #           random = r + K  ~ 1|Region , # r and K as random effect gives lowest AIC
  #           fixed = r+K+theta ~ 1,
  #           data = tt,
  #           start = coef(mfit),
  #           control=(msMaxIter=10^12),correlation = corARMA(c(0.1),p=1,q=0,form=~1 | Region))
  
## fit model with semi-chemostat type of setup (almost the same as previous)
  trcFunc <- function(Bio_sd,SST_sd,r,K,theta){
    (r*(K-Bio_sd) * exp(SST_sd * theta))}
  
  mfit <- nls(Prod_sd~trcFunc(Bio_sd,SST_sd,r,K,theta),
              start=list(r=1,K=.1,theta=0),data = tt)
  
  p30 = nlme(Prod_sd ~ trcFunc(Bio_sd,SST_sd,r,K,theta),
             random = r + K ~ 1|Region , # r and K random gives lowest AIC
             fixed = r+K+theta ~ 1,
             data = tt,
             start = coef(mfit),
             control=(msMaxIter=10^12))
  summary(p30)  
  coef(p30)
  
### so why do we get temperature effect in statistical model? 
  # fit the full model from previous analysis
  # fishing effect within/across regions is the same so we can use the actual observation
  # trophic level did not have any importance
  
  statdat <- full[,c(1,2,4,8:16)]
  colnames(statdat) <- c("Region","Year","Bio","SST","ER","LER","tl_across",
                         "SST_across", "LER_across", "tl_within", "SST_within", "LER_within")
  
  cs1 <- corARMA(c(0.1,0.1), p = 2, q = 0,form =~ 1|Region) # this "form" ignores years without data

  fit_cor <- lme(Bio ~ SST_within + SST_across +  LER , 
                  random= ~ 1 + SST_within|Region, correlation=cs1, data=statdat)
  
  # ACF takes into account groupings (Region)
  plot(ACF(fit_cor), alpha = 0.05) #  not pretty 
  coef(fit_cor)
  
  # compare with model without correlation
  fit_nocor <- update(fit_cor,correlation=NULL)
  plot(ACF(fit_nocor), alpha = 0.05)
  
  anova(fit_cor,fit_nocor) # fit_cor is here the "better" model
  
  # what happens if we use the standardized biomass and temp estimates
  # temperature effect seems to disappear -- no indication of random SST_sd slope 
  # note: random SST_sd slope model with LER gives convergence errors (but works with ER) 
  cs1 <- corARMA(c(0.7,0.1), p = 2, q = 0,form =~ 1|Region) 
  
  tt$LER <- log10(tt$Catch_sd/tt$Bio_sd)
  fit_cor_st <- lme(Bio_sd  ~ SST_sd +  LER , 
                 random= ~ 1|Region, correlation=cs1, data=tt)
  
  plot(ACF(fit_cor_st), alpha = 0.05)
  
  #### let's look at a region with a strong temp effect -- Eastern Bering Sea
  EBS <- subset(tt,tt$Region =="Eastern Bering Sea")
  par(mfrow=c(1,2))
  plot(EBS$Bio_sd~EBS$SST,xlab="SST(i)",ylab="Bio(i)",las=1,main="Eastern Ber. Sea")
  plot(EBS$Bio_1y_sd~EBS$Bio_sd,ylab="Bio(i+1)",xlab="Bio(i)",las=1,main="Eastern Ber. Sea") 
  
  # what happens if we fit this data - both type of models
  trcFunc <- function(SST_sd,Bio_sd,Catch_sd,a,g,theta){
    ((1+ a + SST_sd * theta + g* Bio_sd )* Bio_sd  - Catch_sd)}
  
  mfit <- nls(Bio_1y_sd~trcFunc(SST_sd,Bio_sd,Catch_sd,a,g,theta),
              start=list(a=1,g=1,theta=0.1),data = EBS)
  acf(residuals(mfit))
  
  EBS$LER <- log10(EBS$ER)
  fit_EBS <- gls(Bio_sd  ~ SST_sd +  LER , 
                    correlation= corAR1(), data=EBS)
  acf(residuals(fit_EBS))
  