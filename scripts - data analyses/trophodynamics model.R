
dat <- ttfin
dat$frac <- dat$Dem/(dat$Dem + dat$Pel)

dat$totcatch <- dat$catch/dat$frac
catch <- dat$ER * dat$NPP*0.1^(dat$tl-1)

TE <- ifelse(dat$SST >20,0.1*0.74,0.1)
TE <- ifelse(dat$SST <10,0.1*1.26,TE)
TE <- ifelse(dat$SST <10,0.1*2,TE)
TE <- 0.1
TE <- 0.1 * 2.5^((10-dat$SST)/10)

# conversions
dat$NPP <- dat$NPP * 9*365 # mg C / m**2 / day --> mg/m2/year == kg/km2/year
dat$ben_prod <- 10*dat$ben_prod
dat$ben_prod <- dat$ben_prod *1000     # g / m**2 / year --> kg/km2/year
dat$lz_prod <-  dat$lz_prod*1000

# predict catch
TE <- 0.2
TE <- 0.2 * 2.5^((10-dat$SST)/10)
catch <- (10*dat$ER)*(dat$ben_prod*TE^(dat$tl-1) + dat$frac*dat$lz_prod*TE^(dat$tl-2.1))
plot(log10(catch),log10(dat$catch))
abline(0,1)

frac <- dat$Dem/(dat$Dem+dat$Pel)

# since biomass is reasonably constant over time
dat$NPP*x^(dat$tl-1) = C/B*B
TTE <- (dat$catch/(dat$NPP)) ^ (1 / (dat$tl-1))
plot(TTE~dat$SST)

# now do the same for 
dat$ben_prod[1]*TE^(dat$tl[1]-1) + dat$lz_prod[1]*TE^(dat$tl[1]-2.1) = C/B*B

TTE2 <- c()
for(j in 1:nrow(dat)){
  TE <- seq(0,0.2,length.out=100)
  xx <- dat$ben_prod[j]*TE^(dat$tl[j]-1) + dat$frac[j]*dat$lz_prod[j]*TE^(dat$tl[j]-2.1)
  TTE2 <- c(TTE2,TE[which(abs(xx-(dat$catch[j]))==min(abs(xx-(dat$catch[j]))))])
}
plot(TTE2~dat$SST,col="blue",pch=16)
points(TTE~dat$SST,col="red",pch=16)

Biomass = 220000*TTE2^(dat$tl-1) + 47000*TTE2^(dat$tl-2.1)/dat$ER
Biomass = dat$ben_prod*TTE2^(dat$tl-1) + dat$frac*dat$lz_prod*TTE2^(dat$tl-2.1)/dat$ER
plot(log10(xx),log10(dat$bio))
abline(0,1)

xx <-  1690649*TTE^(dat$tl-1)/dat$ER

