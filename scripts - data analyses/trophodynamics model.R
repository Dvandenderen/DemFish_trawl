
dat <- ttfin
dat$frac <- dat$Dem/(dat$Dem + dat$Pel)

dat$totcatch <- dat$catch/dat$frac
catch <- dat$ER * dat$NPP*0.1^(dat$tl-1)

TE <- ifelse(dat$SST >20,0.1*0.74,0.1)
TE <- ifelse(dat$SST <10,0.1*1.26,TE)
TE <- ifelse(dat$SST <10,0.1*2,TE)

TE <- 0.1
TE <- 0.1 * 2.5^((10-dat$SST)/10)

dat$NPP <- dat$NPP * 9*365 # mg C / m**2 / day 

dat$ER <- dat$catch/dat$NPP

catch <- dat$ER*(10*dat$ben_prod*TE^(dat$tl-1) + dat$lz_prod*TE^(dat$tl-2.1))
catch <- catch*1000

plot(log10(catch),log10(dat$catch))
abline(0,1)
cor(log10(catch),log10(dat$catch))
