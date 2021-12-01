
timeser <- tt 

colnames(timeser)[2] <- "bio"
colnames(timeser)[4] <- "catch"
colnames(timeser)[5] <- "catch_pel"

timeser$ER <- timeser$catch/timeser$bio
mod <- (lm(bio~SST + tl + log10(ER),data=timeser))

par(mfrow=c(1,3))
mod1 <- lm(bio~SST + tl ,data=timeser)
plot(residuals(mod1)~log10(timeser$ER),xlab="log10(ER)",ylab="Residuals",las=1,pch=16)

mod2 <- lm(bio~log10(ER) + tl ,data=timeser)
plot(residuals(mod2)~timeser$SST,xlab="SST",ylab="",las=1,pch=16)

mod3 <- lm(bio~log10(ER) + SST ,data=timeser)
plot(residuals(mod3)~timeser$tl,xlab="tl",ylab="",las=1,pch=16)

dat <- timeser
# conversions
dat$NPP <- dat$NPP * 9*365 # mg C / m**2 / day --> mg/m2/year == kg/km2/year
dat$ben_prod <- 10*dat$ben_prod # get detritus flux
dat$ben_prod <- dat$ben_prod *1000     # g / m**2 / year --> kg/km2/year
dat$lz_prod <-  dat$lz_prod*1000

# get fraction demersal
dat$frac <- dat$catch/ (dat$catch + dat$catch_pel)
dat$exploit <- dat$catch/(dat$ben_prod + dat$lz_prod*dat$frac)

# predict TE
TTE2 <- c()
for(j in 1:nrow(dat)){
  TE <- seq(0,0.2,length.out=100)
  xx <- dat$ben_prod[j]*TE^(dat$tl[j]-1) + dat$frac[j] * dat$lz_prod[j]*TE^(dat$tl[j]-2.1)
  TTE2 <- c(TTE2,TE[which(abs(xx-(dat$catch[j]))==min(abs(xx-(dat$catch[j]))))])
}
plot(TTE2~dat$SST,col="blue",pch=16)

TTEP <- 0.14
TTEB <- 0.4
ft   <- ifelse(dat$SST >20,.74,1)

TTEP <- 0.1 * 1.8^((10-dat$SST)/10)
TTEB <- 0.1 * 1.8^((10-dat$SST)/10)
ft   <- 1

Biomass = (dat$ben_prod*ft*TTEB^(dat$tl-1) + dat$frac * dat$lz_prod*ft*TTEP^(dat$tl-2.1))/dat$ER
plot(log10(Biomass)~log10(dat$bio),xlab="observed log10()",ylab="predicted log10()" , las =1 ,pch=16)
abline(0,1)

mod1 <- lm(log10(Biomass)~log10(dat$bio))
x <- c(3,5.5)
y <- coefficients(mod1)[1] + coefficients(mod1)[2]*x
lines(y~x, col="red",lty=3)

