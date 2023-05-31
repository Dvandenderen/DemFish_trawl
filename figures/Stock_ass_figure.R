################
### create figures for stock assessment comparison with trawl survey data
###
### 

library(Metrics)

stock <- read.csv("processed data/stock assessment comparison.csv",header=T)
colnames(stock)[23] <- "wt"
colnames(stock)[24] <- "wt_q"

#### all data
pdf("figures/Stock_ass_comparison.pdf",width=6.9,height=3.56)  
par(mfrow=c(1,2), mar=c(4, 4, 2, 1))

plot(log10(stock$biomass),log10(stock$wt_q),xlim=c(-1,8),ylim=c(-1,8),xlab="Stock assess. biomass - log10(MT)",
     ylab="Survey biomass - log10(MT)",main="",las=1,pch=16)
abline(0,1,lty=5)
text(-0.5,7.5,"a)")
mod1 <- lm(log10(stock$wt_q)~log10(stock$biomass))
x <- c(-1,9)
y <- coefficients(mod1)[1] + coefficients(mod1)[2]*x 
lines(y~x,lty=1)

text(6,0,"RMSE = 0.88 \n R2 = 0.61")


#points(log10(stock$biomass[stock$toanalyze <30]),log10(stock$wt_q[stock$toanalyze <30]),col="blue",pch=16)

plot(log10(stock$biomass),log10(stock$wt),xlim=c(-1,8),ylim=c(-1,8),xlab="Stock assess. biomass - log10(MT)",
     ylab="",main="",las=1,pch=16)
abline(0,1,lty=5)
text(-0.5,7.5,"b)")
mod1 <- lm(log10(stock$wt)~log10(stock$biomass))
x <- c(-1,9)
y <- coefficients(mod1)[1] + coefficients(mod1)[2]*x 
lines(y~x,lty=1)

text(6,0,"RMSE = 1.25 \n R2 = 0.54")

dev.off()

## per region
pdf("figures/Stock_ass_comparison_appendix.pdf",width=6.9,height=5)  
par(mfrow=c(2,3), mar=c(4, 4, 2, 1))
pac <- subset(stock,stock$region %in% c("US West Coast","Canada West Coast","US Alaska"))

plot(log10(pac$biomass),log10(pac$wt_q),xlim=c(-1,8),ylim=c(-1,8),xlab="",
     ylab="Survey biomass - log10(MT)",main="Northeast Pacific",las=1,pch=16)
abline(0,1)
text(-0.5,7.5,"a)")

EastC <- subset(stock,stock$region %in% c("US East Coast","Canada East Coast","US Southeast and Gulf"))
plot(log10(EastC$biomass),log10(EastC$wt_q),xlim=c(-1,8),ylim=c(-1,8),xlab="",
     ylab="",main="Northwest Atlantic",las=1,pch=16)
abline(0,1)
text(-0.5,7.5,"b)")

EU <- subset(stock,stock$region %in% c("European Union","Europe non EU"))
plot(log10(EU$biomass),log10(EU$wt_q),xlim=c(-1,8),ylim=c(-1,8),xlab="",
     ylab="",main="Northeast Atlantic",las=1,pch=16)
abline(0,1)
text(-0.5,7.5,"c)")

pel <- c("Bluefish Atlantic Coast",
         "Capelin Barents Sea",
         "Herring ICES 22-24-IIIa",
         "Herring Scotian Shelf and Bay of Fundy", 
         "Pacific herring Central Coast",
         "Herring Northern Irish Sea",
         "Herring IIIa, VIId and North Sea",
         "Atlantic herring Northwestern Atlantic Coast",
         "Pacific herring Prince Rupert District",
         "Pacific herring Haida Gwaii",
         "Herring ICES 28",
         "Herring ICES VIa",
         "Herring ICES VIa-VIIb-VIIc",
         "Pacific herring West Coast of Vancouver Island",
         "Spanish mackerel Southern Atlantic Coast",
         "Sprat ICES Baltic Areas 22-32",
         "Sprat North Sea",
         "Herring NAFO 4T fall and spring spawners")

pelstoc <- subset(stock,stock$stocklong %in% pel)
her <- pelstoc[c(3:14,18),]
pelstoc <- pelstoc[-c(3:14,18),]
plot(log10(pelstoc$biomass),log10(pelstoc$wt_q),xlim=c(-1,8),ylim=c(-1,8),xlab="",
     ylab="Survey biomass - log10(MT)",main="Pelagic stocks",las=1,pch=16)
points(log10(her$biomass),log10(her$wt_q),pch="+")
abline(0,1)
text(-0.5,7.5,"d)")

demstoc <- subset(stock,!(stock$stocklong %in% pel))
plot(log10(demstoc$biomass),log10(demstoc$wt_q),xlim=c(-1,8),ylim=c(-1,8),xlab="Stock assess. biomass - log10(MT)",
     ylab="",main="Demersal stocks",las=1,pch=16)
abline(0,1)
text(-0.5,7.5,"e)")

highoverlap <- subset(stock,stock$area_overlap_in_ocean >0.8)
plot(log10(highoverlap$biomass),log10(highoverlap$wt_q),xlim=c(-1,8),ylim=c(-1,8),xlab="",
     ylab="",main="High spatial overlap",las=1,pch=16)
abline(0,1)
text(-0.5,7.5,"f)")

dev.off()

