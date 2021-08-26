
# get NEFSC group-specific gear values

library(ecodata)

nefsc <- read.csv("data/NEFSC_gear_q/NEFSC_Survey_Species_q.csv",header=T,sep=",")

# get latin names
lat <- ecodata::species_groupings
lat <- lat %>%
  distinct(SVSPP,SCINAME)%>%
  as.data.frame()

nefsc <- cbind(nefsc,lat[match(nefsc$SVSPP,lat$SVSPP),c(2)])
colnames(nefsc)[ncol(nefsc)] <- "Sci_name"
nam <- colnames(nefsc)
nefsc <- cbind(nefsc,str_split_fixed(nefsc$Sci_name, " ", 2))
colnames(nefsc) <-c(nam,"Genus","Species")

nefsc_spec <- nefsc %>%
  group_by(SVSPP,GRP) %>%
  summarize_at(.vars=c('q_edwards','q_assess','q_infer'), .funs=function(x) mean(x,na.rm=T)) %>% 
  ungroup() %>%
  as.data.frame()

nefsc_spec$effic <- rowMeans(nefsc_spec[,c('q_edwards','q_assess','q_infer')],na.rm=T)

# and the grouping
nefsc_GRP <- nefsc_spec %>%
  group_by(GRP) %>%
  summarize_at(.vars=c('effic'), .funs=function(x) mean(x,na.rm=T)) %>% 
  ungroup() %>%
  as.data.frame()

# get walker et al group-specific length-based values
geareff <- read.csv(file = "data/Walkeretal_2017_supp/EfficiencyTab.csv",sep=",",header = T)
geareff <- subset(geareff,!(geareff$Code %in% c("GRP1","GRP2","GRP3","GRP4","GRP5","GRP6","GRP7")))
geareff <- subset(geareff,!(geareff$Gear %in% c("GOV")))

walk_avg <- aggregate(geareff$Efficiency,by=list(geareff$Group),FUN = mean,na.rm=T)

effi <- cbind(walk_avg,nefsc_GRP[match(walk_avg$Group.1,nefsc_GRP$GRP),c(2)])
colnames(effi) <- c("group","Walker","NEFSC")

plot(effi$Walker,effi$NEFSC,xlim=c(0,0.5),ylim=c(0,0.5),col="white",xlab="Walker - lenght-based average q",ylab="NEFSC - q")
text(effi$Walker,effi$NEFSC,effi$group)




