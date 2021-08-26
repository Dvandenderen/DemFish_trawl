
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

# get all species oceanadapt
load("traits and species/oceanadapt_species_list.RData")
goodspec <- subset(goodspec, goodspec$class %in% c("Elasmobranchii","Actinopteri","Holocephali","Myxini","Petromyzonti")) 
goodspec$capitals <- toupper(goodspec$scientificname)

# based on species
gd_new <- cbind(goodspec,nefsc[match(goodspec$capitals,nefsc$Sci_name),c("SVSPP")])
colnames(gd_new)[ncol(gd_new)] <- "q_group"

# based on genus
gd_new$capitals <- toupper(gd_new$genus)
gd_new <- cbind(gd_new,nefsc[match(gd_new$capitals,nefsc$Genus),c("SVSPP")])
colnames(gd_new)[ncol(gd_new)] <- "q_group2"

gd_new$q_group <- ifelse(is.na(gd_new$q_group), gd_new$q_group2,gd_new$q_group) 

# now get groupings based on Walker id
gd_new <- cbind(gd_new,q_names[match(gd_new$AphiaID,q_names$AphiaID),c("q_group")])
colnames(gd_new)[ncol(gd_new)] <- "q_group3"

gd_new$q_group <- ifelse(is.na(gd_new$q_group), gd_new$q_group3,gd_new$q_group) 

# now do the same based on trait data
traits <- read.csv("traits and species/Beukhofetal_2019/Traits_fish.csv",header=T,sep=";",row.names=NULL)
gd_new <- cbind(gd_new,traits[match(gd_new$scientificname,traits$taxon),c("habitat","body.shape")])

gd_new$q_group4 <- NA
gd_new$q_group4 <- ifelse(gd_new$habitat %in% c("benthopelagic","reef-associated") ,"GRP4",gd_new$q_group4)
gd_new$q_group4 <- ifelse(gd_new$body.shape == "flat","GRP3",gd_new$q_group4)
gd_new$q_group4 <- ifelse(gd_new$body.shape %in% c("eel-like" , "fusiform","compressiform","elongated") & gd_new$habitat %in% c("demersal","bathydemersal"),"GRP2",gd_new$q_group4)
gd_new$q_group4 <- ifelse(gd_new$habitat %in% c("pelagic","bathypelagic"),"GRP6",gd_new$q_group4)
gd_new$q_group4<- ifelse(gd_new$body.shape %in% c("short and/or deep") & gd_new$habitat  %in% c("demersal","bathydemersal") ,"GRP7",gd_new$q_group4)

gd_new$q_group <- ifelse(is.na(gd_new$q_group), gd_new$q_group4,gd_new$q_group) 

# now get groupings based on Walker genus
gd_new <- cbind(gd_new,q_names[match(gd_new$genus,q_names$Genus),c("q_group")])
colnames(gd_new)[ncol(gd_new)] <- "q_group5"

gd_new$q_group <- ifelse(is.na(gd_new$q_group), gd_new$q_group5,gd_new$q_group) 

# now do the same based on trait data at genus
gd_new <- gd_new %>%
  select(-habitat,-body.shape)
gd_new <- cbind(gd_new,traits[match(gd_new$genus,traits$genus),c("habitat","body.shape")])

gd_new$q_group6 <- NA
gd_new$q_group6 <- ifelse(gd_new$habitat %in% c("benthopelagic","reef-associated") ,"GRP4",gd_new$q_group6)
gd_new$q_group6 <- ifelse(gd_new$body.shape == "flat","GRP3",gd_new$q_group6)
gd_new$q_group6 <- ifelse(gd_new$body.shape %in% c("eel-like" , "fusiform","compressiform","elongated") & gd_new$habitat %in% c("demersal","bathydemersal"),"GRP2",gd_new$q_group6)
gd_new$q_group6 <- ifelse(gd_new$habitat %in% c("pelagic","bathypelagic"),"GRP6",gd_new$q_group6)
gd_new$q_group6<- ifelse(gd_new$body.shape %in% c("short and/or deep") & gd_new$habitat  %in% c("demersal","bathydemersal") ,"GRP7",gd_new$q_group6)

gd_new$q_group <- ifelse(is.na(gd_new$q_group), gd_new$q_group6,gd_new$q_group) 

## final edits
gd_new$q_group <- ifelse(gd_new$family %in% c("Achiridae","Ammodytidae"),"GRP1",gd_new$q_group)
gd_new$q_group <- ifelse(gd_new$q_group == "WHB" ,"GRP5",gd_new$q_group)
gd_new$q_group <- ifelse(gd_new$q_group == "BTF" ,"GRP2",gd_new$q_group)
gd_new$q_group <- ifelse(gd_new$q_group == 1 ,"GRP2",gd_new$q_group)

# remove "Chaetopterus" ,"Hepatus princeps"  "Cantharus" 
gd_new <- subset(gd_new,!(gd_new$spec %in% c( "Chaetopterus" ,"Hepatus princeps" , "Cantharus" )))

# manual edits 
tt <- data.frame(Species = c("Cypselurus heterurus","Cypselurus cyanopterus" ,"Cypselurus melanurus" ,"Nealotus tripes","Leptocephalus" ), group= "GRP6" )
tt <- rbind(tt,data.frame(Species = c("Holanthias martinicensis", "Gnatholepis thompsoni","Corythoichthys albirostris" ,"Paradiplogrammus bairdi" ,
                                      "Gilbertidia sigalutes"), group= "GRP2" ))  
tt <- rbind(tt,data.frame(Species = c("Beringraja binoculata" , "Rhinobatos lentiginosus"),group= "GRP3"))
tt <- rbind(tt,data.frame(Species = c("Hemipteronotus novacula", "Pomacentrus leucostictus","Pomacentrus partitus","Pomacentrus variabilis" ,"Pomacentrus fuscus" ,
                                      "Hemipteronotus martinicensis" ,"Caelorinchus scaphopsis"),group= "GRP4"))
tt <- rbind(tt,data.frame(Species =c("Prognatholiparis ptychomandibularis" , "Ulcina olrikii" ,"Gnathagnus egregius"),group= "GRP7"))

gd_new <- cbind(gd_new,tt[match(gd_new$spec,tt$Species),c("group")])
colnames(gd_new)[ncol(gd_new)] <- "q_group7"

gd_new$q_group <- ifelse(is.na(gd_new$q_group), gd_new$q_group7,gd_new$q_group) 

gd_new <- gd_new %>%
  select(-q_group2,-q_group3,-q_group4,-q_group5,-q_group6,-q_group7,-habitat,-body.shape, -capitals)

# now get efficiencies
# for the species
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

nefsc_GRP <- nefsc_GRP[-7,]
nefsc_GRP <- rbind(nefsc_GRP,data.frame(GRP="GRP5",effic=0.3418750))

# now combine with species list
gd_new <- cbind(gd_new,nefsc_spec[match(gd_new$q_group,nefsc_spec$SVSPP),c("effic")])
colnames(gd_new)[ncol(gd_new)] <- "Efficiency"

gd_new <- cbind(gd_new,nefsc_GRP[match(gd_new$q_group,nefsc_GRP$GRP),c("effic")])
colnames(gd_new)[ncol(gd_new)] <- "effic"

gd_new$Efficiency <- ifelse(is.na(gd_new$Efficiency), gd_new$effic,gd_new$Efficiency)

gd_new <- gd_new %>%
  select(-effic)

save(gd_new,file = "traits and species/oceanadapt_species_qvalues.RData")

