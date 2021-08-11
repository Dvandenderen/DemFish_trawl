
library(worms)
library(worrms)
library(stringr)
library(dplyr)

# load species gear efficiency
geareff <- read.csv(file = "data/Walkeretal_2017_supp/EfficiencyTab.csv",sep=",",header = T)

# get unique name and grouping code
uni_name <- geareff %>%
            distinct(Species, Group, Code)

uni_name <- subset(uni_name,!(uni_name$Code %in% c("GRP1","GRP2","GRP3","GRP4","GRP5","GRP6","GRP7")))

# load latin name
latinname <- read.csv(file = "data/Walkeretal_2017_supp/common_to_latin.csv",sep=",",header = T)

# cbind to get latin name
uni_name <- cbind(uni_name,latinname[match(uni_name$Code,latinname$Code),c(1)])
colnames(uni_name) <- c("Code","Common","Group","Species")

# get aphiaID from Walker et al dataset
my_sp_taxo <- wm_record_(name = uni_name$Species)
df_test <- data.frame(do.call(rbind, my_sp_taxo))
my_sp_taxo2 <- wm_record_(c(126892,367297,127147,403388)) # four species that did not work
df_test2 <- data.frame(do.call(rbind, my_sp_taxo2))
df_test <- rbind(df_test,df_test2)
uni_name <- cbind(uni_name,df_test[match(uni_name$Species,df_test$scientificname),c("valid_AphiaID","family","genus")])

# combine with survey based on AphiaID 
survey_new <- cbind(survey,uni_name[match(survey$AphiaID,uni_name$valid_AphiaID),c("Group")])
colnames(survey_new)[ncol(survey_new)] <- "q_group"

# based on genus
survey_new <- cbind(survey_new,uni_name[match(survey_new$Genus,uni_name$genus),c("Group")])
colnames(survey_new)[ncol(survey_new)] <- "q_group2"
survey_new$q_group <- ifelse(is.na(survey_new$q_group), survey_new$q_group2,survey_new$q_group) 

# and for the remaining species use trait information to classify
traits <- read.csv("traits and species/Beukhofetal_2019/Traits_fish.csv",header=T,sep=";",row.names=NULL)
survey_new <- cbind(survey_new,traits[match(survey_new$Species,traits$taxon),c("habitat","body.shape")])

survey_new$q_group3 <- NA
survey_new$q_group3 <- ifelse(survey_new$habitat %in% c("benthopelagic","reef-associated") ,"GRP4",survey_new$q_group3)
survey_new$q_group3 <- ifelse(survey_new$body.shape == "flat","GRP3",survey_new$q_group3)
survey_new$q_group3 <- ifelse(survey_new$body.shape %in% c("eel-like" , "fusiform","compressiform","elongated") & survey_new$habitat %in% c("demersal","bathydemersal"),"GRP2",survey_new$q_group3)
survey_new$q_group3 <- ifelse(survey_new$habitat %in% c("pelagic","bathypelagic"),"GRP6",survey_new$q_group3)
survey_new$q_group3 <- ifelse(survey_new$body.shape %in% c("short and/or deep") & survey_new$habitat  %in% c("demersal","bathydemersal") ,"GRP7",survey_new$q_group3)
survey_new$q_group <- ifelse(is.na(survey_new$q_group), survey_new$q_group3,survey_new$q_group) 

# and finally classify manually 
tt <- data.frame(Species = c("Gasterosteus aculeatus aculeatus","Thunnus thynnus", "Esox lucius" ,  "Coregonus albula" ,"Nezumia sclerorhynchus",
                             "Scomberesox saurus"  ,"Stomias boa" , "Scopelogadus" ), group= "GRP6" )
tt <- rbind(tt,data.frame(Species = c("Syngnatus", "Blennius"  ,  "Acipenser sturio" ,"Perca fluviatilis" ,  "Lipophrys pholis", "Pagellus bellottii", 
                                       "Pentanemus quinquarius"  , "Stichaeus punctatus" ,"Lycodichthys dearborni","Lepidion schmidti"), group= "GRP2" ))  
tt <- rbind(tt,data.frame(Species = c("Dasyatis tortonesei"),group= "GRP3"))
tt <- rbind(tt,data.frame(Species = c("Gymnocephalus cernua","Rutilus rutilus",  "Abramis brama" ,"Vimba vimba" ,
                                      "Pagellus", "Abramis"),group= "GRP4"))
tt <- rbind(tt,data.frame(Species =c("Hippocampus guttulatus" , "Paraliparis copei"),group= "GRP7"))
survey_new <- cbind(survey_new,tt[match(survey_new$Species,tt$Species),c("group")])
colnames(survey_new)[ncol(survey_new)] <- "q_group4"
survey_new$q_group <- ifelse(is.na(survey_new$q_group), survey_new$q_group4,survey_new$q_group) 

# remove all extra columns
survey_new <- survey_new %>%
  select(-q_group2,-q_group3,-q_group4,-habitat,-body.shape)

# now select the species for which there is species-specific information for the GOV
geareff_gov <- subset(geareff,geareff$Gear == "GOV")
geareff_gov <- subset(geareff_gov,!(is.na(geareff_gov$Efficiency)))
geareff_gov <- subset(geareff_gov,!(geareff_gov$Code %in% c("GRP1","GRP2","GRP3","GRP4","GRP5","GRP6","GRP7")))
geareff_gov  <- cbind(geareff_gov,uni_name[match(geareff_gov$Code,uni_name$Code),c("valid_AphiaID")])
colnames(geareff_gov)[ncol(geareff_gov)] <- "AphiaID"
survey_new  <- cbind(survey_new,geareff_gov[match(survey_new$AphiaID,geareff_gov$AphiaID),c("Code")])
colnames(survey_new)[ncol(survey_new)] <- "q_code"

# if q_code (species-specific) info available use it, otherwise use q of the group
survey_new$q_group <- ifelse(is.na(survey_new$q_code),survey_new$q_group,survey_new$q_code)

q_names <- survey_new %>%
  select(AphiaID,Species,Genus,Family,q_group) %>%
  distinct(AphiaID,Species,Genus,Family,q_group)
row.names(survey_new) <- NULL

save(q_names,file="traits and species/Names_DATRAS_Walker_match.Rdata")

rm(survey_new,geareff_gov,tt,geareff,uni_name,latinname,my_sp_taxo,my_sp_taxo2,df_test,df_test2,traits)
