
library(worms)
library(worrms)
library(stringr)

full <- readRDS("processed data/all-regions-full-oceanadapt.rds")
spec <- unique(full$spp)
spec <- str_remove_all(spec, " spp.")
spec <- str_remove_all(spec, " sp.")

ptm <- proc.time()
my_sp_taxo <- wm_record_(name = spec) # takes some time (could split up in steps of 1000)
df_test <- data.frame(do.call(rbind, my_sp_taxo))
tot <- df_test
proc.time() - ptm

# combine with original species information
newspec <- data.frame(spec = spec)
newspec <- cbind(newspec,tot[match(newspec$spec,tot$scientificname),c(1:27)])

# save step in between
# getwd()
# save(newspec,file="oceanadapt_species_list.RData")

# get all NA's
nospec <- subset(newspec,is.na(newspec$AphiaID))
nospec <- data.frame(spec = nospec$spec)
nospec <- nospec %>% 
    separate(spec, c("first", "second"))

my_sp_taxo <- wm_records_name(name = nospec$first[1],fuzzy = T,marine_only = TRUE)
df_test <- data.frame(do.call(cbind, my_sp_taxo[1,]))
tot <- df_test
for(i in 2:nrow(nospec)){
my_sp_taxo <- try(wm_records_name(name = nospec$first[i],fuzzy = T,marine_only = TRUE))
if(!(is.null(nrow(my_sp_taxo)))){
df_test <- data.frame(do.call(cbind, my_sp_taxo[1,]))
tot <- rbind(tot,df_test)
}}

# get all non NA's
goodspec <- subset(newspec,!(is.na(newspec$AphiaID)))

# add names based on second match
goodspec2 <- cbind(nospec,tot[match(nospec$first,tot$scientificname),c(1:27)])
goodspec2$first <- nospec$spec
goodspec2 <- goodspec2[,-2]
colnames(goodspec2) <- colnames(goodspec)

# combine and save
goodspec <- rbind(goodspec,goodspec2)
save(goodspec,file="traits and species/oceanadapt_species_list.RData")

