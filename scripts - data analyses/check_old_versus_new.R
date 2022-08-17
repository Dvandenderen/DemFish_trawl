
load("cleaned data/surveyed_grid.RData") # get grid information
load("cleaned data/Depth_grid.RData") # get depth per grid cell and year
load("cleaned data/220224_biomass_grid.RData") # get biomass per grid cell and year
load("cleaned data/sstdat_1967_2018_COBE.RData") # get SST COBE

source("scripts - data analyses/source_get_bio_function.R") # source script to obtain biomass per ecoregion/subdiv

t_start <- 2010
t_end <- 2015

# select data
dat <- get_bio(cpue_good,t_start = t_start,t_end = t_end,spatialunit = "ECO_REG")  # spatialunit = "ECO_REG" or "subdivision"
dat$ER <- dat$Catch_sqkm/dat$biomass
summary(lm(dat$biomass~dat$SST_time + dat$tlw +log10(dat$ER)+ dat$lz_prod))

# select new data
load("cleaned data/220720_biomass_grid.RData") # get biomass per grid cell and year
source("scripts - data analyses/source_get_bio_function_new.R") # source script to obtain biomass per ecoregion/subdiv

# select data
dat_new <- get_bio(cpue_good,t_start = t_start,t_end = t_end,spatialunit = "ECO_REG")  # spatialunit = "ECO_REG" or "subdivision"
dat_new$ER <- dat_new$Catch_sqkm/dat_new$biomass
summary(lm(dat_new$biomass~dat_new$SST_time + dat_new$tlw +log10(dat_new$ER) + dat_new$lz_prod))

plot(dat$biomass,dat_new$biomass)
abline(0,1)
