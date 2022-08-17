
rm(list=ls())

### load libraries
library(rgdal)
library(raster)
library(sp)
library(rgeos)
library(maptools)
library(dplyr)
library(sf)
library(tidyverse)
library(lavaan)

##################
load("cleaned data/surveyed_grid.RData") # get grid information
load("cleaned data/Depth_grid.RData") # get depth per grid cell and year
load("cleaned data/Biomass_grid.RData") # get biomass per grid cell and year
load("cleaned data/sstdat_1967_2018_COBE.RData") # get SST COBE
load("cleaned data/tdat_1993_2016_Glorys.RData")

source("scripts - data analyses/source_get_watercol_temp.R") # source script to obtain biomass per ecoregion/subdiv

save.image (file = "figures/my_work_space_watercolT.RData")

# ------------------------------------------------------------------------------
# R Markdown -->> run SEM final - knitted output_bottomtemp #
# ------------------------------------------------------------------------------

  # analyse output
  datSEM <- read.csv("figures/SEM_out_waterT.csv")
  
  # get average standardized coef
  datSEMavg <- data.frame(datSEM[1:9,2:4],AVG_all = rowMeans(data.frame(datSEM[1:9,11],datSEM[81:89,11])),
                          AVG_years = rowMeans(data.frame(datSEM[21:29,11],datSEM[41:49,11],datSEM[61:69,11],
                                                          datSEM[101:109,11],datSEM[121:129,11],datSEM[141:149,11])))
  
  Rsqavg <- data.frame(datSEM[17:20,2:4],AVG_all = rowMeans(data.frame(datSEM[17:20,6],datSEM[97:100,6])),
                       AVG_years = rowMeans(data.frame(datSEM[37:40,6],datSEM[57:60,6],datSEM[77:80,6],
                                                       datSEM[117:120,6],datSEM[137:140,6],datSEM[157:160,6])))
  
  # get nb of times p <0.05
  pcount = data.frame(datSEM[1:9,9],datSEM[81:89,9])
  datSEMavg$pcount_all <- rowSums(pcount <0.05)
  
  pcount = data.frame(datSEM[21:29,9],datSEM[41:49,9],datSEM[61:69,9],
                      datSEM[101:109,9],datSEM[121:129,9],datSEM[141:149,9])
  datSEMavg$pcount_years <- rowSums(pcount <0.05)
  datSEMavg;Rsqavg

