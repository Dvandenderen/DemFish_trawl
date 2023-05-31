
# ------------------------------------------------------------------------------
# The data processing scripts are modified based on earlier work from Pinsky et 
# al. (2013) and Maureaud et al. (2019). The final dataset contains approx. 
# 180,000 unique tows and includes data from 1970 to 2019. All data used are 
# publicly available and were downloaded in July 2021. We selected scientific 
# surveys that sampled the fish community with (predominantly) otter trawls. 
# For each tow in each survey, we selected all demersal teleost and elasmobranch 
# species and obtained species weight. We corrected these weights for 
# differences in sampling area (in km2) and trawl gear catchability. The 
# final data gives a standardized fish biomass (in kg per km2) per species, 
# haul and survey.
#
# The data were used to examine the effects of temperature, and other 
# environmental variables, on fish community biomass in the Northeast Pacific 
# and North Atlantic shelf regions.

# ------------------------------------------------------------------------------
# step 1 -  processing of all individual data sets
# ------------------------------------------------------------------------------
  # individual data sets can be found in the data folder
  # compile_DATRAS_with_catchability.R
  # compile_NORBTS_with_catchability.R
  # compile_oceanadapt_dvd.R

# ------------------------------------------------------------------------------
# step 2 - create surveyed grid
# ------------------------------------------------------------------------------

  # run script to create equal area size of 6000 km2
  # --> "scripts - data analyses/Create_grid_with_surveyed_area.R"

# ------------------------------------------------------------------------------
# Step 3 - combine stock assessment data with survey data for different regions
# ------------------------------------------------------------------------------

  # run script to calculate overlap between stock assessment area 
  # --> "scripts - data analyses/Calculate overlap between stock assessment areas and survey area.R"
  
  # run script to estimate stock biomass for the two methods
  # --> "scripts - data analyses/Comparison_trawlsurvey_stockassessments.R")
  
  # make figures for stock assessment comparison
  # -->  "figures/Stock_ass_figure.R"

# ------------------------------------------------------------------------------
# Step 4 - create datasets for statistical model
# ------------------------------------------------------------------------------

  # --> "scripts - data analyses/Obtain_data_for_regional_statistics.R"
  

# ------------------------------------------------------------------------------
# Step 5 - run statistical models - spatial and trophodynamic equation
# ------------------------------------------------------------------------------
 
  # --> "scripts - data analyses/Spatial analysis.R"
  # --> "scripts - data analyses/Spatial analysis local grid resolution.R"
  # --> "scripts - data analyses/Trophodynamics_model.R"

# ------------------------------------------------------------------------------
# Step 6 - analyse time-series
# ------------------------------------------------------------------------------
  # --> "scripts - data analyses/Obtain_data_for_regional_statistics_timeseries.R"
  # --> "scripts - data analyses/Timeseries_analysis_recursive.R"


