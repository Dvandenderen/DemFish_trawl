
#### step 1 -  cleaning of all individual data sets
# 

#### step 2 - create surveyed grid
#
  # run script to create equal area size of 6000 km2
  # --> "scripts - data analyses/Create_grid_with_surveyed_area.R"
  # --> "scripts - data analyses/Create_grid_with_surveyed_area_1lon_05lat.R"

#### Step 3 - combine stock assessment data with survey data for different regions
#
  # run script to calculate overlap between stock assessment area 
  # --> "scripts - data analyses/Calculate overlap between stock assessment areas and survey area.R"
  
  # run script to estimate stock biomass for the two methods
  # --> "scripts - data analyses/Comparison_trawlsurvey_stockassessments.R")
  
  # make figures for stock assessment comparison
  # -->  "figures/Stock_ass_figure.R"

#### Step 4 - create datasets for statistical model, per grid / subdivision / ecoregion
  # --> "scripts - data analyses/Obtain_data_for_regional_statistics.R"
  # --> "scripts - data analyses/Obtain_data_for_regional_statistics_1lon_05lat.R"

#### Step 5 - run statistical models

#### Step 6 - get grid cells that can be used to analyse time-series
  # --> "source_gridcell_timeseries.R"
  # --> "source_fill_gaps_timeseries.R"



