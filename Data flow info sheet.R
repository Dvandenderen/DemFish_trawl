
# ------------------------------------------------------------------------------
# step 1 -  processomg of all individual data sets
# ------------------------------------------------------------------------------
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
# Step 4 - create datasets for statistical model per grid 
# ------------------------------------------------------------------------------

  # --> "scripts - data analyses/Obtain_data_for_regional_statistics.R"


# ------------------------------------------------------------------------------
# Step 5 - run statistical models - spatial and trophodynamic equation
# ------------------------------------------------------------------------------
 
  # --> "scripts - data analyses/Spatial analysis.R"
  # --> "figures/SEM - knitted output/Rmd"
  # --> "scripts - "scripts - data analyses/Spatial analysis local grid resolution.R"
  # --> "scripts - "scripts - data analyses/trophodynamics model.R"

# ------------------------------------------------------------------------------
# Step 6 - get grid cells that can be used to analyse time-series
# ------------------------------------------------------------------------------
  # --> "scripts - data analyses/Obtain_data_for_regional_statistics_timeseries.R"
  # --> "scripts - data analyses/Timeseries_analysis_recursive.R"


