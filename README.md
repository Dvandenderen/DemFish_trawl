# Demersal fish commmunity biomass in the Northeast Pacific and North Atlantic shelf regions.

[![DOI](https://zenodo.org/badge/395102730.svg)](https://zenodo.org/badge/latestdoi/395102730)

The data processing scripts are modified based on earlier work from Pinsky et al. (2013) and Maureaud et al. (2019). The final dataset contains approx. 197,000 
unique tows and includes data from 1970 to 2019 (166,000 tows between 1980-2015). All data used are publicly available and were downloaded in July 2021. We selected scientific surveys that sampled the fish community with (predominantly) otter trawls. For each tow in each survey, we selected all teleost and elasmobranch species and obtained species weight. We corrected these weights for differences in sampling area (in km2) and trawl gear catchability.

If the correction for gear catchability is not important, please use the FishGlob database (DOI: [10.5281/zenodo.7484547](https://zenodo.org/record/7527447)).

## Publication
The data were used to examine the drivers of fish community biomass in the Northeast Pacific and North Atlantic shelf regions. A pre-print with the results is available here: 10.22541/au.167275091.18905396/v1

## Processed dataset
Secondary data product is processed data for all three regions combined. The secondary data product is archived as a .csv data table in ZENODO with a DOI: xxx. The data table includes haul id, region, gear type, sampling year, sampling month, sampling coordinates (longitude, latitude), swept area, bottom depth, species taxonomy (species and family name), species trophic level, estimated weight (kg) per km2 and estimated weight (kg) per km2 corrected for gear catchability. The data product can also be obtained by running "scripts - data processing/source_combine_all_surveys_after_cleaning.R" 

## Scripts to analyse the data
* Individual survey data sets can be found in the data folder. There are three data processing scripts:
  * [ICES DATRAS data processing](https://github.com/Dvandenderen/DemFish_trawl/blob/master/scripts%20-%20data%20processing/compile_DATRAS_with_catchability.R)
  * [Norwegian data processing](https://github.com/Dvandenderen/DemFish_trawl/blob/master/scripts%20-%20data%20processing/compile_NORBTS_with_catchability.R)
  * [US and Canada data processing](https://github.com/Dvandenderen/DemFish_trawl/blob/master/scripts%20-%20data%20processing/compile_oceanadapt_dvd.R)

* Average biomass, in tonnes per km2, was estimated per equal area grid cell (6000 km2) and surveyed year:
  * [Create grid with surveyed area](https://github.com/Dvandenderen/DemFish_trawl/blob/master/scripts%20-%20data%20analyses/Create_grid_with_surveyed_area.R)

* The corrected trawl survey biomasses were compared with available fisheries stock assessment biomasses to validate the range and distribution of the estimates:
  * [Calculate overlap between stock assessment areas and survey area](https://github.com/Dvandenderen/DemFish_trawl/blob/master/scripts%20-%20data%20analyses/Calculate%20overlap%20between%20stock%20assessment%20areas%20and%20survey%20area.R)
  * [Estimate stock biomass for the two methods](https://github.com/Dvandenderen/DemFish_trawl/blob/master/scripts%20-%20data%20analyses/Comparison_trawlsurvey_stockassessments.R)
  * [Compare outputs](https://github.com/Dvandenderen/DemFish_trawl/blob/master/figures/Stock_ass_figure.R)

* Spatial and trophodynamic model analysis
  * [Create datasets for analyses](https://github.com/Dvandenderen/DemFish_trawl/blob/master/scripts%20-%20data%20analyses/Obtain_data_for_regional_statistics.R)
  * [SEM model](https://github.com/Dvandenderen/DemFish_trawl/blob/master/scripts%20-%20data%20analyses/Spatial%20analysis.R)
  * [Wavelet-revised regression model](https://github.com/Dvandenderen/DemFish_trawl/blob/master/scripts%20-%20data%20analyses/Spatial%20analysis%20local%20grid%20resolution.R) 
  * [Trophodynamic model](https://github.com/Dvandenderen/DemFish_trawl/blob/master/scripts%20-%20data%20analyses/Trophodynamics_model.R)

* Time-series analyses
  * [Create datasets for analyses](https://github.com/Dvandenderen/DemFish_trawl/blob/master/scripts%20-%20data%20analyses/Obtain_data_for_regional_statistics_timeseries.R) 
  * [Time-series models](https://github.com/Dvandenderen/DemFish_trawl/blob/master/scripts%20-%20data%20analyses/Timeseries_analysis_recursive.R)

All steps can be run with the output provided on GitHub without running any prior steps.
