# Esimating demersal fish commmunity biomass

The data processing scripts are modified based on earlier work from Pinsky et al. (2013) and Maureaud et al. (2019). The final dataset contains approx. 180,000 unique tows and includes data from 1970 to 2019. All data used are publicly available and were downloaded in July 2021. We selected scientific surveys that sampled the fish community with (predominantly) otter trawls. For each tow in each survey, we selected all demersal teleost and elasmobranch species and obtained species weight. We corrected these weights for differences in sampling area (in km2) and trawl gear catchability. The final data gives a standardized fish biomass (in kg per km2) per species, haul and survey. 

The data were used to examine the effects of temperature, and other environmental variables, on fish community biomass in the Northeast Pacific and North Atlantic shelf regions. A pre-print with the results is available here: 10.22541/au.167275091.18905396/v1

To obtain the final dataset, run: DemFish_trawl/scripts - data processing/source_combine_all_surveys_after_cleaning.R 
