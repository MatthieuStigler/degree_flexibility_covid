# Replication code for the paper Researchers’ Degrees of Flexibility: Revisiting COVID-19 Policy Evaluations

The code is written in R, and uses an Rstudio R project which will take care of the relative paths of the different files. 
It is there recommended to run the script from Rstudio by loading the Rproject `degree_flexibility_covid.Rproj`.

The code is also hosted on https://github.com/MatthieuStigler/degree_flexibility_covid (but only the code), this is done to allow to make eventual updates to the code if mistakes are discovered. 

## Data dissemination

This dataset offers aggregated and transformed data derived from Google Mobility, Safegraph, Cuebiq, and PlaceIQ. To respect their data use agreements, we only provide anonymized county-level information.

## How to run this, Software Requirements: R and R packages


This folder uses R and the Rstudio R-project system. This allows all file paths to be expressed relatively to where the file `degree_flexibility_covid.Rproj` is located. Users should open the `degree_flexibility_covid.Rproj` in Rstudio. Alternatively, they should set the working directory to where `degree_flexibility_covid.Rproj` is located. 

R and R package versions can be read from the `renv.lock` file. This project was configured with `renv`, although it is deactivated by default within this project. To activate it, and hence get the same package versions as used for the paper, run: 

```
renv::activate()
renv::restore()
```
For more information, see https://rstudio.github.io/renv/articles/renv.html

The project also uses two non-CRAN packages, tht can either be installed manually using the command below, or by running `renv::activate()`

- library("matPkg") version 0.2.50, use: `remotes::install_github("MatthieuStigler/matPkg", upgrade = "never")`
- library("multidiff") version 0.3.0, use: `remotes::install_github("MatthieuStigler/multiDiff", upgrade = "never")`

## Computational requirements

The full project was run (using `master_run_all.R`, see below) on a Linux server with 128 GB of RAM. R version was 4.4.1 (see also `renv.lock`). Some scripts in the folder `code/1_code_analysis` are memory and time intensive, and thus their output is made available in the folder `data_replicate/3_data_analysis_output` to avoid lengthy run times. 

Some files are particularly slow:

- `code/1_code_analysis/2_regression_event_study_CSestimator.R`
- `code/1_code_analysis/3_events_all_outcomes_all_specs_F_run.R` 

They use parallel processing, and thus can be very memory intensive. One might want to adjust the `cores_number = 12` value to reduce RAM memory usage. 




## Folder structure 

### quick overview

To produce the figures and tables in the paper, re-run scripts in the `code/2_code_figures_tables` folder. 
Each script contains in its name the specific figure/table of the paper. The output of these scripts is stored in the `output_replicate` folder. 

The file `code/master_run_all.R` can run all the files. By default (ie. with option `run_heavy=FALSE`), it runs only the final files to create the figures and tables. Setting `run_heavy=TRUE` will re-run the regression files, which are very slow and heavy (running  `code/master_run_all.R` with `run_heavy=TRUE` took about 3 hours 40 on a server).

### full description

The project is structured as following:

``` bash
├── code
│   ├── 0_code_clean
│   ├── 1_code_analysis
│   ├── 2_code_figures_tables
│   ├── auxiliary_packages
│   └── auxiliary_scripts
├── data_replicate
│   ├── 0_data_raw
│   ├── 1_data_intermediate
│   ├── 2_data_final
│   └── 3_data_analysis_output
├── output_replicate
└── renv
```

  
- `code`: 5 folders
  - a `master_run_all.R` file that runs all the files
  - `0_code_clean`: initial code to clean/create the various datasets (0_* scripts) and to merge the datasets (1_* scripts)
  - `1_code_analysis`: code to produce the various DiD and event-study coefficients
  - `2_code_figures_tables`: code to produce the figures and tables in the paper. 
  - `auxiliary_scripts`: auxiliary scripts that only contain raw functions that will be used within other scrips by using `source()`
  - `auxiliary_packages`: auxiliary packages that are not on CRAN but on github. While we recommend installing these packages using `renv`, these are kept here in case for the sake of reproducibility. 
- `data_replicate`: input data. In particular:
  - `0_data_raw`: raw data files
  - `1_data_intermediate`: datasets processed within this project by the scripts in `code/0_code_clean`
  - `2_data_final`: the final datasets used in the analysis. Most importantly:
    - `merged_panel_did.rds` the main dataset, in R's native `.rds` format.
    - `merged_panel_event_dummies.rds` the main dataset in event-study form
  - `3_data_analysis_output`: output from the DiD, CS etc analysis. As these analyses can be time- and memory-consuming, it is preferable to store them before plotting. 
- `output_replicate`: output figures/tables
- `renv`: this is a folder used by `renv`



### Specific files

Due to data sharing restrictions, some files cannot be run without downloading data:

- `0_clean_map_us_counties.R` (in `code/0_code_clean`): this downloads census data. You will need to request an API key at http://api.census.gov/data/key_signup.html and use it with `tidycensus::census_api_key()`
- `0a_clean_safegraph_aggregate_SD_county.R` and `0b_clean_safegraph_county_level_SD.R` (in `code/0_code_clean`): these scripts require access to the Safegraph raw data files, access can be requested at https://www.safegraph.com/
- `0_clean_cuebiq.R` (in `code/0_code_clean`): these scripts require access to the Cuebiq raw data files, access can be requested at https://www.safegraph.com/ https://www.cuebiq.com/

As a consequence, some files cannot be run either:

- `code/0_code_clean/1_2_merge_main_datasets`: requires the safegraph and cuebiq data

However, files in `code/1_code_analysis` and `code/2_code_figures_tables` still can be run. 

### master_run_all

The file `code/master_run_all.R` runs all files, except the ones cited above. It contains a `run_heavy` option, which is  `FALSE` by default, thus excluding very heavy files from running. 

The file `code/1_code_analysis/2_regression_event_study_CSestimator.R` uses a very important amount of RAM memory (120 GB!!) when used with parallel processing. To avoid too heavy computations, it needs to be run twice by changing one line of code, see line `## This needs to be manually adjusted! run twice`. 




## Data Availability and Provenance Statements

### Summary of Availability

Some data cannot be made publicly available.

### Details on each Data Source


`0_data_raw` contains 5 files:

  - `all_cities_policy.rds`: collected by the authors and MTurk workers through 2020-05-01
  - `cities_min_policy.rds`: collected by the authors and MTurk workers through 2020-05-01
  - `clean_states_declarations.rds`: collected by the authors and MTurk workers through 2020-05-01
  - `counties_declarations_cleaned.rds` collected by the authors and MTurk workers through 2020-05-01, checked against NACO https://ce.naco.org/?dset=COVID-19&ind=Emergency%20Declaration%20Types 
  - `counties_declarations_panel.rds`: collected by the authors and MTurk workers through 2020-05-01, checked against NACO https://ce.naco.org/?dset=COVID-19&ind=Emergency%20Declaration%20Types
  - `county_dex.csv`: downloaded 2020-04-26 from Github  "https://raw.githubusercontent.com/COVIDExposureIndices/COVIDExposureIndices/master/dex_data/county_dex.csv
  - `countypres_2000-2016.csv`: downloaded 2020-04-26 from MIT Election lab, https://electionlab.mit.edu/data
  - `DAILY_WEATHER.dta`: downlaoded 2020-04-30 from Global Historical Climatology Network Daily  https://www.ncei.noaa.gov/products/land-based-station/global-historical-climatology-network-daily
  - `google_mobility_raw.csv`: downloaded 2020-04-26 from Google: https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv?cachebust=911a386b6c9c230f
  - `NYT_us_counties.csv`: downloaded 2020-04-26 from New York Times, https://github.com/nytimes/covid-19-data/archive/master.zip
  - `state_policies_dateissued_group.rds`: downloaded 2020-04-26 from https://raw.githubusercontent.com/COVID19StatePolicy/SocialDistancing/master/data/USstatesCov19distancingpolicy.csv

| Data name                     | Data files                          | Description                                                                                                                                                                                                                                           | Location                       | Provided |
|-------------------------------|-------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|--------------------------------|----------|
| All city-level MRP            | all_cities_policy.rds              | City-level mobility-restricting policies, collected by the authors and MTurk workers through 2020-05-01                                                                                                                                              | data_replicate/0_raw_data/     | Yes      |
| Earliest city-level MRP       | cities_min_policy.rds              | City-level first mobility-restricting policies, collected by the authors and MTurk workers through 2020-05-01                                                                                                                                        | data_replicate/0_raw_data/     | Yes      |
| All states MRP                | clean_states_declarations.rds      | State-level mobility-restricting policies, collected by the authors and MTurk workers through 2020-05-01                                                                                                                                             | data_replicate/0_raw_data/     | Yes      |
| All counties MRP              | counties_declarations_cleaned.rds  | County-level mobility-restricting policies, collected by the authors and MTurk workers through 2020-05-01, checked and completed with NACO data: [NACO](https://ce.naco.org/?dset=COVID-19&ind=Emergency%20Declaration%20Types)                      | data_replicate/0_raw_data/     | Yes      |
| Counties MRP, panel format    | counties_declarations_panel.rds    | County-level mobility-restricting policies in a panel format, collected by the authors and MTurk workers through 2020-05-01, checked and completed with NACO data: [NACO](https://ce.naco.org/?dset=COVID-19&ind=Emergency%20Declaration%20Types)    | data_replicate/0_raw_data/     | Yes      |
| PlaceIQ Device exposure metric| county_dex.csv                     | PlaceIQ device exposure data, downloaded 2020-04-26 from [Github](https://raw.githubusercontent.com/COVIDExposureIndices/COVIDExposureIndices/master/dex_data/county_dex.csv)                                                                        | data_replicate/0_raw_data/     | Yes      |
| County presidential election data | countypres_2000-2016.csv       | Presidential election data, downloaded 2020-04-26 from MIT Election lab, [MIT Election Lab](https://electionlab.mit.edu/data)                                                                                                                        | data_replicate/0_raw_data/     | Yes      |
| Daily weather data            | DAILY_WEATHER.dta                  | Daily weather data, downloaded 2020-04-30 from Global Historical Climatology Network Daily [GHCN](https://www.ncei.noaa.gov/products/land-based-station/global-historical-climatology-network-daily)                                               | data_replicate/0_raw_data/     | Yes      |
| Google mobility data          | google_mobility_raw.csv            | Google mobility data downloaded 2020-04-26 from [Google Mobility Report](https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv?cachebust=911a386b6c9c230f)                                      | data_replicate/0_raw_data/     | Yes      |
| New York Times covid tracker data | NYT_us_counties.csv            | Covid tracker data, downloaded 2020-04-26 from New York Times, [NYT](https://github.com/nytimes/covid-19-data/archive/master.zip)                                                                             | data_replicate/0_raw_data/     | Yes      |
| State-level MRP complement data | state_policies_dateissued_group.rds | State level covid policies, downloaded 2020-04-26 from [COVID19StatePolicy](https://raw.githubusercontent.com/COVID19StatePolicy/SocialDistancing/master/data/USstatesCov19distancingpolicy.csv)               | data_replicate/0_raw_data/     | Yes      |
| Safegraph mobility data       |                                     |                                                                                                                                                                                                                                                       |                                | No       |
| Cuebiq mobility data          |                                     |                                                                                                                                                                                                                                                       |                                | No       |


| Data files                                  | Description                                      | How created                                                                                      | Location                            | Provided |
|---------------------------------------------|--------------------------------------------------|--------------------------------------------------------------------------------------------------|-------------------------------------|----------|
| US_states_dataOnly.csv                      | Cleaned US states                               | 0_code_clean/0_clean_us_states.R                                                                  | data_replicate/1_data_intermediate  | Yes      |
| cases_mortality_nyt.rds                     | Cleaned COVID cases                             | 0_code_clean/0_clean_cases_mortality_NYT.R                                                       | data_replicate/1_data_intermediate  | Yes      |
| county_pres_elections_2016.rds              | Cleaned presidential elections data             | 0_code_clean/0_clean_county_presidential_elections.R                                             | data_replicate/1_data_intermediate  | Yes      |
| declarations_counties_states_cases_dates.rds | Cleaned counties declaration data               | 0_code_clean/1_1_merge_counties_states_declarations_create_panel.R                               | data_replicate/1_data_intermediate  | Yes      |
| declarations_counties_states_dates.rds       | Clean counties and states declarations          | 0_code_clean/1_1_merge_counties_states_declarations_create_panel.R                               | data_replicate/1_data_intermediate  | Yes      |
| declarations_counties_states_panel.rds       | Cleaned states declarations, panel format       | 0_code_clean/1_1_merge_counties_states_declarations_create_panel.R                               | data_replicate/1_data_intermediate  | Yes      |
| google_mobility_clean.csv                   | Cleaned Google mobility                         | 0_code_clean/0_clean_googleMob_csv.R                                                             | data_replicate/1_data_intermediate  | Yes      |
| placeIQ_county_device_exposure.rds          | Cleaned device exposure (PlaceIQ) data          | 0_code_clean/0_clean_placeIQ_county_dex.R                                                        | data_replicate/1_data_intermediate  | Yes      |
| us_acs5_sf.rds                              | Cleaned county census data                      | 0_code_clean/0_clean_map_us_counties.R                                                           | data_replicate/1_data_intermediate  | Yes      |
| weather_cleaned.rds                         | Cleaned weather data                            | 0_code_clean/0_clean_weather.R                                                                   | data_replicate/1_data_intermediate  | Yes      |
| vars_names_and_formulas                     | Set of files manually constructed for models    | 1_code_analysis/0_1_generate_table_regression_formula.R, 0_2_generate_table_covariates.R, etc.   | data_replicate/1_data_intermediate  | Yes      |
| Cuebiq cleaned                              | Cleaned Cuebiq mobility data                    | 0_code_clean/0_clean_cuebiq.R                                                                    |                                     | No       |
| Sagegraph mobility data aggregated          | Aggregated Safegraph data to the county level   | 0_code_clean/0a_clean_safegraph_aggregate_SD_county.R                                            |                                     | No       |
| Sagegraph mobility data aggregated/cleaned  | Clean county-level aggregated Safegraph data    | 0_code_clean/0b_clean_safegraph_county_level_SD.R                                                |                                     | No       |
| merged_panel_did.rds                        | Merged panel for static two-way fixed effects   | 0_code_clean/1_2_merge_main_datasets.R                                                           | data_replicate/2_data_final/        | Yes      |
| merged_panel_did_long.rds                   | Merged panel, long version for fixed effects    | 0_code_clean/1_2_merge_main_datasets.R                                                           | data_replicate/2_data_final/        | Yes      |
| merged_panel_event_dummies.rds              | Merged panel for dynamic event studies models   | 0_code_clean/1_3_merge_main_datasets_panel_event.R                                               | data_replicate/2_data_final/        | Yes      |
| regression_event_study_CS                   | Folder for Callaway-Sant'Anna regression objects | 1_code_analysis/2_regression_event_study_CSestimator.R                                           | data_replicate/3_data_analysis_output/ | Yes   |
| regression_event_study_vanilla              | Folder for two-way fixed effects event studies  | 1_code_analysis/3_events_all_outcomes_all_specs_F_run.R                                          | data_replicate/3_data_analysis_output/ | Yes   |
| all_bacon_decomps_individual_policies.rds   | Goodman-Bacon decomposition outputs             | 1_code_analysis/4_bacon_decomp_individual_policies.R                                             | data_replicate/3_data_analysis_output/ | Yes   |
| all_regs_tidy_binded.rds                    | Standard static two-way fixed effects output    | 1_code_analysis/1_regression_2wayFE.R                                                            | data_replicate/3_data_analysis_output/ | Yes   |
| regression_event_study_CS_reference.zip     | Callaway-Sant'Anna regression output (zipped)   | 1_code_analysis/2_regression_event_study_CSestimator.R                                           | data_replicate/3_data_analysis_output/ | Yes   |
| regression_event_study_vanilla_reference.zip| Dynamic event studies output (zipped)           | 1_code_analysis/3_events_all_outcomes_all_specs_F_run.R                                          | data_replicate/3_data_analysis_output/ | Yes   |
