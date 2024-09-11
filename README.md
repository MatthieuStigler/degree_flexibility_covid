# Replication code for the paper Researchers’ Degrees of Flexibility: Revisiting COVID-19 Policy Evaluations

The code is written in R, and uses an Rstudio R project which will take care of the relative paths of the different files. 
It is there recommended to run the script from Rstudio by loading the Rproject `degree_flexibility_covid.Rproj`.

The code is also hosted on https://github.com/MatthieuStigler/degree_flexibility_covid (but only the code), this is done to allow to make eventual updates to the code if mistakes are discovered. 

## Data dissemination

This dataset offers aggregated and transformed data derived from Google Mobility, Safegraph, Cuebiq, and PlaceIQ. To respect their data use agreements, we only provide anonymized county-level information.

## How to run this in R: packages

This project was configured with `renv`, although it is deactivated by default within this project. To activate it, and hence get the same package versions as used here, run: 

```
renv::activate()
renv::restore()
```
For more information, see https://rstudio.github.io/renv/articles/renv.html

The project also uses two non-CRAN packages:

- library("matPkg") version 0.2.50, use: `remotes::install_github("MatthieuStigler/matPkg", upgrade = "never")`
- library("multidiff") version 0.3.0, use: `remotes::install_github("MatthieuStigler/multiDiff", upgrade = "never")`

## Description



The data is structured as following:

├── code
│   ├── 0_code_clean
│   ├── 1_code_analysis
│   ├── 2_code_figures_tables
│   └── auxiliary_scripts
├── data_replicate
│   ├── 0_data_raw
│   ├── 1_data_intermediate
│   ├── 2_data_final
│   └── 3_data_analysis_output
├── output_replicate
└── renv

  
- `code`: 5 folders
  - a `master_run_all.R` file that runds all the files
  - `0_code_clean`: initial code to clean/create the various datasets (0_* scripts) and to merge the datasets (1_* scripts)
  - `1_code_analysis`: code to produce the various DiD and event-study coefficients
  - `2_code_figures_tables`: code to produce the figures and tables in the paper. 
  - `auxiliary_scripts`: auxiliary scripts that only contain raw functions that will be used within other scrips by using `source()`
  - `auxiliary_packages`: auxiliary packages that are not on CRAN but on github. While we recommend installing these packages using `renv`, these are kept here in case for the sake of reproducibility. 
- `data_replicate`: input data. In particular:
  - `0_data_raw`: raw data files
  - `1_data_intermediate`: datasets processed within this project
  - `2_data_final`: the final datasets used in the analysis. Most importantly:
    - `merged_panel_did.rds` the main dataset, in R's native `.rds` format.
    - `merged_panel_event_dummies.rds` the main dataset in event-study form
  - `3_data_analysis_output`: output from the DiD, CS etc analysis. As these analyses can be time- and memoey-consuming, it is preferable to store themn before plotting. 
- `output_replicate`: output figures/tables
- `renv`: this is a folder used by `renv`



### Specific files

Some files need some specific action:

- `0_clean_map_us_counties.R` (in `code/0_code_clean`): this downloads census data. You will need to request an API key at http://api.census.gov/data/key_signup.html and use it with `tidycensus::census_api_key()`
- `0a_clean_safegraph_aggregate_SD_county.R` and `0b_clean_safegraph_county_level_SD.R` (in `code/0_code_clean`): JOJO
- `0_clean_cuebiq.R` (in `code/0_code_clean`): JOJO
