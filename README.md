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

This contains the folders:
  
- code replicate: 3 folders
  - `auxiliary_scripts`: auxiliary scripts that only contain raw functions that will be used within other scrips by using `source()`
  - `1_code_analysis`: code to produce the various DiD and event-study coefficients
  - `2_code_figures_tables`: code to produce the figures and tables in the paper. 
- data_replicate: input data. In particular:
  - `merged_panel_did.rds` the main dataset, in R's `.rds` format.
  - `merged_panel_event_dummies.rds` the main dataset in event study format
- output_replicate: output fugures/tables



