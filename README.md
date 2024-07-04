# Replication code for the paper Researchers’ Degrees of Flexibility: Revisiting COVID-19 Policy Evaluations

The code is written in R, and uses an Rstudio R project which will take care of the relative paths of the different files. 
It is there recommended to run the script from Rstudio by loading the Rproject `degree_flexibility_covid.Rproj`.

The code is also hosted on https://github.com/MatthieuStigler/degree_flexibility_covid (but only the code), this is done to allow to make eventual updates to the code if mistakes are discovered. 

## Data dissemination

This dataset offers aggregated and transformed data derived from Google Mobility, Safegraph, Cuebiq, and PlaceIQ. To respect their data use agreements, we only provide anonymized county-level information.

## Description

This contains the folders:
  
- code replicate: code
  - the folder `auxiliary_scripts` contains auxiliary scripts that will be called with `source()`
- data_replicate: input data. In particular:
  - `merged_panel_did.rds` the main dataset, in R's `.rds` format.
  - `merged_panel_event_dummies.rds` the main dataset in event study format
- output_replicate: output fugures/tables
