#' ---
#' title: "Rerun all files"
#' author: "Matthieu"
#' date: 2024-09-08
#' ---

library(tidyverse)
library(collateral)

################################
#'## List scripts
################################

files_df <- tibble(full_path=list.files("code", pattern= "\\.R$", recursive = TRUE, full.names=TRUE)) %>% 
  mutate(file = basename(full_path))

## need to exclude some:
files_keep <- files_df %>% 
  ## exclude safegraph files
  filter(!str_detect(file, "0a_aggregate_safegraph_SD_county|0b_clean_safegraph_county_level_SD")) %>% 
  ## exclude cuebiq files
  filter(!str_detect(file, "cuebiq")) %>% 
  ## exclude files calling above
  filter(!str_detect(file, "1_2_merge_main_datasets")) %>% 
  ## exclude own file
  filter(file!="master_run_all.R")


## order files now
files_keep_order <- files_keep %>% 
  mutate(folder = basename(dirname(full_path)),
         order = case_when(file=="0_clean_map_us_counties.R"~1,
                           TRUE ~NA)) %>% 
  arrange(folder, order)

files_keep_order


### Now run
out <- map_safely(files_keep$full_path[1:2], ~source(.))
out

has_errors(out)
out
################################
#'## 
################################

################################
#'## 
################################

################################
#'## 
################################

#write_rds(..., "data_intermediary/")

## save plots  
prj_ggsave(..., )