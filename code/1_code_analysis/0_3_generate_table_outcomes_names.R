#' ---
#' description: "Generate a table of outcome variables"
#' date: 2021-04-21
#' author: Matthieu
#' run_final: TRUE
#' ---

my_packages <- c("tidyverse", "sf")
sapply(my_packages, require, character.only = TRUE)

map<-purrr::map #For the nice iterations
select<-dplyr::select

source("code/auxiliary_scripts/general_options.R")
#-------------------------------------------- ----------------------------------------------------------------

################################
#'## Create
################################

vars <- tribble(~list_var, ~list_names, ~ Source, ~Category, ~Sign_scaling,
                "Median_home_dwell_time",        "Median Home Dwell Time",      "Safegraph", "Home", "Distancing",
                "Median_not_home_dwell_time",    "Median Not Home Dwell Time",  "Safegraph", "Unsure", "Mobility",
                "Completely_home_pct",           "Completely Home (%)",             "Safegraph", "Home", "Distancing",
                "Median_home_perc",              "Median Home Share (%)",           "Safegraph", "Home", "Distancing",
                "Part_time_work_pct",            "Part Time Work (%)",              "Safegraph", "Work", "Mobility",
                "Full_time_work_pct",            "Full Time Work (%)",              "Safegraph", "Work", "Mobility",
                "Away_min_three_hours_pct",      "Away at least 3 hours (%)",       "Safegraph", "Work", "Mobility",
                
                "Median_distance_traveled",      "Median Distance",             "Safegraph", "Transit", "Mobility",
                "Average_time_away",              "Average Time Not Home",         "Safegraph",    "Away", "Mobility",
                # "Device_exposure_a_log",          "Device Exposure (log)",             "PlaceIQ",   "Unsure",
                "Device_exposure_a",             "Device Exposure",             "PlaceIQ",   "Unsure", "Mobility",
                "gm_retail_and_recreation",      "Retail & Recreation",         "Google",    "Shopping", "Mobility",
                "gm_grocery_and_pharmacy",       "Grocery & pharmacy",          "Google",    "Shopping", "Mobility",
                "gm_parks",                      "Parks",                       "Google",    "Leisure", "Mobility",
                "gm_transit_stations",           "Transit stations",            "Google",    "Transit", "Mobility",
                "gm_workplaces",                 "Workplaces",                  "Google",    "Work", "Mobility",
                "gm_residential",                "Residential",                 "Google",    "Home", "Distancing",
                "cq_Mobility_index",             "Mobility Index",              "Cuebiq",    "Unsure", "Mobility",
                "cq_SIP",                        "Staying Around Home (%)",         "Cuebiq",    "Home", "Distancing",
                "cq_Less_one_mile",              "Staying Around Neighborhood", "Cuebiq",    "Unsure", "Distancing",
                "cq_More_ten_mile",              "Traveling more than 10 miles",         "Cuebiq",    "Unsure", "Mobility")

vars 

## 
table_vars <- vars %>% 
  mutate(list_var_log = if_else(Source!="Google", paste0(list_var, "_log"), list_var) %>% 
           str_replace("pct_log", "log") %>% 
           str_replace("perc_log", "share_log"),
         list_names_log = if_else(Source!="Google", paste (list_names, "(log)"), list_var) %>% 
           str_remove("\\(%\\)") %>% 
           str_squish(),
         list_var_diff = if_else(Source!="Google", paste0(list_var, "_diff"), list_var),
         list_names_diff = if_else(Source!="Google", paste (list_names, "(diff.)"), list_var) %>% 
           str_remove("\\(%\\)") %>% 
           str_squish(),
         list_var_rolling = if_else(Source!="Google", paste0(list_var, "_rolling"), list_var),
         list_names_rolling = if_else(Source!="Google", paste (list_names, "(7-day m.a.)"), list_var) %>% 
           str_remove("\\(%\\)") %>% 
           str_squish(),
         Name_source = paste0(list_names, " (", Source, ")"),
         Categ_name = paste0(Category, ": ", list_names),
         # is_main = list_var %in% c("Completely_home_pct","cq_SIP", "Full_time_work_pct"),
         is_main = Source!="Google" & !  list_var %in% c("cq_Less_ten_mile","Median_home_dwell_time"), 
         is_main_new = list_var %in% c("Completely_home_pct","Device_exposure_a", "Full_time_work_pct", "cq_SIP"),
         Source_short = case_when(Source == "Safegraph" ~ "sg",
                                  Source == "Google" ~ "gm",
                                  Source == "Cuebiq" ~ "cq",
                                  Source == "PlaceIQ" ~ "pq"),
         pref_spec = case_when(str_detect(list_names, "\\(%") ~ "level",
                               str_detect(list_var, "Device_exposure")~ "log",
                               TRUE~"level")) %>% 
  mutate(keep=TRUE) %>%
  mutate(Source_name_signed = case_when(Sign_scaling == "Distancing" ~ paste0(str_to_upper(Source_short),": ",list_names), #Added April 21
                                        Sign_scaling == "Mobility" ~ paste0(str_to_upper(Source_short),": ",list_names,"*"),
                                        TRUE~"ERROR"
                                        )) %>%
  mutate(Source_name_signed = str_remove(Source_name_signed," \\(\\%\\)"))
         

table_vars

## checvk all info there
table_vars %>% 
  filter_all(any_vars(is.na(.))) %>% 
  mat_check_0row()


################################
#'## Long format
################################
names(table_vars)

standard_vars <- select(table_vars, -contains("log"),-contains( "_diff"),-contains("_rolling"))%>% 
  mutate(specification = "level") %>%
  mutate(list_var_orig=list_var,
         list_names_orig=list_names) 

vars_logs <- select(table_vars, -contains( "_diff"),-contains("_rolling")) %>%
  rename(list_var_orig=list_var,
         list_names_orig=list_names) %>%
  rename(list_var = list_var_log,
         list_names = list_names_log) %>% 
  mutate(specification = "log")

vars_rolling <- select(table_vars,-contains( "_diff"),-contains("_log"))  %>%
  rename(list_var_orig=list_var,
         list_names_orig=list_names) %>%
  rename(list_var = list_var_rolling,
         list_names = list_names_rolling)%>% 
  mutate(specification = "rolling")

vars_diff <- select(table_vars, -contains( "_log"),-contains("_rolling")) %>%
  rename(list_var_orig=list_var,
         list_names_orig=list_names) %>%
  rename(list_var = list_var_diff,
         list_names = list_names_diff) %>% 
  mutate(specification = "diff")

table_vars_long <- standard_vars %>%
  bind_rows(vars_logs) %>%
  bind_rows(vars_rolling) %>%
  bind_rows(vars_diff) %>%
  mutate(is_pref_spec = pref_spec == specification) 
  


## inspect res
table_vars_long %>% 
  filter(list_var_orig=="Median_home_dwell_time") %>% 
  select(1:5)


################################
#'## Export
################################

write_rds(table_vars, "data_replicate/1_data_intermediate/vars_names_and_formulas/table_responses_names.rds")
## READ AS: table_vars <- read_rds("data_replicate/1_data_intermediate/vars_names_and_formulas/table_responses_names.rds")

write_rds(table_vars_long, "data_replicate/1_data_intermediate/vars_names_and_formulas/table_responses_names_long.rds")
## READ AS: table_vars_long <- read_rds("data_replicate/1_data_intermediate/vars_names_and_formulas/table_responses_names_long.rds")



