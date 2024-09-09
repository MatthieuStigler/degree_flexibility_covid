#' ---
#' description: "Table of covariates"
#' date: 2020-05-02
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
#'## Read data
################################

table_formulas <- read_rds("data_replicate/1_data_intermediate/vars_names_and_formulas/table_formulas_reg.rds")

################################
#'## 
################################

regs_final <- table_formulas %>% 
  filter(formu_final) %>% 
  pull(formu_num)

## extract covariates from regressions:
table_covars_extr <- table_formulas %>% 
  select(formu_vars, formu_num) %>% 
  tidyr::unnest(formu_vars) 

## aggregate
table_covars_aggr <- table_covars_extr%>% 
  mutate(covariate_name = forcats::fct_inorder(covariate_name)) %>% #preserve order
  group_by(covariate_name) %>% 
  summarise(in_reg = paste(formu_num, collapse = "-")) %>% 
  ungroup() 

table_covars_aggr


## For now we are happy with current order, not changing it! so: order = row_number()
## add info, cute names, order
table_covars <- table_covars_aggr%>% 
  mutate(covariate_name_clean= case_when(#covariate_name == "State_ED" ~"State Emergency",
                                         #covariate_name %in% c("State_SAH_BC", "State_SIP_BC") ~"State Shelter-in-Place",
                                         covariate_name == "StatePol_ED" ~"State Emergency",
                                         covariate_name == "StatePol_SIP" ~"State Shelter-in-Place",
                                         covariate_name == "StatePol_Resclo" ~"State Earliest Restriction or Closure",
                                         covariate_name == "County_ED" ~"County Emergency",
                                         #covariate_name == "County_SIP_BC" ~ "County Shelter-in-Place",
                                         covariate_name == "County_SIP" ~ "County Shelter-in-Place",
                                         covariate_name == "City_earliest_policy_pop_share" ~ "County Pop. Share Under City Policy",
                                         covariate_name == "Cum_cases" ~"Cumulative cases",
                                         covariate_name == "Cum_cases_sq" ~ "Cumulative cases sq",
                                         covariate_name == "Cases_dummy" ~"Cases > 0",
                                         covariate_name == "Deaths_dummy" ~"Deaths > 0",
                                         covariate_name == "tmean" ~ "Temperature (mean)",
                                         covariate_name == "prcp" ~ "Precipitation",
                                         covariate_name == "snow" ~ "Snow",
                                         TRUE ~ as.character(covariate_name)) %>% 
           str_replace("County_ED", "County Emergency") %>% 
           str_replace_all("_", " ") %>% 
           str_replace_all(":", " * "),
         order = row_number(),
         order = case_when(covariate_name =="City_earliest_policy_pop_share" ~5L,
                           covariate_name%in% "Cum_cases" ~max(order)+10L,
                           covariate_name%in% "Cum_cases_sq" ~max(order)+11L,
                           covariate_name%in% "Days_since_first_case" ~max(order)+12L,
                           covariate_name%in% "tmean" ~max(order)+20L,
                           covariate_name%in% "prcp" ~max(order)+21L,
                           covariate_name%in% "snow" ~max(order)+22L,
                           TRUE ~ order)) %>% 
  arrange(order) %>% 
  mutate(covar_tex = str_replace_all(covariate_name, "_", "\\\\_"),
         is_final= str_detect(in_reg, paste(regs_final, collapse = "|")),
         covar_type = case_when(covariate_name %in% c("tmean", "prcp", "snow") ~"Control",
                                str_detect(covariate_name, "(C|c)ases|dummy")~"Control",
                                str_detect(covariate_name, "T[0-9]$|landslide|lag|Tercile")~"Main_derived",
                                str_detect(covariate_name, "County|State|City")~"Main"))
                                         
table_covars

## check weather at end?
table_covars %>% 
  tail()

## check final coefs:
table_covars %>% 
  filter(is_final)

################################
#'## Export
################################

write_rds(table_covars, "data_replicate/1_data_intermediate/vars_names_and_formulas/table_covariates_reg.rds")
##READ AS: table_covars <- read_rds("data_replicate/1_data_intermediate/vars_names_and_formulas/table_covariates_reg.rds")
