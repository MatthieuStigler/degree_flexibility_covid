#' ---
#' description: "2way basic FE model"
#' date: 2020-10-01
#' author: Matthieu
#' run_final: TRUE
#' ---


my_packages <- c("tidyverse","magrittr","lfe","stargazer", "broom", "future", "furrr", "Formula")
sapply(my_packages, require, character.only = TRUE)

if(!require(matPkg)) remotes::install_github("MatthieuStigler/matPkg", upgrade = "never")

map<-purrr::map 
select<-dplyr::select

source("code/auxiliary_scripts/regression_function_stargaze.R")
source("code/auxiliary_scripts/888_formula_functions_frm.R")
source("code/auxiliary_scripts/general_options.R")
source("code/auxiliary_scripts/888_misc_functions.R")

################################
#'## Read data
################################

data_reg <- readRDS("data_replicate/2_data_final/merged_panel_did.rds")
df_cities <- read_rds("data_replicate/1_data_intermediate/declarations_counties_states_panel.rds")

table_vars <- read_rds("data_replicate/1_data_intermediate/vars_names_and_formulas/table_responses_names.rds")
table_formulas <- read_rds("data_replicate/1_data_intermediate/vars_names_and_formulas/table_formulas_reg.rds")
table_covars <- read_rds("data_replicate/1_data_intermediate/vars_names_and_formulas/table_covariates_reg.rds")



################################
#'## Prepare formulas
################################

## df of dependent variables
list_var_df <- table_vars %>% 
  select(list_var, list_var_log, list_names, list_names_log, pref_spec) 


## Select formulas: aug 28
formus <- table_formulas  %>% 
  filter(formu_oct) 
  
## formula vars
all_covars_keep <- formus %>% 
  select(formu_vars) %>% 
  unnest(formu_vars) %>% 
  distinct()

## all there?
all_covars_keep %>% 
  anti_join(table_covars, by = "covariate_name") %>% 
  prj_check_0row()

## Select covariates for stargazing
table_covars_subset <- table_covars %>% 
  semi_join(all_covars_keep, by = "covariate_name")

## Check if all covariate there!?
all(table_covars_subset$covariate_name %in% colnames(data_reg))


## Formus: take both logs and levels
formus_log <- frm_df_extend_y(formus,
                              y_df = list_var_df, y_var = list_var_log) %>%
  filter(!str_detect(list_var,"^gm_")) 

formus_noLog <- frm_df_extend_y(formus,
                              y_df = list_var_df, y_var = list_var) 

formula(formus_log$formu_char[[1]], rhs=0)
formula(formus_noLog$formu_char[[1]], rhs=0)

formus_all <- formus_log %>% 
  mutate(has_log=TRUE) %>% 
  rbind(formus_noLog %>% 
          mutate(has_log=FALSE))

formus_all

## export
write_rds(formus_all, "data_replicate/1_data_intermediate/vars_names_and_formulas/formus_all_keep.rds")
## READ AS: formus_all <- read_rds("data_replicate/1_data_intermediate/vars_names_and_formulas/formus_all_keep.rds") # from code/1_code_analysis/1_

################################
#'## Run regs
################################

## RUN ALL
if(interactive()) future::plan(sequential) else future::plan(future::multisession)
options(mc.cores=6)

out <- formus_all %>% 
  mutate(reg = furrr::future_map(formula, ~felm(as.Formula(.x), data=data_reg) ))

out


################################
#'## Get coefficient into df
################################

## get all coefs
coefs_out <- out %>% 
  mutate(coefs = map(reg, tidy, conf.int = TRUE)) %>% 
  select(-reg, -formu_vars, -formula) %>% 
  unnest(coefs) %>% 
  mat_tidy_clean()

coefs_out

## check NAs

coefs_out %>% 
  filter(is.na(estimate)) %>% 
  count(term, formu_num) 



################################
#'## Export
################################

write_rds(coefs_out, "data_replicate/3_data_analysis_output/2wayFE_coefs_all.rds")
