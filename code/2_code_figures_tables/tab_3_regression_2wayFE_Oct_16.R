#' ---
#' description: "2way basic FE model"
#' date: 2020-10-01
#' author: Matthieu
#' run_final: TRUE
#' ---

my_packages <- c("tidyverse","magrittr","lfe","stargazer", "broom", "Formula")
sapply(my_packages, require, character.only = TRUE)

map<-purrr::map #For the nice iterations
select<-dplyr::select

source("code/auxiliary_scripts/regression_function_stargaze.R")
source("code/auxiliary_scripts/888_formula_functions_frm.R")
source("code/auxiliary_scripts/general_options.R")


################################
#'## Read data
################################

data_reg <- readRDS("data_replicate/2_data_final/merged_panel_did.rds")

table_vars <- read_rds("data_replicate/1_data_intermediate/vars_names_and_formulas/table_responses_names.rds")
table_covars <- read_rds("data_replicate/1_data_intermediate/vars_names_and_formulas/table_covariates_reg.rds")
# table_formulas <- read_rds("data_replicate/1_data_intermediate/vars_names_and_formulas/table_formulas_reg.rds")

formus_all <- read_rds("data_replicate/1_data_intermediate/vars_names_and_formulas/formus_all_keep.rds") # from code/1_code_analysis/1_

################################
#'## Prepare regs
################################

formus_completely_home <- formus_all %>%
  filter(str_detect(formu_char,"Completely_home_log ~"))

################################
#'## Run regs
################################

## RUN ALL
out <- formus_completely_home %>%
  mutate(reg = map(formula, ~felm(Formula::as.Formula(.x), data=data_reg) ))


################################
#'## Export to tex 
################################
#
stargaze_reg_df_all_diffLogs(df_regs = out,
                             df_data=data_reg,
                             path_base = "output_replicate/tab_3_reg_2FE_",
                             float = FALSE)





