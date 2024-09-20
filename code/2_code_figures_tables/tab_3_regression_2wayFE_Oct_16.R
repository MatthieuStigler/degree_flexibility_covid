#' ---
#' description: "2way basic FE model"
#' date: 2020-10-01
#' author: Matthieu
#' run_final: TRUE
#' ---

my_packages <- c("tidyverse","magrittr","lfe","stargazer", "broom", "future", "furrr", "Formula")
sapply(my_packages, require, character.only = TRUE)
sapply(my_packages, function(x) packageVersion(x) %>%  as.character)

if(!require(matPkg)) remotes::install_github("MatthieuStigler/matPkg", upgrade = "never")

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

################################
#'## Run regs
################################

## RUN ALL
if(interactive()) future::plan(sequential) else future::plan(future::multisession)
options(mc.cores=6)

out <- readRDS("data_replicate/formus_completely_home.rds") %>%
  mutate(reg = furrr::future_map(formula, ~felm(as.Formula(.x), data=data_reg) ))


################################
#'## Convert to tex 
################################
#
stargaze_reg_df_all_diffLogs(df_regs = out,
                             df_data=data_reg,
                             path_base = "output_replicate/tab_3_reg_2FE_",
                             float = FALSE)





