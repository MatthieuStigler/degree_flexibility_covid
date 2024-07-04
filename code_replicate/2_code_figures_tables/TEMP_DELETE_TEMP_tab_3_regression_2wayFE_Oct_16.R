#' ---
#' description: "2way basic FE model"
#' date: 2020-10-01
#' author: Matthieu
#' run_final: TRUE
#' ---


#rm(list = ls()) 
my_packages <- c("tidyverse","magrittr","lfe","stargazer", "broom", "future", "furrr", "Formula")
sapply(my_packages, require, character.only = TRUE)
sapply(my_packages, function(x) packageVersion(x) %>%  as.character)

if(!require(matPkg)) remotes::install_github("MatthieuStigler/matPkg", upgrade = "never")

map<-purrr::map #For the nice iterations
select<-dplyr::select

source("code_replicate/auxiliary_scripts/regression_function_stargaze.R")
source("code_replicate/auxiliary_scripts/888_formula_functions_frm.R")
source("code_replicate/auxiliary_scripts/general_options.R")


################################
#'## Read data
################################

data_reg <- readRDS("data_replicate/merged_panel_did.rds")
df_cities <- read_rds("data_replicate/declarations_counties_states_panel.rds")

table_vars <- read_rds("data_replicate/table_responses_names.rds")
table_formulas <- read_rds("data_replicate/table_formulas_reg.rds")
table_covars <- read_rds("data_replicate/table_covariates_reg.rds")



################################
#'## Quick checsks
################################


## quick check new vars
#data_reg %>% 
#  mutate(at_least_one_death = as.numeric(!is.na(Days_since_first_death) &Days_since_first_death>=0),
#         at_least_one_case = as.numeric(!is.na(Days_since_first_case) &Days_since_first_case>=0)) %>% 
#  filter(at_least_one_death!=Deaths_dummy|at_least_one_case!=Cases_dummy) %>% 
#  mat_check_0row()



################################
#'## Prepare formulas
################################

## df of dependent variables
list_var_df <- table_vars %>% 
  # filter(is_main_new) %>%
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
  mat_check_0row()

## Select covariates for stargazing
table_covars_subset <- table_covars %>% 
  semi_join(all_covars_keep, by = "covariate_name")
  # filter(str_detect(in_reg, paste(formu_oct$formu_num, collapse = "|")))

## Check if all covariate there!?
all(table_covars_subset$covariate_name %in% colnames(data_reg))


## Formus: take both logs and levels
formus_log <- frm_df_extend_y(formus,
                              y_df = list_var_df, y_var = list_var_log) %>%
  filter(!str_detect(list_var,"^gm_")) #Added April 21, 2021

formus_noLog <- frm_df_extend_y(formus,
                              y_df = list_var_df, y_var = list_var) 

formula(formus_log$formu_char[[1]], rhs=0)
formula(formus_noLog$formu_char[[1]], rhs=0)

formus_all <- formus_log %>% 
  mutate(has_log=TRUE) %>% 
  rbind(formus_noLog %>% 
          mutate(has_log=FALSE))

formus_all

################################
#'## Run regs
################################

## RUN ALL
if(interactive()) future::plan(sequential) else future::plan(future::multisession)
options(mc.cores=6)

out <- formus_all %>% 
  # slice(n=5) %>% 
  #head(1) %>%
  mutate(reg = furrr::future_map(formula, ~felm(as.Formula(.x), data=data_reg) ))

out


################################
#'## Convert to tex 
################################


## TEST to tex one
stargaze_reg_df_one(df_regs=filter(out,list_var=="Completely_home_pct"& has_log),
                    df_data=data_reg,
                    path_base = "output_replicate/intermed_check/aTEST_reg_2FE_",
                    title_prefix = "FE2",
                    float.env="table")

# mat_table_to_pdf("Output/regression_2wayFE_vanilla/tables_october/aTEST_reg_2FE_Completely_home_log.tex",
#                  "Output/regression_2wayFE_vanilla/tables_october/aTEST_reg_2FE_Completely_home_log.pdf",
#                  is_path_x = TRUE, copy.mode=FALSE)

## Manual check coefs for test!
filter(out,list_var=="Completely_home_pct"& has_log) %>% 
  select(formu_num, reg) %>% 
  mutate(reg = map(reg, broom::tidy, conf.int=TRUE)) %>% 
  unnest(reg) %>% 
  select(formu_num, term, estimate) %>% 
  mutate(estimate = round(estimate, 3)) %>% 
  spread(formu_num, estimate)

## to tex all
stargaze_reg_df_all_diffLogs(df_regs = out,
                             df_data=data_reg,
                             path_base = "output_replicate/intermed_check/reg_2FE_")

# stargaze_reg_df_all_diffLogs(df_regs = out,
#                              df_data=data_reg,
#                              path_base = "Output/regression_2wayFE_vanilla/tables_october_sideways/reg_2FE_",
#                              float.env="sidewaystable")

### Tabular version
stargaze_reg_df_all_diffLogs(df_regs = out,
                             df_data=data_reg,
                             path_base = "output_replicate/intermed_check/reg_2FE_",
                             float = FALSE)

## cross outcome: formula 44
# stargaze_cross_formu_df_one(df_regs=out %>% filter(formu_num==44 & spec_log_main),
#                             path_base = "Output/regression_2wayFE_vanilla/tables_october/")

################################
#'## TeX to Pdf
################################

## List tex files to create pdf for
tex_tables_df <- mat_list_dir("output_replicate/intermed_check", pattern = "\\.tex") %>% 
  mutate(path_out = str_replace(full_path, "\\.tex$", ".pdf")) %>% 
  filter(str_detect(path_out, "side"))

## create pdf
tex_tables_df  %$%
  walk2(full_path, path_out, ~mat_table_to_pdf(.x, .y, is_path_x = TRUE, copy.mode=FALSE,
                                               plus = "\\usepackage{booktabs}\n\\usepackage{dcolumn}\n\\usepackage{underscore}\n\\usepackage{rotating}"))

# rotating

## sideways?
mat_table_to_pdf(tex_tables_df$full_path[[1]], 
                 tex_tables_df$path_out[[1]], is_path_x = TRUE, copy.mode=FALSE,
                 plus = "\\usepackage{booktabs}\n\\usepackage{dcolumn}\n\\usepackage{underscore}\n\\usepackage{rotating}",
                 quiet = TRUE)


#' ################################
#' #'## Get coefficient into df
#' ################################
#' 
#' ## get all coefs
#' coefs_out <- out %>% 
#'   # head(2) %>% 
#'   mutate(coefs = map(reg, tidy, conf.int = TRUE)) %>% 
#'   select(-reg, -formu_vars, -formula) %>% 
#'   unnest(coefs) %>% 
#'   mat_tidy_clean()
#' 
#' coefs_out
#' 
#' ## check NAs?
#' 
#' coefs_out %>% 
#'   filter(is.na(estimate)) %>% 
#'   count(term, formu_num) 



################################
#'## Export
################################



# write_rds(coefs_out, "Output/regression_2wayFE_vanilla/tables_october/coefs_all.rds")
## READ AS: reg_2wayFE_Oct_coefs <- read_rds("Output/regression_2wayFE_vanilla/tables_october/coefs_all.rds")
  







