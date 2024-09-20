#' ---
#' description: "Run Goodman-Bacon estimator using bacon_decomp_scripts.R for speed"
#' date: 2021-04-22
#' author: Jo
#' run_final: TRUE
#' ---

my_packages <- c("tictoc","doParallel","tidyverse","magrittr","Formula","data.table")
lapply(my_packages, library, character.only = TRUE)
library("bacondecomp")
source("code/auxiliary_scripts/bacon_decomp_scripts.R")

#----------
select<- dplyr::select

cores_number = 4
parallel::detectCores() 
registerDoParallel(cores=cores_number)
getDoParWorkers()


################################
#'## Read data
################################

df <- readRDS("data_replicate/2_data_final/merged_panel_did.rds")

################################
#'## Process
################################

remove_na_var <- function(var_to_clean, df) {
  
  cleaned_df <- df %>%
    mutate(County_got_na = ifelse(is.na({{var_to_clean}}),1,0)) %>%
    group_by(FIPS) %>%
    mutate(County_got_na = max(County_got_na)) %>%
    filter(County_got_na==0) %>% #Only keep counties without NA values
    ungroup()
  cleaned_df
}

check <- remove_na_var(Device_exposure_a_log, df)

#-----  Put outcome in long
outcome_vars <- readRDS("data_replicate/1_data_intermediate/vars_names_and_formulas/table_responses_names_long.rds") %>%
  filter(Source_short != "gm" & specification %in% c("level","log"))

df2 <- df %>%
  select(FIPS,Date,Cases_dummy,Deaths_dummy,
         StatePol_ED,StatePol_SIP,StatePol_Resclo,County_ED,County_SIP, any_of(outcome_vars$list_var)) %>% 
  mutate(Date_num = as.numeric(Date))

names(df2)
rm(df)
#----- Prepare big data frame here


all_dep <- outcome_vars$list_var[!str_detect(outcome_vars$list_var,"gm_")] #necessary, too many missing values
all_indep <- c("StatePol_ED","StatePol_SIP","StatePol_Resclo","County_ED","County_SIP")

all_reg <- expand_grid(all_dep,all_indep) 


# Prepare dataset
all_reg %<>%
  mutate(regression = map2_chr(all_dep,all_indep, ~paste0(.x," ~ ",.y))) %>%
  mutate(regression = map(regression, ~Formula::as.Formula(.x))) 



all_decomps <- list(rep(NA,nrow(all_reg)))

#---- Run bacondecomp, clean parallel (no future bullshit)
tic()
all_decomps <- foreach(reg=1:nrow(all_reg)) %dopar% {
  
  #Clean NA values for subset
  all_reg_i <- all_reg[reg,] %>%
    mutate(df_cleaned = map(all_dep, ~remove_na_var(eval(as.symbol(.x),df2), df2 ))) 
  
  message("parallelizing regression ",reg)
  
  out <- bacon_decomp_custom(formula = all_reg_i[[1,3]][[1]] , 
                             data = all_reg_i[[1,4]][[1]], 
                             id_var = "FIPS", time_var = "Date_num",
                             quietly = TRUE) 
  
  out <- out %>%
    mutate(Dep_var = all_reg_i[[1,1]][[1]]) %>%
    mutate(Indep_var = all_reg_i[[1,2]][[1]]) 
  
  out
  
}
toc()

saveRDS(all_decomps,"data_replicate/3_data_analysis_output/all_bacon_decomps_individual_policies.rds")
message("smooth exit!")
