#' ---
#' description: "Run callaway Santanna estimators, Replication"
#' date: 2021-04-21
#' author: Jo 
#' run_final: TRUE
#' ---


#---------------------------------------------------------------------------------------------------------------
#---------- This code runs alternative event studies using the Callaway Sant'Anna estimators
#---------------------------------------------------------------------------------------------------------------

my_packages <- c("tidyverse","magrittr")
#install.packages(my_packages, repos = "http://cran.rstudio.com")
lapply(my_packages, library, character.only = TRUE)
#install.packages("did")
library("did")
library("tictoc")
library("doParallel")
source("code/auxiliary_scripts/888_misc_functions.R")
map<-purrr::map #To avoid conflicts
select<-dplyr::select


options(scipen = 999) # Do not print scientific notation
set.seed(0101)
cores_number = 12
parallel::detectCores() 
getDoParWorkers() #check
registerDoParallel(cores=cores_number)
getDoParWorkers()

options(stringsAsFactors = FALSE) ## Do not load strings as factors


path_out = "data_replicate/3_data_analysis_output/regression_event_study_CS/"

## This needs to be manually adjusted! run twice
covariates = "_no_covariates" # first run
covariates = "_covariates_covid" # second run




#control_group = "nevertreated" # doesn't really change much
control_group = "notyettreated"

if(control_group == "nevertreated") {
  control_group_file = "_nevertreated"
} else {
  control_group_file = ""
}

#------------------------


#-----  Put outcome in long
#outcome_vars <- readRDS("Output/vars_names_and_formulas/table_responses_names.rds") 
outcome_vars <- readRDS("data_replicate/table_responses_names_long.rds") %>%
  filter(Source_short != "gm" | specification == "level")

did.df <- readRDS("data_replicate/2_data_final/merged_panel_did.rds") #Version standard

missing_weather <- did.df %>% filter(is.na(tmean))
unique(missing_weather$FIPS)

did.df_prepared <- did.df %>%
  mutate(Date_numeric = as.numeric(Date)) %>%
  mutate(ID = as.integer(as.factor(FIPS))) %>%
  mutate(County_SIP_numeric = as.integer(County_SIP_date)) %>%
  mutate(County_SIP_numeric = if_else(is.na(County_SIP_numeric),as.integer(0),County_SIP_numeric)) %>%
  mutate(County_ED_numeric = as.integer(County_ED_date)) %>%
  mutate(County_ED_numeric = if_else(is.na(County_ED_numeric),as.integer(0),County_ED_numeric)) %>%
  mutate(StatePol_SIP_numeric = as.integer(StatePol_SIP_date)) %>%
  mutate(StatePol_SIP_numeric = if_else(is.na(StatePol_SIP_numeric),as.integer(0),StatePol_SIP_numeric)) %>%
  filter(!FIPS %in% missing_weather$FIPS ) %>%  #remove counties with missing weather data
  mutate(StatePol_ED_dummy = if_else(StatePol_ED_date<=Date,1,0))

check<- did.df_prepared %>% select(FIPS, Date,StatePol_ED_dummy)



if(covariates == "_no_covariates") {
  
  covariates_variables_cleaned = as.formula(~1)
  
} else if(covariates=="_weather_covariates") {
  
  covariates_variables_cleaned <- c("prcp","snow","tmean") %>% paste(collapse =" + ") %>%
    paste0("~ ",.) %>%
    as.formula()
  
} else if(covariates=="_covariates_covid") {
  
  covariates_variables_cleaned <- c("prcp","snow","tmean"#,"City_earliest_policy_pop_share"
                                 ,"Cases_dummy","Deaths_dummy"
                                 #,"County_ED","County_SIP","StatePol_ED","StatePol_Resclo","StatePol_SIP"
  ) %>% paste(collapse =" + ") %>%
    paste0("~ ",.) %>%
    as.formula()
  
} else if(covariates == "_covariates_state_ED") {
  
  covariates_variables_cleaned <- c("prcp","snow","tmean","StatePol_ED_dummy") %>%
    paste(collapse =" + ") %>%
    paste0("~ ",.) %>%
    as.formula()
  
}

policy_to_test <- c("County_ED","County_SIP")
policy_to_test_CS <- paste0(policy_to_test,"_numeric")



## helper functions
did_extr_vals <- function(df){
  tibble(Overall_ATT = df$overall.att,
         Overall_SE = df$overall.se)
}

did_extr_ATT_all <- function(did_obj, na.rm= TRUE){
  tibble(type=c("simple", "dynamic", "group", "calendar")) %>% 
    mutate(out_aggr= map(type,~did_aggte_safe(did_obj, type = ., na.rm=na.rm) %>% did_extr_vals())) %>% 
    unnest(out_aggr)
}

did_aggte_safe <- function(x, type= "simple", na.rm=TRUE){
  out <- try(aggte(x, type = type, na.rm=na.rm), silent = TRUE)
  if(inherits(out, "try-error")) {
    return(list(overall.att=NA_real_, overall.se=NA_real_))
  }
  out
}

## this is to avoid message: 
#Caused by error in `rowSums()`:
# ! 'x' must be an array of at least two dimensions""
# caused with: 
#   -outcome_var_index=61
#   -individual_policy = "County_ED_numeric"

if(FALSE){
  library(did)
  data(mpdta)
  out <- att_gt(yname="lemp", tname="year",idname="countyreal",
                gname="first.treat",xformla=NULL, data=mpdta) 
  did_extr_vals(aggte(out, type = "dynamic"))
  did_extr_ATT_all(out)
}


#----- Run DID models
null_to_na <- function(x) if(is.null(x)) NA_real_ else drop(x)

#outcome_var_index=37 #rolling
#outcome_var_index=2 #rolling
#outcome_var_index=1 
#individual_policy = "County_ED_numeric"
# foreach(outcome_var_index = 1:nrow(outcome_vars)  ) %dopar% { 

## if interupted, rerun only those not there
if(FALSE){
  list_there <- mat_list_dir(paste0(path_out), pattern = "\\.rds$") %>% 
    mutate(folder = basename(dirname(full_path)),
           num = str_extract(filename, "(?<=_)[0-9]+(?=_)") %>% as.integer()) 
  nums_there <- sort(unique(list_there$num))
  ids_to_run_all <- 1:nrow(outcome_vars)
  ids_to_run <- setdiff(ids_to_run_all, nums_there)
  ids_to_run <- c(10L, 11L, 12L, 13L, 14L, 16L, 22L, 30L, 44L, 58L)
}

#-- Delete unused vars to speed up


## outcome_var_index = 1
## big loop
foreach(outcome_var_index = 1:nrow(outcome_vars), .final = \(x) NULL) %dopar% {

  # issue: outcome_var_index = 57
  # issue: individual_policy = policy_to_test_CS[3]
  #individual_policy = "County_ED_numeric"
  
    # for(individual_policy in policy_to_test_CS ) {
  foreach(individual_policy = policy_to_test_CS ) %do% {
    
    message("Going for ", outcome_var_index," (", outcome_vars$list_var[[outcome_var_index]],
            "), policy: ",individual_policy)
    tictoc::tic()
    
    
    did.df_prepared_no_NA <- did.df_prepared %>%
      #filter(!is.na(Completely_home_pct_rolling))
      filter( !is.na(get(outcome_vars$list_var[[outcome_var_index]])))
    
    ## run main function att_gt
    did_cs_sample <- att_gt(yname = outcome_vars$list_var[[outcome_var_index]],
                            gname = individual_policy,
                            idname = "ID",
                            tname = "Date_numeric",
                            xformla=covariates_variables_cleaned,
                            control_group = control_group,
                            data = did.df_prepared_no_NA #%>% filter(StatePol_SIP_numeric==0|StatePol_SIP_numeric==18355)
                            #%>% mutate(StatePol_ED_numeric = if_else(StatePol_ED_numeric==18337,as.integer(0),StatePol_ED_numeric))
                            ,est_method = "reg")
    #ggdid(did_cs_sample)
    
    tidy_agg_sample_all <- tibble(Time = did_cs_sample$t,
                                  ATT = did_cs_sample$att,
                                  Group = did_cs_sample$group,
                                  N = did_cs_sample$n,
                                  SE = did_cs_sample$se,
                                  W = null_to_na(did_cs_sample$W),
                                  Wpval = null_to_na(did_cs_sample$Wpval)) %>%
      #mutate(P_val_pre_test_parallel_trends = if_else(is.numeric(did_cs_sample$Wpval[[outcome_var_index]]), 
      #                                                did_cs_sample$Wpval[[outcome_var_index]],99) ) %>% #issue with value, often NULL
      mutate(Treatment_var = individual_policy) %>%
      mutate(Outcome_var = outcome_vars$list_var[[outcome_var_index]]) %>%
      mutate(specification = outcome_vars$specification[[outcome_var_index]]) %>%
      mutate(Control_group_CS = control_group)
      
    #message("Wald for pre-test number ",outcome_var_index,": ",did_cs_sample$Wpval[[outcome_var_index]])
    
    if(covariates != "_no_covariates") {
      tidy_agg_sample_all %<>%
        mutate(Included_covariates = as.character(covariates_variables_cleaned)[[2]])
    }
    
    saveRDS(tidy_agg_sample_all,
            paste0(path_out,"all_atts/did_cs_",individual_policy,"_",outcome_var_index,covariates,control_group_file,".rds"))

    # Now run event study: aggte()
    
    agg_did_sample <- aggte(did_cs_sample, type = "dynamic", na.rm = T) #Do the event study aggregation
    #ggdid(agg_did_sample)
    
    tidy_agg_sample <- tibble(Time = agg_did_sample$egt,
                              ATT = agg_did_sample$att.egt,
                              SE = agg_did_sample$se.egt) %>%
      mutate(Overall_ATT = agg_did_sample$overall.att) %>%
      mutate(Overall_SE = agg_did_sample$overall.se) %>%
      mutate(Treatment_var = individual_policy) %>%
      mutate(Outcome_var = outcome_vars$list_var[[outcome_var_index]]) %>%
      mutate(specification = outcome_vars$specification[[outcome_var_index]]) %>%
      mutate(Control_group_CS = control_group)
    
    if(covariates != "_no_covariates") {
      tidy_agg_sample %<>%
        mutate(Included_covariates = as.character(covariates_variables_cleaned)[[2]])
    }
    
    saveRDS(tidy_agg_sample,
            paste0(path_out,"aggregated_att/did_cs_",individual_policy,"_",outcome_var_index,covariates,control_group_file,".rds"))
    
    
    
    prj_toc(paste("\t", outcome_var_index, ' with policy: ',individual_policy," were saved!"))
    
    ### other aggrs for aggte()
    all_ATT <- did_extr_ATT_all(did_cs_sample) %>%
      mutate(Treatment_var = individual_policy,
             Outcome_var = outcome_vars$list_var[[outcome_var_index]],
             specification = outcome_vars$specification[[outcome_var_index]],
             Control_group_CS = control_group)
    saveRDS(all_ATT,
            paste0(path_out,"aggregated_att_allAggregs/did_cs_allAggregs_",individual_policy,"_",outcome_var_index,covariates,control_group_file,".rds"))
    # message(paste("\t", outcome_var_index, ' with policy: ',individual_policy," were saved!"))
    #tidy_agg_init <- bind_rows(tidy_agg_init,tidy_agg_sample) ##bind rows
  }
  
}



message("smooth exit!")


