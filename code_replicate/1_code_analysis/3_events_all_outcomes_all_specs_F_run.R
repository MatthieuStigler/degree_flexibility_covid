#' ---
#' description: "Runs standard event studies, all outcomes, all specs and F tests"
#' runMat: FALSE
#' run_final: TRUE
#' date: 2021-04-21
#' ---



my_packages <- c("tidyverse","magrittr","lfe","stargazer", "broom", "doParallel", "furrr","data.table","car")
#install.packages(my_packages, repos = "http://cran.rstudio.com")
sapply(my_packages, require, character.only = TRUE)

if(!require(matPkg)) remotes::install_github("MatthieuStigler/matPkg", upgrade = "never")

map<-purrr::map #For the nice iterations
select<-dplyr::select

source("code_replicate/auxiliary_scripts/888_event_code.R")
source("code_replicate/auxiliary_scripts/general_options.R")




cores_number = 8
parallel::detectCores() 
getDoParWorkers() #check
registerDoParallel(cores=cores_number)
getDoParWorkers()

run_evt_reg = T

path_out = "data_replicate/regression_event_study_vanilla/all_outcomes_all_specs_with_Ftests/"

df <- readRDS("data_replicate/2_data_final/merged_panel_event_dummies.rds") 

glimpse(df)

#------------------------------------------------------------------------------------------------------------

#-----  Get outcomes
outcome_long <- readRDS("data_replicate/table_responses_names_long.rds") %>%
  filter(Source!="Google" | specification == "level") #Keeping google in level


vars_transforms = bind_rows(outcome_long %>% mutate(Weights = F),
                            outcome_long %>% 
                              mutate(Weights = T) %>% 
                              mutate(list_names = paste0(list_names, ", weighted"))
)

glimpse(vars_transforms)


#---------- All simultaneous policies
vars_treatment <- names(df)[str_detect(names(df), "_((b)[0-9]{1,2}$)")] 
vars_treatment_pre <- vars_treatment[!str_detect(vars_treatment,"_b1$")] #remove the baseline dummy
tests_pre_all <- as.character(paste0(vars_treatment_pre,"=0"))
rm(vars_treatment,vars_treatment_pre)


set.seed(101)

if(run_evt_reg) { #takes a while

  
  looping <- 10 #set bigger number if want more smaller objects 
  
  vars_transforms %<>%
    mutate(Sample_loop = sample(seq(1:10), n(), replace = T))
  
  table(vars_transforms$Sample_loop)
  
  
  foreach(j=1:looping) %dopar% {
  
  #------------- Run regressions with multiple policies
  Big_df <- vars_transforms %>%
    filter(Sample_loop == j) %>% # run reg
    #head(1) %>% #for prototyping 
    mutate(reg = pmap(.l = list(list_var,Weights), .f= ~evt_reg(df, baseline = -1, var_dependent = ..1, 
                                                                vars_treatment =c("County_ED", "County_SIP", "StatePol_ED", "StatePol_SIP","StatePol_Resclo"),
                                                                vars_controls = c("City_earliest_policy_pop_share","prcp","snow","tmean","Cases_dummy","Deaths_dummy"), 
                                                                var_clustering = "FIPS", pop_weights = ..2)
    )) %>% # run F test
    mutate(Ftest = pmap(.l = list(reg), .f =  ~linearHypothesis(..1, tests_pre_all, test = "F") %>% get("Pr(>F)",.) %>% `[[`(2) )#Retrieve p-value here
    ) %>% # extract coefs
    mutate(tidy = map(reg, 
                      ~evt_tidy(reg_output = .x, baseline = -1, number_lags = 21, number_leads = 21,
                                vars_treatment =c("County_ED", "County_SIP", "StatePol_ED", "StatePol_SIP","StatePol_Resclo"))    )
    )  %>%
    mutate(Nobs = map_dbl(reg, ~.x %>% `[[`("N") )) %>%
    mutate(tidy_cleaned = pmap(list(tidy,list_names,Source,Ftest,specification, Weights,list_var,Nobs), ~ ..1 %>% 
                                 mutate(Outcome_var = ..2) %>%
                                 mutate(Source = ..3) %>%
                                 mutate(Ftest = ..4) %>%
                                 mutate(Policies_in_regression = "Simultaneous policies") %>%
                                 mutate(specification = ..5) %>%
                                 mutate(Weights = ..6) %>%
                                 mutate(dep_var = ..7) %>%
                                 mutate(Number_obs = ..8) 
                               )
           ) 
    
  
  simultaneous_tidy <- bind_rows(Big_df$tidy_cleaned)
  saveRDS(simultaneous_tidy,paste0(path_out,"saved_objects/tidy_simultaneous_",j,"_regressions_20_outcomes_4_transforms_weighted.rds") )
  
  }
  
  message("first foreach loop done")
  
  #------------- Regressions with policies one at a time
  
  looping <- 10 #set bigger number if want more smaller objects

  Big_df_uniques <- vars_transforms %>%
    expand_grid(tibble(unique_var = c("County_ED", "County_SIP", "StatePol_ED", "StatePol_SIP","StatePol_Resclo")) ) %>%
    mutate(Sample_loop = sample(seq(1:10), n(), replace = T))
  
  table(Big_df_uniques$Sample_loop)

  foreach(i=1:looping) %dopar% {
    
    Big_df_uniques_slice <- Big_df_uniques %>%
      filter(Sample_loop  == i) %>%
      #head(1) %>% 
      mutate(pre_trends_vars = map(unique_var, ~tests_pre_all[str_detect(tests_pre_all,.x)])) %>% 
      mutate(reg = pmap(.l = list(list_var,Weights,unique_var), .f= ~evt_reg(df, baseline = -1, var_dependent = ..1, 
                                                                             vars_treatment = ..3,
                                                                             vars_controls = c("City_earliest_policy_pop_share","prcp","snow","tmean","Cases_dummy","Deaths_dummy"), 
                                                                             var_clustering = "FIPS", pop_weights = ..2))
             ) %>%
      mutate(Ftest = pmap(.l = list(reg,pre_trends_vars), .f =  ~linearHypothesis(..1, ..2, test = "F") %>% get("Pr(>F)",.) %>% `[[`(2) ) #Retrieve p-value here
             ) %>%
      mutate(tidy = map(reg, 
                        ~evt_tidy(reg_output = .x, baseline = -1, number_lags = 21, number_leads = 21,
                                  vars_treatment =c("County_ED", "County_SIP", "StatePol_ED", "StatePol_SIP","StatePol_Resclo")) )
             )  %>%
      mutate(Nobs = map_dbl(reg, ~.x %>% `[[`("N") )) %>%
      mutate(tidy_cleaned = pmap(list(tidy,list_names,Source,unique_var,Ftest,specification,Weights,list_var,Nobs), ~ ..1 %>% 
                                   mutate(Outcome_var = ..2) %>%
                                   mutate(Source = ..3) %>%
                                   mutate(Policies_in_regression = ..4) %>%
                                   mutate(Ftest = ..5) %>%
                                   mutate(specification = ..6) %>% 
                                   mutate(Weights = ..7) %>% 
                                   mutate(dep_var = ..8) %>%
                                   mutate(Number_obs = ..9)
                                 )
      )
    

    unique_policies_tidy <- bind_rows(Big_df_uniques_slice$tidy_cleaned)
    saveRDS(unique_policies_tidy,paste0(path_out,"saved_objects/tidy_unique_policies_slice_",i,"_regressions_14_outcomes_4_transforms_weighted.rds") )
    
  }
  
  message("second foreach loop done")

  #---- Putting all the slices together    
  all_objects <- list.files(paste0(path_out,"saved_objects/"), full.names = T)
  
  tidy_all <- readRDS(all_objects[[1]])
  tidy_all %>% count(Outcome_var) %>% count(n)
  
  
  for (i in 2:length(all_objects)) {
    tidy_i <- readRDS(all_objects[[i]])
    tidy_all <- bind_rows(tidy_all,tidy_i)
  }
  
  tidy_all %>% count(Policies_in_regression) 
  table(tidy_all$Number_obs)
  table(tidy_all$Treatment_var)
  table(tidy_all$Policies_in_regression)
  
  glimpse(tidy_all)
  
  tidy_all_cleaned <- tidy_all %>%
    mutate(Treatment_var = str_replace(Treatment_var,"_"," ")) %>% #move elsewhere?
    mutate(Treatment_var = str_replace(Treatment_var,"Resclo","ERC")) %>% #move elsewhere?
    mutate(Treatment_var = str_replace(Treatment_var,"SIP","SIPO")) %>% #move elsewhere?
    mutate(Treatment_var = str_remove(Treatment_var,"Pol"))  %>%
    
    mutate(Policies_in_regression = str_replace(Policies_in_regression,"_"," ")) %>% #move elsewhere?
    mutate(Policies_in_regression = str_replace(Policies_in_regression,"Resclo","ERC")) %>% #move elsewhere?
    mutate(Policies_in_regression = str_replace(Policies_in_regression,"SIP","SIPO")) %>% #move elsewhere?
    mutate(Policies_in_regression = str_remove(Policies_in_regression,"Pol"))  %>%
    
    mutate(Policies_in_regression_f = factor(Policies_in_regression, 
                                             levels = c("County ED", "County SIPO", "State ED", 
                                                        "State SIPO","State ERC", "Simultaneous policies")))
  
  table(tidy_all_cleaned$Policies_in_regression_f)
  table(tidy_all_cleaned$Treatment_var)
  saveRDS(tidy_all_cleaned,paste0(path_out,"all_regs_tidy_binded.rds"))
  
}