library("tidyverse")
library("matPkg")
library("lfe")
library("stats")
library("broom")



################################
#'## Main function: evt_data_build
################################

evt_data_build <- function(df, vars_treatment,
                           unit_var=FIPS, time_var=Date,
                           number_lags, number_leads){
  df %>%
    arrange({{unit_var}},  {{time_var}}) %>% 
    evt_add_timing(vars_treatment=vars_treatment,
                   unit_var= {{unit_var}},
                   time_var={{time_var}}) %>% 
    evt_add_lags(vars_treatment=vars_treatment,
                 unit_var= {{unit_var}},
                 time_var={{time_var}},
                 number_lags=number_lags,
                 number_leads=number_leads) %>% 
    evt_bin_lags(unit_var= {{unit_var}},
                 time_var={{time_var}})
  
  
}

################################
#'## Utilities
################################


## get initial raw data, with one single dummy for event (0 after event)
## this will add:
## -x_timing: 0/1/0 for exact event
## -x_days: number of days before after: -1/0/1
evt_add_timing <- function(df, vars_treatment,
                           unit_var=FIPS, time_var=Date){
  df %>% 
    select({{unit_var}}, {{time_var}}, one_of(vars_treatment)) %>% 
    group_by({{unit_var}}) %>% 
    mutate(row_number = row_number()) %>% 
    mutate(across(vars_treatment, 
                  list(timing =~ifelse(row_number==which(.==1)[1], 1, 0) %>% ## 0/1 for specifc event
                         replace_na(0),
                         days = ~as.integer(Date -Date[which(.==1)[1]]) %>% ## cumulative days before/after
                         replace_na(0)
                       ),
                  .names="{.col}_{.fn}")) %>% 
    select(-ends_with("_days"), ends_with("_days")) %>% ## aesthetic
    ungroup() %>% 
    mutate_if(is.numeric, as.integer)
}


evt_add_timing_from_date <- function(df, var_date,
                                     unit_var=FIPS ,
                                     start_date=NA,
                                     end_date=NA){
  
  has_many_vars <- length(var_date)>1
  
  ## make long over variable
    df_c <- df %>% 
      select(all_of(var_date), {{unit_var}}) %>% 
      gather(variable, date, -{{unit_var}}) %>% 
      filter(!is.na(date))
    group_vars <- vars({{unit_var}}, variable)

  ## extract all dates
  range_dates <- range(df_c$date, na.rm=TRUE)
  if(inherits(range_dates, 'Date')){
    if(is.na(start_date)) start_date <- as.Date(NA)
    if(is.na(end_date)) end_date <- as.Date(NA)
  }
  
  # set dates
  min_date <- min(c(start_date, range_dates[1]), na.rm=TRUE)
  max_date <- max(c(end_date, range_dates[2]), na.rm=TRUE)
  if(is.null(max_date)) max_date <- range_dates[2]
    
  all_dates <- seq(min_date, max_date, by = 1)

  ## complete data at unit var level (slooow)
  df_comp <- df_c %>% 
    mutate(timing=1) %>% 
    group_by_at(group_vars) %>% 
    complete(date=all_dates,
             fill=list(timing=0)) %>% 
    ungroup() 
  
  ## now wide
  out <- df_comp %>% 
    pivot_wider(names_from = "variable",
                values_from="timing",
                names_glue = "{variable}_timing",
                values_fill=0)
  
  ## make shorter/longer
  if(!is.na(end_date)){
    if(end_date<range_dates[2]) out <- out %>% 
        filter(date<=end_date)
  }
  out
}

################################
#'## Step 2: add lags
################################

## now add lags
evt_add_lags <- function(df_timing, vars_treatment,
                           unit_var=FIPS, time_var=Date,
                         number_lags,#=20,
                         number_leads){
  if(number_lags<0) stop("number_lags should be positive!")
  
  unit_var_char <- rlang::as_name(ensym(unit_var))
  time_var_char <- rlang::as_name(ensym(time_var))
  df_timing %>% 
    multiDiff::lag_group(group_var = unit_var_char, 
                         value_var = paste(vars_treatment, "timing", sep = "_"),
                         time_var =  {{time_var_char}}, 
                         lagamount = (-1*number_lags):number_leads, 
                         default = 0L) %>% 
    rename_all(~str_replace(., "(?<=timing_)lag", "f") %>% ## confusing but need to invert!
                 str_replace(., "(?<=timing_)lead", "b") %>% 
                 str_replace("_timing_(b|f)", "_\\1"))
  
}

## alternative function, using collapse package supposed to be super fast
evt_add_lags_alter <- function(df_timing, vars_treatment,
                               unit_var=FIPS, time_var=Date,
                               number_lags, number_leads){
  if(number_lags<0) stop("number_lags should be positive!")
  
  unit_var_char <- rlang::as_name(ensym(unit_var))
  time_var_char <- rlang::as_name(ensym(time_var))
  
  lags_leads <- (-1*number_lags):number_leads
  collapse::flag(df_timing %>% 
                   select(all_of(paste(vars_treatment, "timing", sep = "_"))), 
                 n=lags_leads,
                 g = pull(df_timing, {{unit_var}}),
                 t = pull(df_timing, {{time_var}}))
  df_timing %>% 
    multiDiff::lag_group(group_var = unit_var_char, 
                         value_var = paste(vars_treatment, "timing", sep = "_"),
                         time_var =  {{time_var_char}}, 
                         lagamount = , 
                         default = 0L) %>% 
    rename_all(~str_replace(., "(?<=timing_)lag", "f") %>% ## confusing but need to invert!
                 str_replace(., "(?<=timing_)lead", "b") %>% 
                 str_replace("_timing_(b|f)", "_\\1"))
  
}


## "Binning" min and max lag 
evt_bin_lags <- function(df_lag_leads, 
                         unit_var=FIPS, time_var=Date){
  ## guess min-max data
  cols <- enframe(colnames(df_lag_leads), value = "colname", name=NULL) %>% 
    filter(str_detect(colname, "_b[0-9]+$|_f[0-9]+$")) %>% 
    evt_get_lag_val(colname) %>% 
    distinct(lag_val_char, lag_val)
  
  ## 
  first_val <- filter(cols, lag_val==min(lag_val)) 
  last_val <- filter(cols, lag_val==max(lag_val)) 
  
  df_lag_leads %>% 
    group_by({{unit_var}}) %>% 
    mutate_at(vars(ends_with(first_val$lag_val_char)), ~ifelse(row_number<=which(.==1)[1], 1L, .) %>% 
                replace_na(0)) %>% 
    mutate_at(vars(ends_with(last_val$lag_val_char)), ~ifelse(row_number>=which(.==1)[1], 1L, .) %>% 
                replace_na(0)) %>% 
    ungroup()
}


## Utility function to extract lag values
evt_get_lag_val <- function(df, var = term, add_more=TRUE){ 
  res <- df %>% 
    mutate(lag_val_char = str_extract({{var}}, "((b|f)[0-9]{1,2})|_0"),
           lag_val = str_replace(lag_val_char, "b", "-") %>% 
             str_remove("^b|^f|_") %>% 
             as.integer())
  if(add_more){
    res <- res %>% 
      mutate(term_cat=str_remove({{var}}, "_((b|f)[0-9]{1,2})|_0"),
             has_lags= str_detect({{var}}, "_((b|f)[0-9]{1,2})|_0"))
  }
  ## check for NAs
  if(anyNA(res$lag_val)) warning("NAs in lag extraction!")
  
  res
}


################################
#'## Run event study
################################

evt_reg <- function(df, vars_treatment_reg,
                    unit_var=FIPS, time_var=Date, 
                    baseline, var_dependent,vars_controls = "",var_clustering,
                    direct_effect=TRUE, show_formula = T, pop_weights = F) {
  
  
  # If interactions, remove them
  var_policy_only <- str_remove_all(vars_treatment_reg, ":.*")
    

  
  # Policy only, all treatment lags and leads
  dummy_vars_in_reg <- str_extract(names(df), paste0("(",paste(var_policy_only, collapse = "|"),")","_((b|f)[0-9]{1,2}$)"))

  dummy_vars_in_reg <- dummy_vars_in_reg[!is.na(dummy_vars_in_reg)]
  
  # remove the baseline dummy
  if (as.integer(baseline)==baseline) {
    if(baseline == 0) {
      baseline_reg <- "_f0$"
    } else if(baseline < 0 ) {
      baseline_reg <- paste0("_b",abs(baseline),"$") 
    } else if(baseline > 0 ) {
      baseline_reg <- paste0("_f",abs(baseline),"$")
    }
  } else {
    stop("Yo baseline needs to be an integer!")
  }
  
  dummy_vars_in_reg <- dummy_vars_in_reg[!str_detect(dummy_vars_in_reg, baseline_reg)] 
  
  
  ### Check if presence of interaction
  vars_index_is_interacted <- str_detect(vars_treatment_reg, ":")
  
  for (i in 1:length(vars_index_is_interacted)) { ## ADD CASE WHERE BOTH
    
    if(vars_index_is_interacted[i]) { #If there is an interaction ":"
      
      policy_not_interacted <- str_extract(vars_treatment_reg[i], ".*(?=:)") #Left of :
      interaction_var <- str_extract(vars_treatment_reg[i], "(?<=:).*") #Right of :
      
      dummy_vars_in_reg <- str_replace(dummy_vars_in_reg, 
                                       paste0(policy_not_interacted,"_((b|f)[0-9]{1,2}$)"),
                                       paste0(dummy_vars_in_reg,":",interaction_var)) #Add interaction to event dummy
    }
    
  }

  
  # Add controls
  if(vars_controls[1] != "") {
    treatment_and_controls <- c(dummy_vars_in_reg,vars_controls)
  } else {
    treatment_and_controls <- dummy_vars_in_reg
  }
  
  #Add direct effect
  if(direct_effect) { 
    direct_effect_dummies <- str_extract(treatment_and_controls, ".*(?=:)") #Left of :
    direct_effect_dummies <- direct_effect_dummies[!is.na(direct_effect_dummies)]
    treatment_and_controls <- c(treatment_and_controls,direct_effect_dummies)
  }
  
  treatment_and_controls <- paste(treatment_and_controls, collapse = " + ")
  
  unit_var_char <- rlang::as_name(ensym(unit_var))
  time_var_char <- rlang::as_name(ensym(time_var))
  
  formula_felm <-  paste0(var_dependent," ~ ", 
                          treatment_and_controls," | ",unit_var_char,"+",time_var_char," | 0 | ", var_clustering)  %>%
    as.formula()
  
  if(show_formula) {
    print(formula_felm)  
  }
  
  if(!pop_weights) {
    output_from_felm <- felm(as.formula(formula_felm), data = df) 
  } else if(pop_weights) {
    output_from_felm <- felm(as.formula(formula_felm), weights = df$Population_county, data = df) 
  }
  
  
}





######################################## Make output tidy! ########################################

evt_tidy <- function(reg_output,vars_treatment,
                     unit_var=FIPS, time_var=Date, 
                     baseline, number_lags, number_leads,var_interaction, significance_level=0.05) { 
  
  output_made_tidy <- tidy(reg_output) 
  
  
  check_na <- output_made_tidy %>% filter(is.na(estimate)|is.nan(estimate)) %>% nrow()
  
  if(check_na > 0) {
    message("removing ",check_na," NA coefficients from table")
    
    
    output_made_tidy <- output_made_tidy %>%
      filter(!is.na(estimate))
    
  }
  
  print(output_made_tidy)
  
  output_made_tidy <- output_made_tidy %>% 
    mutate(Before_colons = str_extract(term, ".*(?=:)") ) %>%
    mutate(After_colons = str_extract(term, "(?<=:).*") ) %>%
    mutate(Before_colons_is_policy = str_detect(Before_colons, "_((b|f)[0-9]{1,2}$)|(_baseline$)") ) %>%
    mutate(order_corrected = if_else(Before_colons_is_policy, 
                                     paste0(After_colons,":",Before_colons),
                                     as.character(NA) )) %>%
    mutate(term = if_else(!is.na(order_corrected),order_corrected,term)) %>% #impute correct order
    select(-Before_colons,-Before_colons_is_policy,-After_colons,-order_corrected)
  
  
  # Get dummy right after baseline
  if (as.integer(baseline)==baseline) {
    if(baseline >= -1) {
      dummy_post_baseline <- paste0("_f",baseline+1)
    } else if(baseline < -1 ) {
      dummy_post_baseline <- paste0("_b",abs(baseline+1) )
      message(paste0(vars_treatment,dummy_post_baseline))
    }
  } else {
    stop("Yo baseline needs to be an integer!")
  }
  

  ### Check if presence of interaction
  vars_index_is_interacted <- str_detect(vars_treatment, ":")
  
  policies_only <- vars_treatment #Vector to replace
  
  for (i in 1:length(vars_index_is_interacted)) {
    
    if(vars_index_is_interacted[i]) { #If there is an interaction ":"
      
      policies_only[i] <- str_extract(vars_treatment[i], ".*(?=:)") #Left of :
      
    } else {
      
      policies_only[i] <- vars_treatment[i]
      
    }
    
  }

  
    # Add indices where rows need to be added (right before the baseline)
  all_coefs <- output_made_tidy %>%
    mutate(add_index = as.integer(rownames(output_made_tidy)) ) %>%
    mutate(add_index = if_else(str_detect(term,
                                          paste(paste0(policies_only,dummy_post_baseline), collapse = "|")
                                          ), #detect _bx or _fx
                                add_index,
                                as.integer(0))   )
  
  adding_rows <- unique(all_coefs$add_index)[-1] #Remove first value that is 0
  
  
  for (i in 1:length(adding_rows)) { 
    all_coefs <- all_coefs %>%
      as_tibble() %>%
      add_row(term = "", estimate = 0,std.error = 0,statistic = 0,
              p.value = 0,add_index = 0,.before = adding_rows[i])
    
    adding_rows = adding_rows +1 #need to move the indices since we are adding rows
  }
  
  
  
  all_coefs <- all_coefs %>%
    mutate(term = ifelse(term == "", 
                         paste0( str_extract(lag(term), ".*(?=_(b|f)\\d$)"),"_baseline"),
                         term)) 
  
  coefs_cleaned_up <- all_coefs %>%
    mutate(Treatment_var = str_extract(term, ".*(?=_((b|f)[0-9]{1,2}$)|(_baseline$))") ) %>%
    mutate(check_relative_time = str_extract(term, "((b|f)[0-9]{1,2}$)|(_baseline$)") ) %>%
    mutate(check_before_after = str_extract(check_relative_time, "(b|f)(?=[0-9]{1,2}$)|(_baseline$)") ) %>%
    mutate(get_digit_time = as.numeric(str_extract(check_relative_time, "(?<=(b|f))[0-9]{1,2}$")) ) %>%
    mutate(Relative_time = case_when(check_before_after == "b" ~ -get_digit_time,
                                       check_before_after == "f" ~ get_digit_time,
                                       check_before_after == "_baseline" ~ baseline,
                                       TRUE ~ 9999 #Problem
                                       )) %>%
    select(-check_relative_time,-check_before_after,-get_digit_time) %>%
    mutate(estimate_signif = ifelse(p.value<significance_level, estimate, NA),
           estimate_not_signif = ifelse(p.value>significance_level, estimate, NA)) %>%
    mutate(Policy_var = if_else(str_detect(term,":"),str_extract(term, "(?<=:).*"),term)) %>%
    mutate(Policy_var = str_extract(Policy_var, ".*(?=_((b|f)[0-9]{1,2}$)|(_baseline$))") ) %>%
    mutate(Interaction_var = if_else(str_detect(term,":"),"Interacted","NA")) %>%
    mutate(Interaction_var = if_else(Interaction_var=="Interacted", str_extract(term, ".*(?=:)"),"NA")) %>%
    mutate(Interaction_var = na_if(Interaction_var,"NA")) %>%
    mutate(Cohort_nb = paste0("Cohort ", str_extract(Interaction_var, "(?<=_cohort)[0-9]{1,2}")) )
  
  
  
  number_vars_control <- coefs_cleaned_up %>% filter(is.na(Treatment_var)) %>% nrow()
  
  #Need to reorganize order of term in table if interaction (put interaction after the :)
  
  #Need to fix check if interactions
  if(!any(str_detect(vars_treatment,":"))) {
   #Check for number of rows
  }
  else {
    if( nrow(coefs_cleaned_up) != (length(vars_treatment)*(number_lags+number_leads+1)+number_vars_control) ) message("problem with number of rows in tidy tibble!")
  }
  
  coefs_cleaned_up
}


################################
#'## Plot
################################

evt_plot <- function(df, color, linetype=NULL,
                     fac1_var = NULL, fac2_var = NULL,
                     scales = "free", fac_space = "fixed", switch="y", width=18,
                     size=7,
                     errorbar=TRUE, point=TRUE){
  
  plot <- ggplot(data = df, aes(x=Relative_time, y=estimate, color={{color}}, linetype={{linetype}})) +
    {if(errorbar) geom_errorbar(data = df %>% filter(!is.na(estimate_signif)), aes(x=Relative_time, ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error, color = {{color}}), width=.2, position=position_dodge(0.05)) } +
    geom_line() +
    {if(point) geom_point(data = df %>% filter(!is.na(estimate_signif)), aes(x=Relative_time, y=estimate_signif, color={{color}}), size = 1)}+
    mat_gg_hline_0 
  
  plot + facet_grid(rows = if (rlang::quo_is_null(enquo(fac1_var))) NULL
                    else rlang::enquos(fac1_var), 
                    cols = if (rlang::quo_is_null(enquo(fac2_var))) NULL
                    else rlang::enquos(fac2_var), 
                    scales = scales, space = fac_space, 
                    switch=switch,
                    labeller = label_wrap_gen(width=width)) +
    xlab("Relative time") +
    ylab("Coefficient") +
    theme(strip.text.y = element_text(size=size),
          legend.position="bottom",
          legend.box = "horizontal",
          panel.background = element_rect(fill = "white"),
          panel.grid=element_line(color="#cccccc", size=0.2),
          panel.grid.major=element_line(color="#cccccc", size=0.2),
          panel.grid.minor=element_line(color="#cccccc", size=0.15))
}

######################################################
######################################################
######################################################
######################################################


#full testing

if(FALSE) {
    
    df_final <- readRDS("Code/merge_main_datasets/merged_panel_did.rds")%>% 
      arrange(FIPS,  Date)
    
    ### Full
    df_final_built <- df_final %>%
      evt_data_build(number_lags = 21,number_leads = 21)
    
    df_reg <- df_final_built %>%
      left_join(df_final) 
    
    test_reg <- evt_reg(df_reg, baseline = -1, var_dependent = "Completely_home_log", 
                        vars_controls = c("prcp","snow","tmean"), var_clustering = "FIPS")
    
    tidy_output <- evt_tidy(reg_output = test_reg, baseline = -1, number_lags = 21, number_leads = 21)  
    
    evt_plot(df = tidy_output, color = Treatment_var, linetype=Treatment_var)
  
}
  
######################################################
######################################################
######################################################
######################################################


################################
#'## Checks
################################

if(FALSE){
  
  #### Quick example  ####
  
  ## get data
  df_final <- readRDS("Code/merge_main_datasets/merged_panel_did.rds")%>% 
    arrange(FIPS,  Date)
  
  ## Smaller data
  df_TOY <- df_final %>% 
    filter(FIPS %in% c("01003", "01001")) %>% 
    select(FIPS, Date, all_of(c("County_ED", "County_SIP_BC", "State_ED", "State_SAH_BC")))
  
  ## Function use:
  res_TOY <- df_TOY %>% 
    evt_data_build(vars_treatment =c("County_ED", "County_SIP_BC", "State_ED", "State_SAH_BC"))
  
  #### Detailed steps  ####
  
  ## Step 1: add  timing variable, i.e with _timing and _days
  vars_treatment ="County_ED"
  vars_treatment =c("County_ED", "County_SIP_BC", "State_ED", "State_SAH_BC")
  
  STEP_1_out <- df_TOY %>% 
    evt_add_timing(vars_treatment=vars_treatment,
                   unit_var= FIPS,
                   time_var=Date) 
  
  
  # look at output of Step 1:  (window of 2 days before/after)
  STEP_1_out %>% 
    filter(Date %in% c(filter(STEP_1_out, State_ED_timing==1)$Date + rep(c(-1,0, 1, 2), 2))) %>% 
    select(FIPS, Date, State_ED, State_ED_timing, State_ED_days)
  
  
  #### Internal check  ####
  ## No NAs?
  res_TOY %>% 
    filter_all(any_vars(is.na(.))) %>% 
    mat_check_0row()
  
  ## timing? 55 for 01001
  res_TOY %>% 
    filter(FIPS == "01003") %>% 
    filter(County_ED_timing==1) %>% 
    select(1:8)
  
  ## test values around timing
  res_TOY %>% 
    select(FIPS, Date, row_number, County_ED_timing,
           County_ED_b2, County_ED_b1, County_ED_f0, County_ED_f1) %>% 
    filter(between(row_number, 52, 57))
  
  ## test binning
  res_TOY %>% 
    select(FIPS, Date, row_number, County_ED_timing,
           County_ED_b20, County_ED_b19, County_ED_b18) %>% 
    filter(between(row_number, 55-23, 55-18))
  
  unit_var <- quo(FIPS)
  time_var <- quo(Date)
  
}



if(FALSE){
  
  ## happens in 55!
  df_TOY_s1 <- df_TOY %>% 
    filter(FIPS=="01003") %>% 
    evt_add_timing()
  
  ## step 2: add lead/lags of the single dummy event
  df_TOY_s2 <- evt_add_lags(df_timing=df_TOY_s1)
  
  df_TOY_s2 %>% 
    select(FIPS, Date, row_number, County_ED_timing,
           County_ED_f1, County_ED_f0, County_ED_b1, County_ED_b2) %>% 
    filter(between(row_number, 52, 57))
  
  ## step 3: bin outer lags
  df_TOY_s3 <- evt_bin_lags(df_lag_leads=df_TOY_s2)
  df_TOY_s3 %>% 
    select(FIPS, Date, row_number, County_ED_timing,
           County_ED_b20, County_ED_b19, County_ED_b18) %>% 
    filter(between(row_number, 55-23, 55-18))
  
  
}



##########



if(FALSE){
  
  ## Quick demo
  df_demo <- tibble(FIPS=c(1,2),
                    date=as.Date(c("2020-02-02", "2020-02-04")),
                    date_event_2 = as.Date(c(NA, "2020-02-03")))
  
  ## demo: 1 event
  df_demo %>% 
    evt_add_timing_from_date(var_date = "date",
                             start_date = as.Date("2020-02-01"),
                             end_date=as.Date("2020-02-05"))
  
  ## demo: 2 events
  df_demo %>% 
    evt_add_timing_from_date(var_date = c("date","date_event_2"),
                             start_date = as.Date("2020-02-01"),
                             end_date=as.Date("2020-02-05"))
  
  
  ## Now on or data
  ## just formatting data as if we had dates! sloow
  # State_ED, State_SAH_BC
  df_final_dates <- df_final%>% 
    # sample_n(5000) %>% 
    distinct(State_name, FIPS, Date, County_ED, County_SIP_BC, State_ED, State_SAH_BC) %>% 
    filter_at(vars(County_ED, County_SIP_BC, State_ED, State_SAH_BC), any_vars(.==1)) %>% 
    gather(variable, has_event, County_ED, County_SIP_BC, State_ED, State_SAH_BC) %>% 
    filter(has_event==1) %>% 
    group_by(FIPS, variable) %>% 
    filter(Date==min(Date)) %>% 
    # slice_min(Date) %>% 
    ungroup() %>% 
    spread(variable, Date) %>% 
    select(-has_event)
  
  df_final_dates
  
  ## test
  out <- df_final_dates %>% 
    evt_add_timing_from_date(start_date=as.Date("2020-01-20"),
                             end_date=as.Date("2020-04-21"))
  
  out
  
  ## compare with first function
  out_alter <- df_final %>% 
    evt_add_timing() %>% 
    rename(date=Date)
  
  all.equal(out_alter %>% 
              select(all_of(colnames(out))),
            out)
  
  ## test with only one variable!
  df_1var <- df_final_dates %>% 
    select(State_name, FIPS, County_ED) %>% 
    rename(Date=County_ED) %>% 
    evt_add_timing_from_date(var_date = "Date",
                             start_date=as.Date("2020-01-20"),
                             end_date=as.Date("2020-04-21"))
}

