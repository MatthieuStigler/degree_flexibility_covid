#' ---
#' description: Summary table of mobility measures
#' run_final: TRUE
#' ---


my_packages <- c("tidyverse","magrittr","stargazer","xtable")

sapply(my_packages, require, character.only = TRUE)
sapply(my_packages, function(x) packageVersion(x) %>%  as.character)

map<-purrr::map #For the nice iterations
select<-dplyr::select
if(!require(matPkg)) remotes::install_github("MatthieuStigler/matPkg", upgrade = "never")

source("code_replicate/auxiliary_scripts/888_table_to_pdf.R")

options(scipen=999)

################################
#'## Read data
################################


did <- readRDS("data_replicate/merged_panel_did.rds")

table_vars <- read_rds("data_replicate/table_responses_names.rds")
table_covars <- read_rds("data_replicate/table_covariates_reg.rds")

################################
#'## Prepare
################################

other_vars <- table_covars %>% 
  filter(str_detect(covariate_name, "tmean|prcp|snow|Cases|Cases_dummy|Deaths_dummy"))

mobil_vars <- table_vars$list_var
all_vars <- c(mobil_vars, as.character(other_vars$covariate_name))
glimpse(did)

did_mobil <- did %>%
  select(State_name, County_name, FIPS, Date,
         all_of(all_vars))

did_mobil

################################
#'## Remove resign
################################


did_mobil

outcomes_to_resigned <- table_vars %>% filter(Sign_scaling == "Mobility") %>% pull(list_var)


did_mobil_rawSigns <- did_mobil %>% 
  mutate_at(outcomes_to_resigned, ~ -.)

did_mobil_rawSigns


################################
#'## Compute stats
################################

## long
did_mobil_l <- did_mobil_rawSigns %>% 
  select(FIPS, Date, all_of(all_vars)) %>% 
  gather(list_var, value, -Date, -FIPS) %>% 
  filter(!is.na(value))

did_mobil_l

split_date <- as.Date("2020-03-11")

## stats
smry <-  did_mobil_l%>% 
  group_by(list_var) %>% 
  summarise(Mean = mean(value),
            Mean_before_Mar_11=mean(value[Date <split_date]),
            Mean_after_Mar_11=mean(value[Date >=split_date]),
            Min = min(value),
            Max = max(value),
            N = n(),
            N_county = n_distinct(FIPS)) %>% 
  ungroup() %>% 
  mutate(Difference=Mean_after_Mar_11-Mean_before_Mar_11)

smry

## t tests
did_mobil_nst <- did_mobil_l%>% 
  mutate(timing=if_else(Date<split_date, "before", "after")) %>% 
  nest(data=-list_var) 

dat <- did_mobil_nst$data[[1]] 


did_mobil_test <- did_mobil_nst%>% 
  mutate(t_test= map(data, ~t.test(value~ timing, data=.)),
         before=map_dbl(t_test, ~.$estimate[1]),
         after=map_dbl(t_test, ~.$estimate[2]),
         diff=map_dbl(t_test, ~diff(.$estimate)),
         pval=map_dbl(t_test, ~.$p.value),
         stat=map_dbl(t_test, ~.$statistic))

did_mobil_test %>% 
  arrange(abs(stat)) %>% 
  left_join(smry %>% 
              select(list_var, Difference, starts_with("Mean"))) %>% 
  filter(str_detect(list_var, "Median"))
  

## more stats just for text:
## total count
did_mobil_l %>% 
  filter(list_var %in% mobil_vars) %>% 
  group_by(list_var) %>% 
  summarise(n_FIPS = n_distinct(FIPS),
    n_days = n_distinct(Date)) %>% 
  ungroup() %>% 
  left_join(table_vars %>% 
              select(list_var, list_names, Source), by = "list_var") %>% 
  distinct(Source, n_days, n_FIPS)

## some FIPS might not have all days! check:
did_mobil_l %>% 
  filter(list_var %in% mobil_vars) %>% 
  group_by(list_var, FIPS) %>% 
  summarise(#n_FIPS = n_distinct(FIPS),
            n_days = n_distinct(Date)) %>% 
  ungroup() %>% 
  count(list_var, n_days, name = "n_FIPS") %>% 
  left_join(table_vars %>% 
              select(list_var, list_names, Source), by = "list_var") %>% 
  distinct(Source, n_days, n_FIPS)


################################
#'## Polish output smry
################################

table_vars$Source
sources_order <- c("Safegraph", "PlaceIQ", "Google","Cuebiq")
sources_order_2 <- c("Safegraph (SG)", "PlaceIQ (PQ)", "Google (GM)","Cuebiq (CQ)")


table_vars %>% count(Sign_scaling)

table_vars_prep <- table_vars %>% 
  mutate(is_resigned = if_else(Sign_scaling == "Mobility", "yes", "no"),
         Source = paste0(Source, " (", str_to_upper(Source_short), ")")) %>% 
  select(list_var, list_names, Source, Source_short, is_resigned)

## add Source and sign info
smry_c <- smry %>% 
  filter(list_var %in% mobil_vars) %>% 
  left_join(table_vars_prep, by = "list_var") %>% 
  mutate(Source=factor(Source, levels = sources_order_2)) %>% 
  rename(Variable=list_names) %>% 
  select(-list_var) %>% 
  rename_all(~str_replace_all(., "_", " ")) %>% 
  select(Source, Variable, `is resigned`, N, `N county`, starts_with("Mean"), Difference) %>%
  rename(`Counties`=`N county`) %>% 
  arrange(Source, Variable) %>% 
  mutate(Source = mat_keep_first(Source, warn=FALSE),
         Variable = str_replace_all(Variable, "_", " "),
         N = formatC(N, big.mark=","),
         Variable = str_replace(Variable, "\\%", "share"))
smry_c

################################
#'## Other vars
################################

## OLD
smry_other_OLD <- smry %>% 
  filter(list_var %in% other_vars$covariate_name) %>% 
  left_join(table_covars %>% 
              select(covariate_name, covariate_name_clean),
            by = c("list_var"="covariate_name")) %>% 
  rename(Variable=covariate_name_clean) %>% 
  left_join(did_mobil_test %>% 
              select(list_var, pval)) %>% 
  select(-list_var) %>%
  rename_all(~str_replace_all(., "_", " ")) %>% 
  select(Variable, N, starts_with("Mean"), Difference)

smry_other_OLD

## NEW
smry_other <- smry %>% 
  filter(list_var %in% other_vars$covariate_name) %>% 
  left_join(table_covars %>% 
              select(covariate_name, covariate_name_clean),
            by = c("list_var"="covariate_name")) %>% 
  rename(Variable=covariate_name_clean) %>% 
  left_join(did_mobil_test %>% 
              select(list_var, pval), by = "list_var") %>% 
  select(-list_var) %>%
  rename_all(~str_replace_all(., "_", " ")) %>% 
  rename(Counties = `N county`) %>% 
  select(Variable, N, Counties, Mean, Min, Max)

smry_other


################################
#'## To TeX
################################

## texitzie it
smry_add_xt <- smry_c %>% 
  select(-`Counties`) %>% 
  xtable()
caption(smry_add_xt) <-  "Summary statistics: mobility"

digits(smry_add_xt) <-  c(0, 0, 0, 0, 0, rep(2, 4))


## shorter and newer version
smry_add_short_xt <- smry_c %>% 
  select(-contains("Mar 11"), -Difference) %>% 
  rename("Re-signed" = `is resigned`) %>%
  xtable()


caption(smry_add_short_xt) <-  "Summary statistics: mobility"

digits(smry_add_short_xt) <-  c(rep(0, 4), rep(2, 3))
align(smry_add_short_xt) <-  "lllllll"



print(smry_add_short_xt, include.rownames = FALSE, booktabs = TRUE,
      sanitize.colnames.function = function(x) x,
      caption.placement = "top",floating=FALSE,comment=FALSE,
      file = "output_replicate/tab_2_summary_statistics_mobility_short_tabular.tex")



################################
#'## To pdf
################################

prj_table_to_pdf("output_replicate/tab_2_summary_statistics_mobility_short_tabular.tex")
