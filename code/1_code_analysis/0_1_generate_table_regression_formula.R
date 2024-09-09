#' ---
#' author: Matthieu
#' run_final: TRUE
#' ---

library(tidyverse)
library(Formula)
library(magrittr)


map<-purrr::map #For the nice iterations
select<-dplyr::select

source("code/auxiliary_scripts/general_options.R")


################################
#'## Prepare formulas
################################

formus_orig <-  tibble(formula = c(y~ State_ED  | Date + FIPS | 0 | FIPS,
                              y~ State_ED + State_SAH_BC  | Date + FIPS | 0 | FIPS,
                              y~ State_ED + State_SAH_BC + County_ED   | Date + FIPS | 0 | FIPS,  
                              y~ State_ED + State_SAH_BC + County_ED + County_SIP_BC  | Date + FIPS | 0 | FIPS,
                              y~ State_ED + State_SAH_BC + County_ED + County_SIP_BC  + Cum_cases | Date + FIPS | 0 | FIPS,
                              y~ State_ED + State_SAH_BC + County_ED + County_ED_T1 + County_ED_T3 + County_SIP_BC  | Date + FIPS | 0 | FIPS,
                              y~ State_ED + State_SAH_BC + County_ED:Trump_landslide + County_ED:Clinton_landslide + County_SIP_BC  | Date + FIPS | 0 | FIPS,
                              y~ State_ED + State_SAH_BC + County_ED:Tercile_group + County_SIP_BC | Date + FIPS | 0 | FIPS,
                              y~ State_ED + State_SAH_BC + County_ED + County_SIP_BC  + Cum_cases + Cum_cases_sq | Date + FIPS | 0 | FIPS,
                              y~ State_ED + State_SAH_BC + County_ED + County_SIP_BC  + Cum_cases_before_county_policy | Date + FIPS | 0 | FIPS,
                              y~ County_ED | Date + FIPS | 0 | FIPS,
                              y~ State_ED + State_SAH_BC + County_ED_SIP_BC | Date + FIPS | 0 | FIPS,
                              y~ State_ED + State_SAH_BC + County_ED + County_SIP_BC + tmean + prcp + snow | Date + FIPS | 0 | FIPS,
                              y~ State_ED + State_SAH_BC + County_ED + County_SIP_BC  + Cases| Date + FIPS | 0 | FIPS,
                              y~ State_ED + State_SAH_BC + County_ED + County_SIP_BC  + Cases_sqrt| Date + FIPS | 0 | FIPS,
                              y~ State_ED + State_SAH_BC + County_ED + County_SIP_BC + tmean + prcp + snow + Cases| Date + FIPS | 0 | FIPS,
                              y~ State_ED + tmean + prcp + snow | Date + FIPS | 0 | FIPS,
                              y~ State_SAH_BC + tmean + prcp + snow | Date + FIPS | 0 | FIPS,
                              y~ County_ED + tmean + prcp + snow | Date + FIPS | 0 | FIPS,
                              y~ County_SIP_BC + tmean + prcp + snow | Date + FIPS | 0 | FIPS,
                              y~ City_ED_county_pop_share + tmean + prcp + snow | Date + FIPS | 0 | FIPS,
                              y~ City_SIP_county_pop_share + tmean + prcp + snow | Date + FIPS | 0 | FIPS,
                              y~ State_ED + State_SAH_BC + County_ED + County_SIP_BC + City_ED_county_pop_share + City_SIP_county_pop_share + 
                                    tmean + prcp + snow | Date + FIPS | 0 | FIPS,
                              y~ State_ED + State_SAH_BC + County_ED + County_SIP_BC + City_ED_county_pop_share + City_SIP_county_pop_share +
                                    County_ED_cntW_lag1  + County_SIP_BC_cntW_lag1 + tmean + prcp + snow | Date + FIPS | 0 | FIPS,
                              y~ State_ED + State_SAH_BC + County_ED + County_SIP_BC + City_ED_county_pop_share + City_SIP_county_pop_share + 
                                    County_ED_cntW_lag1  + County_SIP_BC_cntW_lag1 + County_ED_cntW_lag2  + County_SIP_BC_cntW_lag2 +
                                    County_ED_cntW_lag3  + County_SIP_BC_cntW_lag3 +
                                    tmean + prcp + snow | Date + FIPS | 0 | FIPS,
                              y~ State_ED + State_SAH_BC + County_ED + County_SIP_BC + City_ED_county_pop_share + City_SIP_county_pop_share + 
                                County_ED_cntW_lag1  + County_SIP_BC_cntW_lag1 + County_ED_cntW_lag2  + County_SIP_BC_cntW_lag2 +
                                County_ED_cntW_lag3  + County_SIP_BC_cntW_lag3 +
                                Cum_cases + 
                                tmean + prcp + snow | Date + FIPS | 0 | FIPS,
                              y~ State_ED  + tmean + prcp + snow| Date + FIPS | 0 | FIPS,
                              y~ State_SAH_BC  + tmean + prcp + snow| Date + FIPS | 0 | FIPS,
                              y~ State_ED + State_SAH_BC  + tmean + prcp + snow| Date + FIPS | 0 | FIPS,
                              y~ County_ED + tmean + prcp + snow | Date + FIPS | 0 | FIPS,
                              y~ County_SIP_BC + tmean + prcp + snow | Date + FIPS | 0 | FIPS,
                              y~ County_ED + County_SIP_BC + tmean + prcp + snow | Date + FIPS | 0 | FIPS,
                              y~ City_ED_county_pop_share + tmean + prcp + snow| Date + FIPS | 0 | FIPS,
                              y~ City_SIP_county_pop_share + tmean + prcp + snow| Date + FIPS | 0 | FIPS,
                              y~ City_ED_county_pop_share + City_SIP_county_pop_share + tmean + prcp + snow| Date + FIPS | 0 | FIPS,
                              y~ State_ED + State_SAH_BC +  County_ED + County_SIP_BC + tmean + prcp + snow| Date + FIPS | 0 | FIPS,
                              y~ State_ED + State_SAH_BC + City_ED_county_pop_share + City_SIP_county_pop_share  + tmean + prcp + snow| Date + FIPS | 0 | FIPS,
                              y~ County_ED + County_SIP_BC + City_ED_county_pop_share + City_SIP_county_pop_share  + tmean + prcp + snow| Date + FIPS | 0 | FIPS,
                              y~ State_ED + State_SAH_BC +  County_ED + County_SIP_BC + City_ED_county_pop_share + City_SIP_county_pop_share  + tmean + prcp + snow| Date + FIPS | 0 | FIPS,
                              # 40
                              y ~ City_earliest_policy_pop_share+ tmean + prcp + snow | Date + FIPS | 0 | FIPS,
                              y ~ State_ED + State_SAH_BC + County_ED + County_SIP_BC+ City_earliest_policy_pop_share+ tmean + prcp + snow | Date + FIPS | 0 | FIPS,
                              y ~ State_ED + State_SAH_BC + County_ED + County_SIP_BC+ City_earliest_policy_pop_share+ Cases_dummy+tmean + prcp + snow | Date + FIPS | 0 | FIPS,
                              y ~ State_ED + State_SAH_BC + County_ED + County_SIP_BC+ City_earliest_policy_pop_share+ Deaths_dummy+tmean + prcp + snow | Date + FIPS | 0 | FIPS,
                              y ~ State_ED + State_SAH_BC + County_ED + County_SIP_BC+ City_earliest_policy_pop_share+ Cases_dummy+Deaths_dummy+tmean + prcp + snow | Date + FIPS | 0 | FIPS,
                              y~ StatePol_ED + StatePol_SIP+StatePol_Resclo  | Date + FIPS | 0 | FIPS,
                              y~ County_ED + County_SIP  | Date + FIPS | 0 | FIPS,
                              y~ City_earliest_policy_pop_share  | Date + FIPS | 0 | FIPS,
                              y~ StatePol_ED + StatePol_SIP + County_ED + StatePol_Resclo + County_SIP+City_earliest_policy_pop_share  | Date + FIPS | 0 | FIPS,
                              y~ StatePol_ED + StatePol_SIP + County_ED + StatePol_Resclo + County_SIP+City_earliest_policy_pop_share +tmean + prcp + snow | Date + FIPS | 0 | FIPS,
                              y~ StatePol_ED + StatePol_SIP + County_ED + StatePol_Resclo + County_SIP+City_earliest_policy_pop_share +tmean + prcp + snow +Cases_dummy| Date + FIPS | 0 | FIPS,
                              y~ StatePol_ED + StatePol_SIP + County_ED + StatePol_Resclo + County_SIP+City_earliest_policy_pop_share +tmean + prcp + snow +Deaths_dummy| Date + FIPS | 0 | FIPS,
                              y~ StatePol_ED + StatePol_SIP + County_ED + StatePol_Resclo + County_SIP+City_earliest_policy_pop_share +tmean + prcp + snow +Cases_dummy+Deaths_dummy| Date + FIPS | 0 | FIPS
                              
                              ),
                  formu_char = as.character(formula),
                  formu_desc =  c("", "state_only", "","benchmark_decisions_only", "benchmark_cumCases", 
                                  "manual_interact_tercile", "landslide", "interact_tercile","cum_cases_sq","cum_cases_early",
                                  "only_county_ED", "", "benchmark_enviro", "benchmark_cases", "benchmark_cases_sqrt", 
                                  "benchmark_enviro_cases", paste("vx", 1:10, sep="_"),
                                  "state_ED_only +W", "state_SIP_only +W", "state both +W", 
                                  "county_ED_only +W", "county_SIP_only +W", "county_both +W",
                                  "City_ED_only +W", "City_SIP_only +W", "City_both +W",
                                  "State_2_County_2 +W","State_2_City_2 +W", 
                                  "County_2_City_2 +W", 
                                  "all decisions +W",
                                  # 40
                                  "cityCombo + W", "5 dec + W", 
                                  "5 dec + 1Case+ W", #42
                                  "5 dec + 1Death + cumul+W", 
                                  "5 dec + 1Death+1Case + W",
                                  "StatePol: ED + SIP + RC",
                                  "County: ED + SIP",
                                  "City Only",
                                  "five dec + city", "five dec + city + W  ",
                                  "five dec + city + W + one case", 
                                  "five dec + city + W + one death", 
                                  "five dec + city + W + one death case")) %>% 
  mutate(formu_num = row_number(),
         formu_oct = formu_num %in% c(45, 46, 47, 48 ,49, 50, 51, 52),
         formu_final = formu_num %in% c(45:52),
         formu_vars =  map(formula, ~attr(terms(as.Formula(.), rhs=1, lhs=0), "term.labels") %>% 
                             enframe(name=NULL, value = "covariate_name")))%>%
  select(formu_desc, formu_num, formu_char, everything())

formus_orig

## last ones
tail(formus_orig)

## Check final regs
formus_orig %>% 
  filter(formu_final)



### Add legend for formulas we use

formus_orig %<>%
  mutate(formu_legend = case_when(formu_num == 45 ~ "State policies (ED+SIPO+ERC)",
                                  formu_num == 46 ~ "County policies (ED+SIPO)",
                                  formu_num == 48 ~ "All policies",
                                  formu_num == 49 ~ "All policies and weather",
                                  formu_num == 50 ~ "All policies, weather, cases dummy",
                                  formu_num == 51 ~ "All policies, weather, deaths dummy",
                                  formu_num == 52 ~ "All policies, weather, cases and deaths dummy"
                                  ) )



################################
#'## Visu/check results
################################

## function to compare two formulas-character
char_diff <- function(x, y){
  x_spli <- str_split(x, " |\\+")[[1]] %>% 
    discard(~.  %in% c("~", "|", "+", ""))
  y_spli <- str_split(y, " |\\+")[[1]] %>% 
    discard(~.  %in% c("~", "|", "+", ""))
  more <- y_spli[!y_spli%in%x_spli]
  less <- x_spli[!x_spli%in%y_spli]
  # if(length(more)==0) more <- NA_character_
  # if(length(more)>1) more <- paste(more, collapse = " AND ")
  # if(length(less)==0) less <- NA_character_
  more_out <-  if(length(more)>=1) paste("ADD:", paste(more, collapse = " AND ")) else NULL
  less_out <-  if(length(less)>=1) paste("REM:", paste(less, collapse = " AND ")) else NULL
  
  ##
  if(length(more_out)==0&length(less_out)==0) return("SAME")
  paste(more_out, less_out) %>% 
    str_squish()
}

## test
char_diff(x=formus_orig$formu_char[41], y=formus_orig$formu_char[42])
char_diff(x=formus_orig$formu_char[42], y=formus_orig$formu_char[41])

## just inspect vars:
formus_orig %>% 
  mutate(formu_lag = dplyr::lag(formu_char),
         formu_more=map2_chr(formu_char, formu_lag, ~char_diff(.y, .x))) %>% 
  select(1:2, formu_more) %>% 
  tail(7)

################################
#'## Export
################################

write_rds(formus_orig, "data_replicate/1_data_intermediate/vars_names_and_formulas/table_formulas_reg.rds")

