#' description: "Merge prepare event panel"

#---------------------------------------------------------------------------------------------------------------
#---------- This code creates the event panel from the event function
#---------------------------------------------------------------------------------------------------------------

my_packages <- c("tidyverse","magrittr","tictoc")
#install.packages(my_packages, repos = "http://cran.rstudio.com")
lapply(my_packages, library, character.only = TRUE)

map<-purrr::map 
select<-dplyr::select

options(stringsAsFactors = FALSE) ## Do not load strings as factors
source("code/auxiliary_scripts/888_event_code.R")

#-----------------------------------------------------------------------------------------------------------------

did <- readRDS("data_replicate/2_data_final/merged_panel_did_long.rds")



event_panel <- evt_data_build(did, vars_treatment =c("County_ED", #"County_SIP_BC", "State_ED", "State_SAH_BC",
                                                     "County_SIP",
                                                     "StatePol_ED","StatePol_Resclo","StatePol_SIP"),
                              unit_var=FIPS, time_var=Date,
                              number_lags=21, number_leads=21) #35s for 6 treatment vars and 377,487 obs

event_panel_cleaned <- event_panel %>%
  filter(Date <= "2020-04-21")


did_cleaned <- did %>%
  filter(Date <= "2020-04-21")

df_final <- left_join(did_cleaned,event_panel_cleaned)



saveRDS(df_final, "data_replicate/2_data_final/merged_panel_event_dummies.rds")
