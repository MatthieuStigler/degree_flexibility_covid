#' description: "Merge main datasets"

my_packages <- c("tidyverse","magrittr","sf")
#install.packages(my_packages, repos = "http://cran.rstudio.com")
lapply(my_packages, library, character.only = TRUE)

map<-purrr::map #
select<-dplyr::select

#-------------------------------------------------------------------------------------

#Map of the US counties
us_census <- readRDS("data_replicate/1_data_intermediate/us_acs5_sf.rds") %>% 
  st_drop_geometry() 
 
table(us_census$Quantile_group)
table(us_census$Medincome_decile)

#County-level and state declarations 
declarations_panel <- readRDS("data_replicate/1_data_intermediate/declarations_counties_states_panel.rds")

length(unique(declarations_panel$FIPS))

#NYT mortality
nyt_cases_mortality <- readRDS("data_replicate/1_data_intermediate/cases_mortality_nyt.rds")

#Safegraph data
safegraph_SD <- readRDS("data_replicate/1_data_intermediate/safegraph_county_SD.rds") #not shared


#PlaceIQ
placeIQ_county <- readRDS("data_replicate/1_data_intermediate/placeIQ_county_device_exposure.rds")

#County-level static elections data
county_elections <- readRDS("data_replicate/1_data_intermediate/county_pres_elections_2016.rds")

#Google mobility data
gMobil <- read_rds("data_replicate/1_data_intermediate/google_mobility_clean.csv")
gMobil_w <- gMobil %>%
  select(-Category) %>%
  pivot_wider(names_from = Category_raw,
              values_from = Change, names_prefix = "gm_")
gMobil_w

#Weather data
weather <- readRDS("data_replicate/1_data_intermediate/weather_cleaned.rds")


#Cuebiq
cuebiq <- readRDS("data_replicate/1_data_intermediate/cuebiq_cleaned.rds") %>% #not shared
  select(-County_name,-State_name)



#-------------------------------------------- Merging -------------------------------------

merged <- declarations_panel %>%
  select(-Population_county) %>%
  left_join(us_census,by = c("FIPS", "County_name", "State_name"))

merged_2 <- merged %>%
  left_join(nyt_cases_mortality, by = c("FIPS","Date") ) %>%# ,"County_name","State_name")) %>%
  left_join(safegraph_SD, by = c("Date", "FIPS")) %>%
  left_join(placeIQ_county, by = c("Date", "FIPS")) %>%
  left_join(county_elections, by = c("FIPS", "State_abb")) %>%
  left_join(gMobil_w %>% 
              select(FIPS, Date, starts_with("gm_")), by = c("FIPS", "Date")) %>%
  left_join(weather, by = c("Date", "FIPS")) %>%
  left_join(cuebiq, by = c("Date", "FIPS"))

rm(merged)

#================# Generate log first (sign afterwards)

outcomes_names <- readRDS("data_replicate/1_data_intermediate/vars_names_and_formulas/table_responses_names.rds")

unique_outcome_names_non_gm <- unique(outcomes_names$list_var) #%>%
  #str_remove(.,"_pct")  # need to deal with Median_home_perc afterwards
unique_outcome_names_non_gm <- unique_outcome_names_non_gm[!str_detect(unique_outcome_names_non_gm,"gm_")]

merged_3 <- merged_2 %>%
  mutate_at(unique_outcome_names_non_gm, list(log = ~log(.))) %>%
  mutate_if(is.numeric, ~ replace(., is.infinite(.),NA))

rm(merged_2)

#-- Fixing names
names(merged_3) <- str_replace_all(names(merged_3), "_pct_log","_log")
names(merged_3) <- str_replace_all(names(merged_3), "_perc_log","_share_log")
names(merged_3)



#================# Re-signed variables here !111

outcomes_to_resigned <- outcomes_names %>% filter(Sign_scaling == "Mobility") %>% pull(list_var)
outcomes_to_resigned_log <- outcomes_names %>% filter(Sign_scaling == "Mobility") %>% pull(list_var_log)

outcomes_to_resigned_and_log <- unique(c(outcomes_to_resigned,outcomes_to_resigned_log))
rm(outcomes_to_resigned)
if(merged_3 %>% select(outcomes_to_resigned_and_log) %>% ncol() != length(outcomes_to_resigned_and_log)) error("var problem")

merged_4 <- merged_3 %>%
  mutate_at(outcomes_to_resigned_and_log, ~ -.)

message("change of sign done!")

rm(merged_3)

#--- Now generate 7-day moving average and difference
library(data.table)

merged_4 %<>%
  arrange(FIPS,Date) %>%
  group_by(FIPS) %>%
  mutate_at(vars(unique_outcome_names_non_gm), list(diff = ~ . - lag(.))) %>%
  mutate_at(vars(unique_outcome_names_non_gm), list(rolling = ~ frollmean(., n = 7, align = "right"))) %>%
  ungroup() %>%
  mutate_if(is.numeric, ~ replace(., is.infinite(.),NA))

detach("package:data.table", unload=TRUE) #Just needed frollmean


#=============================================================== 


check <- merged_4 %>% group_by(FIPS,Date) %>% add_count() %>% ungroup() %>% filter(n!=1)
if(nrow(check)==0) rm(check) else stop("problem")

if(nrow(merged_4)==nrow(declarations_panel)) {
  message("all good") 
  } else {
  stop("problem!")
}

check <- merged_4 %>% filter(is.na(State_ED))
if(nrow(check)==0) rm(check) else stop("problem")

check <- merged_4 %>% filter(is.na(Completely_home_log)) 
table(check$Date)


# Generate Day * State fixed effects
group_index <- merged_4 %>% group_by(Date,State_name) %>% group_indices(Date,State_name)
merged_4$Date_state_FE <- group_index

merged_5 <- merged_4 %>%
  select(State_name,County_name,FIPS,Date,everything() ) %>%
  mutate(Date_state_FE = as.character(Date_state_FE))

test <- merged_5 %>% select(Date,State_name,FIPS,Date_state_FE)


rm(merged_4)

#-------------------------------------------- Create DiD panel ------------------------------------------------------------

#Create DiD

did <- merged_5 %>%
 mutate_at(vars(County_ED,County_SIP,County_BC,County_SIP_BC,County_ED_SIP_BC,
                State_SAH,State_BC,State_ED,State_SAH_BC,State_ED_SAH_BC,
                StatePol_ED,StatePol_Resclo,StatePol_SIP),
           as.numeric
 )

summary(did)


#Cresate single dummy
did %<>%
  mutate_at(vars(County_ED,County_SIP,County_BC,County_SIP_BC,County_ED_SIP_BC,
                 State_SAH,State_BC,State_ED,State_SAH_BC,State_ED_SAH_BC,
                 City_ED_min, City_SIP_min,
                 StatePol_ED,StatePol_Resclo,StatePol_SIP),
           ~ ifelse(.>=0,1,0)
 )


message("imputing 0 if treatment is missing")

did %<>%
  mutate_at(vars(County_ED,County_SIP,County_BC,County_SIP_BC,County_ED_SIP_BC,
                 State_SAH,State_BC,State_ED,State_SAH_BC,State_ED_SAH_BC,
                 City_ED_min, City_SIP_min,
                 StatePol_ED,StatePol_Resclo,StatePol_SIP),
            ~ ifelse(is.na(.),0 , .)
  )


#------ Create interaction terms manually

did %<>%
  mutate(County_ED_T1 = if_else(Medincome_tercile=="0%_33%" & County_ED==1,1,0), 
         County_ED_T2 = if_else(Medincome_tercile=="33%_66%" & County_ED==1,1,0 ), 
         County_ED_T3 = if_else(Medincome_tercile=="66%_99%" & County_ED==1,1,0 )) %>%
  mutate_at(vars(County_ED_T1,County_ED_T2,County_ED_T3), ~ifelse(is.na(Medincome_tercile),NA,.)) #lines above wrongly imputed 0s... 



#did %<>% mutate(Any_policy_in_force = pmax(County_ED_SIP_BC,State_ED_SAH_BC)) #too few cases

did %<>%
  mutate(Cum_cases_before_county_policy = (1-County_ED_SIP_BC)*Cum_cases)
summary(did$Cum_cases_before_county_policy)
test <- did %>% select(FIPS,Date,Cum_cases,County_ED_SIP_BC,State_ED_SAH_BC,Cum_cases_before_county_policy)
  
#------ Add any_decision
did %<>% 
  rowwise() %>% 
  mutate(Any_policy = max(County_ED_SIP_BC, State_ED_SAH_BC,City_ED_min)) %>% 
  ungroup()

table(did$Date)



#----- REMOVING OLD POLICIES TO AVOID ISSUES, do earlier in the code if this didn't break anything

did %<>%
  select(-contains("State_ED")) %>%
  select(-contains("State_BC")) %>%
  select(-contains("State_SAH")) %>%
  select(-County_SIP_BC,-County_SIP_BC_date,
         -County_ED_SIP_BC)  %>%
  select(-c("State_name","County_name","State_abb","GEOID","County","State","state.x","County_FIPS","state.y",#new
            "stfips","Lat_cnty","Longit_cnty"))
names(did)

#-----------------------------------------------------------------------------
saveRDS(did, "data_replicate/2_data_final/merged_panel_did_long.rds")

did_raboted <- did %>%
  filter(Date <= "2020-04-21") 

saveRDS(did_raboted, "data_replicate/2_data_final/merged_panel_did.rds") 

#----------------------------------------------------------------------------
message("smooth exit")
