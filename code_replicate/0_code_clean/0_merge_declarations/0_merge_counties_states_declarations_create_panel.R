#"This code merges the declarations and outpout panel"


my_packages <- c("tidyverse","magrittr","sf")
#install.packages(my_packages, repos = "http://cran.rstudio.com")
lapply(my_packages, library, character.only = TRUE)

map<-purrr::map 
select<-dplyr::select

#-------------------------------------------- ----------------------------------------------------------------

# Counties collected by MTurk + manually checked
counties <- readRDS("data_replicate/0_data_raw/counties_declarations_cleaned.rds")

# States collected manually
states <- readRDS("data_replicate/0_data_raw/clean_states_declarations.rds")

# Additional states from github Oct. 11, 2020
#https://raw.githubusercontent.com/COVID19StatePolicy/SocialDistancing/master/data/USstatesCov19distancingpolicy.csv
states_revised <- readRDS("data_replicate/0_data_raw/state_policies_dateissued_group.rds") %>%
  select(-StateFIPS)

states_policies_both_sources <- states %>%
  left_join(states_revised, by = c("State_name" = "StateName") )

cities <- readRDS("data_replicate/0_data_raw/cities_min_policy.rds")  

merged_cleaned <- counties %>%
  left_join(states, by = "State_name") %>%
  left_join(states_revised, by = c("State_name" = "StateName") )%>%
  left_join(cities, by = c("FIPS")) 

glimpse(merged_cleaned)
  
check <- cities %>%
  anti_join(counties) %>%
  as.data.frame() %>%
  print()


#-----------------------------------------------------------------------------
saveRDS(merged_cleaned, "code_replicate/0_code_clean/0_merge_declarations/declarations_counties_states_dates.rds")  
#-----------------------------------------------------------------------------

message("Cross section of dates was cleaned and saved!!")


#----------------------------------------------------------------------------------------------
#--------------------------- Add cases     ------------------------------------
#----------------------------------------------------------------------------------------------

# read
complete_panel <- readRDS("code_replicate/0_code_clean/0_clean_cases_mortality_NYT/cases_mortality_nyt.rds")
complete_panel

## get unique date per FIPS
first_cases_dates_df <- complete_panel %>% 
  select(Date, FIPS, starts_with("Date_first")) %>% 
  gather(type, Date_first, starts_with("Date_first")) %>% 
  filter(!is.na(Date_first)) %>% 
  distinct(FIPS, type, Date_first) %>% 
  spread(type, Date_first) 

first_cases_dates_df

first_cases_dates_df %>% count(FIPS) %>% count(n)

## check merge:
no_cases_right <- merged_cleaned %>% 
  anti_join(first_cases_dates_df, by="FIPS")

no_cases_right

## do these FIPS have indeed no cases? yes!
complete_panel %>% 
  filter(FIPS%in%no_cases_right$FIPS) %>% 
  filter(Cum_cases>0|Cum_deaths>0) 

## check cases but no declaration?
no_cases_left <-  first_cases_dates_df%>% 
  anti_join(merged_cleaned, by="FIPS")

## not in data?
merged_cleaned %>% 
  filter(FIPS %in% no_cases_left$FIPS)

## merge
merged_cleaned_cases <- merged_cleaned %>% 
  left_join(first_cases_dates_df, by="FIPS") 

merged_cleaned_cases

## export
write_rds(merged_cleaned_cases,
          "code_replicate/0_code_clean/0_merge_declarations/declarations_counties_states_cases_dates.rds")  

#----------------------------------------------------------------------------------------------
#--------------------------- Now merging the panel version ------------------------------------
#----------------------------------------------------------------------------------------------

# From MTurk and manually checked, put into panel
counties_panel <- readRDS("data_replicate/0_data_raw/counties_declarations_panel.rds")
glimpse(counties_panel)

us_counties <- readRDS("code_replicate/0_code_clean/0_clean_map_us_counties/us_acs5_sf.rds") %>%
  select(FIPS, Population_county) %>%
  st_drop_geometry()

counties_panel_pop_cleaned <- counties_panel %>%
  left_join(us_counties, by = "FIPS")#Removing this since they come from NACO and are incomplete ...

## Now merging with ALL cities declarations to create a variable that is the share of population under a policy
cities_all <- readRDS("data_replicate/0_data_raw/all_cities_policy.rds")

panel_merged <- counties_panel_pop_cleaned %>%
  left_join(states_policies_both_sources, by = "State_name") %>%
  left_join(cities_all, by = c("FIPS")) #Add cities

glimpse(panel_merged)
table(panel_merged$City_ED)

# Create City dummies
panel_merged_city_share_computed <- panel_merged %>%
  mutate(City_ED_dummy = case_when(is.na(City_ED) | (City_ED > Date) ~ 0,
                                   City_ED <= Date ~ 1,
                                   TRUE ~ 9999),
         City_SIP_dummy = case_when(is.na(City_SIP) | (City_SIP > Date) ~ 0,
                                    City_SIP <= Date ~ 1,
                                   TRUE ~ 9999),
         City_earliest_policy_dummy = case_when(is.na(City_earliest_policy) | (City_earliest_policy > Date) ~ 0,
                                    City_earliest_policy <= Date ~ 1,
                                    TRUE ~ 9999)
         ) #%>%
  #select(FIPS,Date,Population_county,City_pop_2019_est,City_ED,City_ED_dummy,City_SIP,City_SIP_dummy) # just for checks

summary(panel_merged_city_share_computed) #Check no 9999
message("City ED dummy almost like City earliest policy dummy -> little variation")


#---- Compute population share under city policy
panel_merged_city_share_computed %<>% 
  # ED
  group_by(Date,FIPS,City_ED_dummy) %>%
  mutate(City_ED_county_pop_share = ifelse(City_ED_dummy==1, 
                                           sum(City_pop_2019_est, na.rm = T)/Population_county, 
                                           0)
         ) %>%
  group_by(Date,FIPS) %>%
  mutate(City_ED_county_pop_share = max(City_ED_county_pop_share, na.rm = T)) %>% #Take the max to ignore 0
  # SIP
  group_by(Date,FIPS,City_SIP_dummy) %>%
  mutate(City_SIP_county_pop_share = ifelse(City_SIP_dummy==1, 
                                           sum(City_pop_2019_est, na.rm = T)/Population_county, 
                                           0)
  ) %>%
  group_by(Date,FIPS) %>%
  mutate(City_SIP_county_pop_share = max(City_SIP_county_pop_share, na.rm = T)) %>%
  # Combined
  group_by(Date,FIPS,City_earliest_policy_dummy) %>%
  mutate(City_earliest_policy_pop_share = ifelse(City_earliest_policy_dummy==1, 
                                            sum(City_pop_2019_est, na.rm = T)/Population_county, 
                                            0)
  ) %>%
  group_by(Date,FIPS) %>%
  mutate(City_earliest_policy_pop_share = max(City_earliest_policy_pop_share, na.rm = T)) %>%
  ungroup() 
  

### Bunch of checks
summary(panel_merged_city_share_computed)
check <- panel_merged_city_share_computed %>% filter(is.na(State_SAH_BC))
check <- panel_merged_city_share_computed %>% filter(FIPS %in% c("04013","06037") )
check <- panel_merged_city_share_computed %>% mutate(diff = City_earliest_policy_pop_share - City_ED_county_pop_share)
summary(check$diff)
rm(check)
# 2 cities have pop greater than county -- cap share to 1
check2 <- panel_merged_city_share_computed %>% filter(FIPS %in% c("48375","11001") )
rm(check2)

panel_merged_city_share_computed %<>%
  mutate_at( vars("City_ED_county_pop_share","City_SIP_county_pop_share","City_earliest_policy_pop_share"),
             ~if_else(.>1,1,.)) 

summary(panel_merged_city_share_computed)


### Now keeping only one row per FIPS/Date
panel_merged_city_share_computed_distinct <- panel_merged_city_share_computed %>%
  select(-City_ED_dummy,-City_ED,-City_SIP,-City_SIP_dummy,-City_earliest_policy,-City_earliest_policy_dummy,-City_pop_rank_within_county,-City_is_county,-City,
         -State,-City_pop_2019_est) %>%
  distinct(.keep_all = T)

summary(panel_merged_city_share_computed_distinct)
glimpse(panel_merged_city_share_computed_distinct)

#-------- Now putting the panel in DiD format (not event study yet)

if(nrow(counties_panel_pop_cleaned) == nrow(panel_merged_city_share_computed_distinct)) message("all good!") else stop("WTF")

glimpse(panel_merged_city_share_computed_distinct)



#---- Put in relative time
panel_merged_final <- panel_merged_city_share_computed_distinct %>%
  mutate(County_ED_date = County_ED,#---- Save variable as Date
         County_SIP_BC_date = County_SIP_BC,
         County_SIP_date = County_SIP,
         State_SAH_date = State_SAH,
         State_BC_date = State_BC,
         State_ED_date = State_ED,
         State_SAH_BC_date = State_SAH_BC,
         StatePol_ED_date = StatePol_ED,
         StatePol_Resclo_date = StatePol_Resclo,
         StatePol_SIP_date = StatePol_SIP
         ) %>%
  mutate_at(vars(County_ED,County_SIP,County_BC,County_SIP_BC,County_ED_SIP_BC, #County
                 State_SAH,State_BC,State_ED,State_SAH_BC,State_ED_SAH_BC, StatePol_ED, StatePol_Resclo, StatePol_SIP,#State
                 City_ED_min,City_SIP_min,City_earliest_policy_min), #City
  ~ ifelse(!is.na(.), Date - . , .)
  )

message("need to double check")

panel_merged_final %<>%
  mutate_at(vars(County_ED,County_SIP,County_BC,County_SIP_BC,County_ED_SIP_BC, #County
                 State_SAH,State_BC,State_ED,State_SAH_BC,State_ED_SAH_BC, #State
                 City_ED_min,City_SIP_min,City_earliest_policy_min,StatePol_ED, StatePol_Resclo, StatePol_SIP), #City
            ~ ifelse(is.infinite(.),NA , .)
  )

glimpse(panel_merged_final)
summary(panel_merged_final)

panel_merged_final %>% 
  count(Date) %>%
  count(n)
max(panel_merged_final$Date)
#-----------------------------------------------------------------------------
saveRDS(panel_merged_final, "code_replicate/0_code_clean/0_merge_declarations/declarations_counties_states_panel.rds")
#-----------------------------------------------------------------------------

