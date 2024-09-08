#'---------------------------------------------------------------------------------------------------------------
#' description: "Format US states map following our conventions"
#'---------------------------------------------------------------------------------------------------------------

my_packages <- c("tidyverse", "sf", "tigris")
sapply(my_packages, require, character.only = TRUE)

map<-purrr::map #For the nice iterations
select<-dplyr::select

if(!require(matPkg)) remotes::install_github("MatthieuStigler/matPkg", upgrade = "never")
#-------------------------------------------- ----------------------------------------------------------------

###############################
#'## Download
################################

us_states_sf <- states(class="sf", cb=TRUE)
us_counties_sf <- counties(class="sf", cb=TRUE)

################################
#'## Clean
################################

us_states_sf_c <- us_states_sf %>% 
  rename(State_FIPS = STATEFP,
         State = NAME,
         State_abb = STUSPS) %>% 
  select(starts_with("State"), geometry, -STATENS) %>% 
  arrange(State_FIPS) %>% 
  mutate(State_num = as.integer(State_FIPS)) %>% 
  mutate(State_status = case_when(State_FIPS=="11" | State_num>56 ~ "territory",
                                  TRUE ~ "state")) %>% 
  select(-State_num) %>% 
  select(State_FIPS, everything())

us_states_sf_c

## weird ones?
us_states_sf_c %>% 
  filter(State_status!="state")

################################
#'## Count counties per state
################################



state_cnty_count <- us_counties_sf %>% 
  mat_st_to_df() %>% 
  count(STATEFP, name = "State_n_cnty") %>% 
  rename(State_FIPS = STATEFP)

state_cnty_count

## add to state data
us_states_sf_c2 <- us_states_sf_c %>% 
  left_join(state_cnty_count, by = "State_FIPS")


################################
#'## Data only version
################################

us_states_df <- us_states_sf_c2 %>% 
  mat_st_to_df() %>% 
  mutate(State_name = State)

us_states_df


################################
#'## Export
################################

## READ AS: us_states_map <- read_rds("Data/US_maps/US_states_map.rds")

write_csv(us_states_df, "data_replicate/1_data_intermediate/US_states_dataOnly.csv")
