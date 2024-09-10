#'---------------------------------------------------------------------------------------------------------------
#' description: "Downloaad from CSV"
#' date: 2020-04-26
#'---------------------------------------------------------------------------------------------------------------

my_packages <- c("tidyverse", "sf")
sapply(my_packages, require, character.only = TRUE)

map<-purrr::map #For the nice iterations
select<-dplyr::select

if(!require(matPkg)) remotes::install_github("MatthieuStigler/matPkg", upgrade = "never")
#-------------------------------------------- ----------------------------------------------------------------

source("code/auxiliary_scripts/888_misc_functions.R")

################################
#'## Read
################################
# raw data downloaded 2020-04-26 "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv?cachebust=911a386b6c9c230f",
gogMob_raw <- read_csv("data_replicate/0_data_raw/google_mobility_raw.csv", guess_max = 500000)
gogMob_raw
max(gogMob_raw$date)

#
us_counties_shp <- read_rds("data_replicate/1_data_intermediate/us_acs5_sf.rds")
us_states_df <- read_csv("data_replicate/1_data_intermediate/US_states_dataOnly.csv")


################################
#'## Clean
################################

gogMob_c <- gogMob_raw %>% 
  filter(country_region=="United States") %>% 
  select(-country_region_code, -country_region) %>% 
  rename(State_name =sub_region_1,
         County_name = sub_region_2) %>% 
  mutate(level = case_when(is.na(State_name) & is.na(County_name) ~ "Country",
                           !is.na(State_name) & is.na(County_name) ~ "State",
                           !is.na(State_name) & !is.na(County_name) ~ "County",
                           TRUE ~"ERROR"))

## Check if Count ok?!
gogMob_c %>% 
  distinct(State_name, County_name, level) %>% 
  count(level)

## We want only county!
gogMob_cnt <- gogMob_c %>% 
  filter(level=="County") %>% 
  select(-level)

gogMob_cnt

################################
#'## Cherck duplicates? no!
################################

gogMob_raw %>% 
  filter(country_region=="United States") %>% 
  add_count(sub_region_1, sub_region_2, date) %>% 
  filter(n>1) %>% 
  prj_check_0row()


################################
#'## Get County FIPS
################################

## county external list
us_counties_df <- us_counties_shp %>% 
  mat_st_to_df() %>% 
  select(FIPS, starts_with("State"), starts_with("County"))

us_counties_df

## Get list
cnt_list <- gogMob_cnt %>% 
  distinct(State_name, County_name)

states_list <- cnt_list %>% 
  distinct(State_name)

## first try merge states only?
merge_try <- states_list %>% 
  full_join(us_counties_df %>% 
              distinct_at(vars(starts_with("State"))))

## CHECK: any unmatched? OK!
merge_try %>% 
  filter_all(any_vars(is.na(.))) %>% 
  prj_check_0row()

## washington DC?
merge_try %>% 
  filter(str_detect(State_name, "ashing|istric|olumb"))

## Now try merge all
county_name_clean <- function(x) {
  str_remove_all(x, "County|Municipality|Census Area|City And Borough|Borough|Parish|City") %>% 
    str_trim() %>% 
    str_to_title()
}

add_county_name_clean <-  function(df, state=".") {
  df %>% 
    mutate(County_name_simple = if_else(str_detect(State_name, state),
                                        county_name_clean(County_name),
                                        str_to_title(County_name)))
}

cnt_list %>% 
  add_county_name_clean(state = "Alaska")


maybe_miss_city <- c("Baltimore", "St. Louis", "Alexandria", "Bristol", "Buena Vista", 
                     "Charlottesville", "Chesapeake", "Colonial Heights", "Covington", 
                     "Danville", "Emporia", "Fairfax", "Falls Church", "Franklin", 
                     "Fredericksburg", "Galax", "Hampton", "Harrisonburg", "Hopewell", 
                     "Lexington", "Lynchburg", "Manassas", "Manassas Park", "Martinsville", 
                     "Newport News", "Norfolk", "Norton", "Petersburg", "Poquoson", 
                     "Portsmouth", "Radford", "Richmond", "Roanoke", "Salem", "Staunton", 
                     "Suffolk", "Virginia Beach", "Waynesboro", "Williamsburg", "Winchester")

cnt_list_prep <- cnt_list %>% 
  add_county_name_clean(state = "Alaska") %>% 
  mutate(County_name_simple = case_when(County_name=="La Salle Parish" & State_name == "Louisiana" ~ "Lasalle Parish",
                                        County_name=="Shannon County" & State_name == "South Dakota" ~ "Oglala Lakota County",
                                        County_name %in% maybe_miss_city ~ paste(County_name, "City"),
                                        # County_name=="Doña Ana County" & State_name == "South Dakota" ~ "Oglala Lakota",
                                        TRUE ~County_name_simple))
us_counties_df_prep <- us_counties_df %>% 
  add_county_name_clean(state = "Alaska") %>% 
  mutate(County_name_simple = case_when(str_detect(County_name, "A Ana") & State_name == "New Mexico" ~ "Doña Ana County",
                                        TRUE ~County_name_simple))

merge_ct_stry <- cnt_list_prep  %>% 
  left_join(us_counties_df_prep, by = c("State_name", "County_name_simple"), 
            suffix = c("", "_fromOurMap"))

## CHECK: any unmatched? OK!
merge_ct_stry %>% 
  filter_all(any_vars(is.na(.))) 

## Check in each if problem
us_counties_df_prep %>% 
  filter(str_detect(County_name, regex("Charlottesville", ignore_case = TRUE)))

cnt_list_prep %>% 
  filter(str_detect(County_name, regex("Charlottesville", ignore_case = TRUE)))

################################
#'## Add codes for all counties now
################################


## colnames trimming?
colnames(gogMob_cnt)

## add now FIPS data
gogMob_cnt_clean <- gogMob_cnt %>% 
  left_join(merge_ct_stry %>% 
              select(-County_name_simple, -County, -State),
            by = c("State_name", "County_name")) %>% 
  select(-County_name) %>% 
  rename(County_name= County_name_fromOurMap) %>% 
  left_join(us_states_df %>% 
              select(State_FIPS, State_abb, State_name),
            by = "State_name") %>% 
  select(FIPS, County_name, State_name, State_FIPS, State_abb, everything()) %>% 
  rename_all(~str_remove(., "_percent_change_from_baseline")) %>% 
  rename(Date=date)

gogMob_cnt_clean

colnames(gogMob_cnt_clean)

## Long now
gogMob_cnt_clean_l <- gogMob_cnt_clean %>% 
  gather(Category_raw, Change, -Date, -FIPS, -starts_with("State"), -starts_with("County"))%>% 
  mutate(Category = str_to_title(Category_raw) %>% 
           str_replace("_and_", " & ") %>% 
           str_replace("_", " ") %>% 
           str_replace("Workplaces", "Workplace"))


################################
#'## Check duplicates
################################

cnt_w <- gogMob_cnt_clean %>% 
  add_count(FIPS, Date) %>% 
  filter(n>1) 

cnt_w %>% 
  select(FIPS, County_name, State_abb, Date, retail_and_recreation, grocery_and_pharmacy)

cnt_w %>% 
  distinct(County_name, State_name)


probs_df <- tibble(FIPS = c("24005", "24510", "29189", "29510", "51059", 
                            "51600", "51067", "51620", "51159", "51760", "51161", "51770"), 
                   County_name = c("Baltimore County", "Baltimore City", "St. Louis County", 
                                   "St. Louis City", "Fairfax County", "Fairfax City", "Franklin County", 
                                   "Franklin City", "Richmond County", "Richmond City", "Roanoke County", 
                                   "Roanoke City"),
                   State_name = c("Maryland", "Maryland", "Missouri", 
                                  "Missouri", "Virginia", "Virginia", "Virginia", "Virginia", "Virginia", 
                                  "Virginia", "Virginia", "Virginia"))

gogMob_cnt_clean %>% 
  semi_join(probs_df) %>% 
  distinct(FIPS, County_name, State_name) 


################################
#'## Export
################################

write_rds(gogMob_cnt_clean_l, "data_replicate/1_data_intermediate/google_mobility_clean.csv")
