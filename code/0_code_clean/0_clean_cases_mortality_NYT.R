#"Clean NYT cases and mortality data"


my_packages <- c("tidyverse","magrittr","sf")
#install.packages(my_packages, repos = "http://cran.rstudio.com")
lapply(my_packages, library, character.only = TRUE)

map<-purrr::map #For the nice iterations
select<-dplyr::select

first_day <- "2020/01/20"
last_day <- "2021/03/21"
#---------------------------------------------------------------------------------------------------------------

#downloaded March 2022, 2021 from https://github.com/nytimes/covid-19-data/archive/master.zip
nyt <- read_csv("data_replicate/0_data_raw/NYT_us-counties.csv") 

nyt %<>%
  rename(Date = date, FIPS = fips, Cum_cases = cases, Cum_deaths = deaths) %>%
  select(-county) %>%
  filter(!is.na(FIPS)) %>% #removing states
  mutate(state = str_to_title(state))

#Calculate instantaneous numbers
nyt_cleaned <- nyt %>%
  arrange(Date) %>%
  group_by(FIPS) %>%
  mutate(lag_cases = dplyr::lag(Cum_cases)) %>%
  mutate(lag_deaths = dplyr::lag(Cum_deaths)) %>%
  group_by(FIPS) %>%
  mutate(Date_first_reported_case = if_else(is.na(lag_cases), Date,as.Date(NA)) ) %>%
  mutate(Date_first_reported_case = max(Date_first_reported_case, na.rm = T) ) %>%
  mutate(Date_first_reported_death = if_else(Cum_deaths>0 & (lag_deaths<=0 | is.na(lag_deaths) ), Date, as.Date(NA)) ) %>%
  mutate(Date_first_reported_death = max(Date_first_reported_death, na.rm = T) ) %>%
  mutate(Days_since_first_case = if_else(!is.na(Date_first_reported_case), as.numeric(Date - Date_first_reported_case), as.numeric(NA)) )%>%
  mutate(Days_since_first_death = if_else(!is.na(Date_first_reported_death), as.numeric(Date - Date_first_reported_death), as.numeric(NA)) ) %>%
  ungroup() %>%
  mutate_if(is.numeric, ~ifelse(is.infinite(.),NA,.) ) %>%
  mutate(lag_cases = if_else(is.na(lag_cases),0,lag_cases), #first entry when first case is confirmed, so imputing 0 before
         lag_deaths = if_else(is.na(lag_deaths),0,lag_deaths)) %>%
  mutate(Cases = Cum_cases - lag_cases,
         Deaths = Cum_deaths - lag_deaths) %>%
  mutate(Cases = if_else(Cases<0,0,Cases), #self-corrections in the cumulative data lead to negative values, correcting
         Deaths = if_else(Deaths<0,0,Deaths)) %>%
  select(-lag_deaths,-lag_cases) %>%
  mutate(Date = as.Date(Date),
         Date_first_reported_death = if_else(is.infinite(Date_first_reported_death),as.Date(NA),Date_first_reported_death))#added June 10 
  


#--- checks
#only non-zero are entered, then stay in the panel
min(nyt_cleaned$Date)
max(nyt_cleaned$Date)
summary(nyt_cleaned)
length(unique(nyt_cleaned$FIPS))
nyt_cleaned %>% summarise_if(is.numeric,sum)
nyt_cleaned %>% filter(is.na(Date_first_reported_death)) %>% summarise_if(is.numeric,sum)
nyt_cleaned %>% count(FIPS) %>% count(n)
#nyt data does not keep track of counties with 0 cases, need to complete the panel

#-------- Create NYT panel

#---- Get correct county name
us_all <- readRDS("data_replicate/1_data_intermediate/us_acs5_sf.rds") %>%
  st_drop_geometry() %>%
  as_tibble() %>%
  select(FIPS,County_name,State_name)


completed_panel <- expand.grid(unique(us_all$FIPS),
                               seq(as.Date(first_day), to = as.Date(last_day), by = "day")) %>%
  as_tibble() %>%
  rename(FIPS = Var1, Date = Var2) %>%
  mutate(FIPS = as.character(FIPS), Date = as.Date(Date)) %>%
  left_join(nyt_cleaned,by = c("FIPS","Date"))


completed_panel %>% count(FIPS) %>% count(n)
as.numeric(as.Date(last_day)-as.Date(first_day))+1

completed_panel_2 <- completed_panel %>%
  replace_na(list(Cum_cases = 0, Cum_deaths = 0, Cases = 0, Deaths = 0)) %>%
  mutate(Cum_cases_sq = Cum_cases^2,
         Cases_sqrt = sqrt(Cases)) %>%
  mutate(Deaths_dummy = as.numeric(!is.na(Days_since_first_death) &Days_since_first_death>=0),
         Cases_dummy = as.numeric(!is.na(Days_since_first_case) &Days_since_first_case>=0)) %>%
  arrange(FIPS,Date) #%>%
  

summary(completed_panel_2)
rm(completed_panel)
saveRDS(completed_panel_2, "data_replicate/1_data_intermediate/cases_mortality_nyt.rds")


