# "Clean weather data"


my_packages <- c("tidyverse","magrittr","haven","tidycensus","tigris","matPkg")
#install.packages(my_packages, repos = "http://cran.rstudio.com")
lapply(my_packages, library, character.only = TRUE)

map<-purrr::map
select<-dplyr::select


#-----------------------------------------------------------------------------------------------------------

weather <- read_dta("data_replicate/0_data_raw/DAILY_WEATHER_IDW200_2020_v2.dta") 

check <- weather %>% filter(is.na(Tmean))
unique(check$fips) #22055 28127 22113


weather %<>%
  mutate(FIPS = as.character(fips)) %>%
  mutate(FIPS = if_else(nchar(FIPS)==4,paste0("0",FIPS),FIPS)) %>%
  mutate(Date = as.Date(paste0(Year,"-",Month,"-",Day))) %>%
  rename(tmean = Tmean, tmin = Tmin, tmax = Tmax, prcp = Prcp, snow = Snow) #not using strtolower as a check

#checks
min(weather$Date)
max(weather$Date)
table(nchar(weather$FIPS))
weather %>% filter(Date == as.Date("2020-02-01"))

weather %<>%
  select(-fips,-Year,-Month,-Day)

saveRDS(weather, "data_replicate/1_data_intermediate/weather_cleaned.rds")

