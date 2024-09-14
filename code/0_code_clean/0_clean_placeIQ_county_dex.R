#' description: "Clean PlaceIQ county level"

my_packages <- c("magrittr","tidyverse","magrittr","data.table")
#install.packages(my_packages, repos = "http://cran.rstudio.com")
lapply(my_packages, library, character.only = TRUE)

map<-purrr::map 
select<-dplyr::select

#---------------------------------------------------------------------------------------------------------------


#Downloaded 2020-04-26  from "https://raw.githubusercontent.com/COVIDExposureIndices/COVIDExposureIndices/master/dex_data/county_dex.csv
 
dat <- fread("data_replicate/0_data_raw/county_dex.csv")

dat %<>%
  as_tibble %>%
  rename(FIPS = county,
         Date = date,
         Device_exposure = dex,
         Num_devices = num_devices,
         Device_exposure_a = dex_a,
         Num_devices_a = num_devices_a) %>%
  mutate(FIPS = as.character(FIPS)) %>%
  mutate(FIPS = if_else(nchar(FIPS)==4,paste0("0",FIPS),FIPS) ) %>%
  mutate(County_FIPS = FIPS, 
         Date = as.Date(Date))

#all_states <- tibble(State_abb = c(state.abb,"DC","PR"), State_name = c(state.name,"District Of Columbia","Puerto Rico"))

table(dat$FIPS)
table(dat$Date)
summary(dat)

saveRDS(dat, "data_replicate/1_data_intermediate/placeIQ_county_device_exposure.rds")
