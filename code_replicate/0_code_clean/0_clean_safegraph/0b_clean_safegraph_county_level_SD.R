#' description: "Clean Safegraph county level"

#---------------------------------------------------------------------------------------------------------------
#---------- This code loads and clean the state-level aggregated SafeGraph Social Distancing Metrics data
#---------------------------------------------------------------------------------------------------------------

getwd()
my_packages <- c("tidyverse","magrittr","sf")
#install.packages(my_packages, repos = "http://cran.rstudio.com")
lapply(my_packages, library, character.only = TRUE)

map<-purrr::map 
select<-dplyr::select



#---------------------------------------------------------------------------------------------------------------


df <- readRDS("Code/clean_safegraph/clean_safegraph_county_level/aggregate_SD_county/safeGraph_SD_county.rds") #Starting Mar 21


glimpse(df)
min(df$date)
max(df$date)
summary(df$SDnew_medianPercHome)
summary(df$SD_average_bucketed_time_away_from_home)
quantile(df$device_count, seq(0,1,.1),na.rm = T)




df_final <- df %>%
  mutate(Away_min_three_hours_pct = (SD_partTimers+SD_fullTimers)/Devices_fips5) %>%
  dplyr::rename(FIPS = fips_5,
                Date = date,
                Device_count = Devices_fips5,
                Median_distance_traveled = SD_distHome,
                Median_home_dwell_time = SD_dwellHome,
                Completely_home_pct = SD_pctHome,
                Part_time_work_pct = SD_partTimersShare,
                Full_time_work_pct = SD_fullTimersShare,
                Median_not_home_dwell_time = SDnew_nonHomeDwell,
                Median_home_perc = SDnew_medianPercHome,
                Average_time_away = SD_average_bucketed_time_away_from_home #Created from buckets midpoints
                ) %>%
  mutate(Median_home_perc = Median_home_perc/100) %>%
  select(FIPS,Date,Device_count,Median_distance_traveled, Median_home_dwell_time, 
         Completely_home_pct, Part_time_work_pct, Full_time_work_pct,Away_min_three_hours_pct,
         Median_not_home_dwell_time,Median_home_perc,Average_time_away) 

glimpse(df_final)




#------------------------------------------------------------------------------------------------------------

df_final %>%
  count(FIPS) %>%
  count(n)

min(df_final$Date)
max(df_final$Date)


saveRDS(df_final,"code_replicate/0_code_clean/0_clean_safegraph/safegraph_county_SD.rds")




