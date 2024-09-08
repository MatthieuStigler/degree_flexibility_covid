# "Clean Cuebiq county level". 

getwd()

my_packages <- c("tidyverse","magrittr")
#install.packages(my_packages, repos = "http://cran.rstudio.com")
lapply(my_packages, library, character.only = TRUE)

map<-purrr::map #For the nice iterations
select<-dplyr::select

source("code/auxiliary_scripts/general_options.R")

#---------------------------------------------------------------------------------------------------------------


df <- read_csv("Data/Cuebiq/UC_Davis_cmi-20200526.csv") #Confidential dataset, not shared

glimpse(df)
min(df$ref_dt)
max(df$ref_dt)

length(unique(df$county_fips_code))
length(unique(df$ref_dt))

check <- df %>%
  group_by(county_fips_code) %>%
  summarise(count= n())

table(check$count)


df_cleaned <- df %>%
  mutate(Date = as.Date(ref_dt)) %>%
  rename(County_name = county_name,
         State_name = state_name,
         FIPS = county_fips_code,
         cq_Mobility_index = cmi,
         cq_SIP = sheltered_in_place) %>%
  mutate(cq_Less_one_mile = less_1_mile + cq_SIP,
         cq_Less_ten_mile = less_10_mile + less_1_mile + cq_SIP) %>%
  mutate(cq_More_ten_mile = 1 - cq_Less_ten_mile) %>%
  select(-ref_dt,-week_name)
  
summary(df_cleaned)
hist(df_cleaned$cq_Less_one_mile)  
hist(df_cleaned$cq_Less_ten_mile)  
hist(df_cleaned$cq_More_ten_mile)  

saveRDS(df_cleaned, "data_replicate/1_data_intermediate/cuebiq_cleaned.rds")


