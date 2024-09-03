# "Clean map US counties and vars ACS5"


my_packages <- c("rgeos","raster","tidyverse","magrittr","haven","readxl","tools","usethis","sf","tidycensus","tigris","units")
#install.packages(my_packages, repos = "http://cran.rstudio.com")
lapply(my_packages, library, character.only = TRUE)

map<-purrr::map #
select<-dplyr::select


#census_api_key("PUT KEY HERE",install = TRUE)
options(tigris_use_cache = TRUE)

#--------------------------------------------  ----------------------------------------------------------------



vars_to_get <- c("B01001_001","B01002_001","B01002_001",
                 "B02001_001","B02001_002","B02001_003","B03003_001","B03003_002","B03003_003",
                 "B08101_001","B08101_025",
                 "B08101_049",
                 "B19326_001","B19013_001") 

# Download data
us.df <- get_acs(geography = "county", 
                 variables = vars_to_get,
                 year = 2018,
                 geometry = FALSE)


us.df_cleaned <- us.df %>%
  select(-moe) %>%
  pivot_wider(names_from = variable, values_from = estimate)

rm(us.df)
summary(us.df_cleaned$B19326_001)

#-------- Generate variables

us.df_cleaned %<>%
  mutate(White_pct = B02001_002/B02001_001,
         Black_pct = B02001_003/B02001_001,
         Hispanic_pct = B03003_003/B03003_001,
         Public_transport_to_work_pct = B08101_025/B08101_001,
         Worked_at_home_pct = B08101_049/B08101_001         )

us.df_cleaned %>% mutate(diff = if_else(B02001_001==B03003_001,1,0)) %>% pull(diff) %>% min

summary(us.df_cleaned)
#-------------------------- Create quantiles income --------------------------

summary(us.df_cleaned$B19326_001)

quantiles_counties <- quantile(us.df_cleaned$B19326_001,seq(0,1,.2), na.rm = T)
quantiles_labels <- labels(quantiles_counties)

#rename
for (i in 1:(length(quantiles_counties)-1)) {
  quantiles_labels[i] = paste0(quantiles_labels[i],"_",quantiles_labels[i+1]) #get classes right
}
quantiles_labels <- quantiles_labels[-length(quantiles_labels)] #remove last one

us.df_cleaned %<>%
  mutate(Quantile_group = base::cut(B19326_001, breaks = quantiles_counties, labels = quantiles_labels, include.lowest = T) ) %>%
  mutate(Medincome_quintile = Quantile_group) #do a proper renaming later when rest of code is cleaned

table(us.df_cleaned$Quantile_group)
table(us.df_cleaned$Medincome_quintile)
rm(quantiles_counties,quantiles_labels)

#----------- Deciles

quantiles_counties <- quantile(us.df_cleaned$B19326_001,seq(0,1,.1), na.rm = T)
quantiles_labels <- labels(quantiles_counties)

#rename
for (i in 1:(length(quantiles_counties)-1)) {
  quantiles_labels[i] = paste0(quantiles_labels[i],"_",quantiles_labels[i+1]) #get classes right
}
quantiles_labels <- quantiles_labels[-length(quantiles_labels)] #remove last one

us.df_cleaned %<>%
  mutate(Decile_income_group = base::cut(B19326_001, breaks = quantiles_counties, labels = quantiles_labels, include.lowest = T )) %>%
  mutate(Medincome_decile = Decile_income_group)

table(us.df_cleaned$Decile_income_group)
table(us.df_cleaned$Medincome_decile)
rm(quantiles_counties,quantiles_labels)

#------- Function

generate_tercile <- function(x, new_var_name, df = us.df_cleaned) {
  
  x_vector <- eval(substitute(x), df)
  
  terc <- quantile(x_vector, seq(0,1,.33333333), na.rm = T)
  terc[length(terc)] <- terc[length(terc)] + 1 #So that the max is included (the problem is terciles is that we cut at 99.999%)
  
  df_augmented <- df %>% mutate(!!new_var_name := base::cut({{x}}, 
                                                            breaks = terc, 
                                                            labels = c("0%_33%","33%_66%","66%_99%"), 
                                                            include.lowest = T ))
  
  df_augmented
  
}

#-------- Terciles for other variables
us.df_cleaned <- generate_tercile(B19326_001, new_var_name = "Medincome_tercile")
us.df_cleaned <- generate_tercile(White_pct, new_var_name = "White_pct_tercile")
us.df_cleaned <- generate_tercile(Black_pct, new_var_name = "Black_pct_tercile")
us.df_cleaned <- generate_tercile(Hispanic_pct, new_var_name = "Hispanic_pct_tercile")
us.df_cleaned <- generate_tercile(Worked_at_home_pct, new_var_name = "Work_at_home_pct_tercile")
us.df_cleaned <- generate_tercile(B01002_001, new_var_name = "Median_age_county_tercile")
us.df_cleaned <- generate_tercile(B19013_001, new_var_name = "Medincome_hh_tercile")


table(us.df_cleaned$Medincome_tercile)
table(us.df_cleaned$White_pct_tercile)
table(us.df_cleaned$Black_pct_tercile)
table(us.df_cleaned$Hispanic_pct_tercile)
table(us.df_cleaned$Work_at_home_pct_tercile)
table(us.df_cleaned$Median_age_county_tercile)
table(us.df_cleaned$Medincome_hh_tercile)

us.df_cleaned %<>%
  dplyr::rename(Median_income_county = B19326_001,
                Population_county = B01001_001,
                Medincome_hh_county = B19013_001,
                Median_age_county = B01002_001)

names(us.df_cleaned)

us.df_cleaned %<>%
  select(-starts_with("B0"))
#-----------

us.sf <- get_acs(geography = "county", 
                 variables = "B19326_001",
                 geometry = TRUE)

us.sf %<>% #May need to reproject
  select(-variable,-estimate,-moe) %>%
  mutate(Area = st_area(us.sf)) %>%
  mutate(Area = set_units(Area, km^2) )

us.sf %<>% 
  left_join(us.df_cleaned, by = c("GEOID", "NAME")) 


us.sf %<>%
  separate(NAME, sep = ",", into = c("County","State"), remove = FALSE) %>%
  mutate(State = str_squish(State),
         County = str_squish(County)) %>%
  mutate(County = str_to_title(County)) %>%
  mutate(State = str_to_title(State)) %>%
  select(-NAME)

glimpse(us.sf)

us.sf %<>%
  mutate(FIPS = GEOID,
         County_name = County,
         State_name = State) %>%
  mutate(Density_pop = as.numeric(Population_county/Area) )

#------------- Create density quantiles
us.sf <- generate_tercile(Density_pop, new_var_name = "Density_pop_tercile", df = us.sf)

table(us.sf$Density_pop_tercile)
summary(us.sf)

#------------- Save

saveRDS(us.sf,"code_replicate/0_code_clean/0_clean_map_us_counties/us_acs5_sf.rds")


