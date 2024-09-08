 #---------------------------------------------------------------------------------------------------------------
#---------- This code loads and clean the counties presidential elections 
#---------------------------------------------------------------------------------------------------------------

my_packages <- c("tidyverse","magrittr")
#install.packages(my_packages, repos = "http://cran.rstudio.com")
lapply(my_packages, library, character.only = TRUE)

map<-purrr::map 
select<-dplyr::select

#-----------------------------------------------------------------------------------------------------------

#from MIT elecions lab, https://electionlab.mit.edu/data
elections <- read_csv("data_replicate/0_data_raw/countypres_2000-2016.csv")

glimpse(elections)

elections %<>%
  rename(Year = year, Office = office, Candidate = candidate, Party = party,
         Candidate_votes = candidatevotes, Total_votes = totalvotes,
         State_abb = state_po) %>%
  select(-county)

elections %<>%
  mutate(state = str_to_title(state))


elections %<>%
  filter(Year == 2016) %>%
  mutate(Share_votes = Candidate_votes/Total_votes) 

elections %<>%
  mutate(FIPS = as.character(FIPS)) %>%
  mutate(FIPS = ifelse(nchar(FIPS)==4, paste0("0",FIPS),FIPS)) %>%
  select(-Party) %>%
  filter(Candidate == "Donald Trump") %>%
  rename("Share_votes_Trump" = Share_votes)

elections %<>%
  mutate(Trump_landslide = ifelse(Share_votes_Trump>=.60,1,0)) %>%
  mutate(Clinton_landslide = ifelse(Share_votes_Trump<=.40,1,0))

unique(elections$Candidate)
table(nchar(elections$FIPS))

saveRDS(elections, "data_replicate/1_data_intermediate/county_pres_elections_2016.rds")
