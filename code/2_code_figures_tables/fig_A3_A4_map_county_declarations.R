#---------------------------------------------------------------------------------------------------------------
#---------- This code maps the declarations
#---------------------------------------------------------------------------------------------------------------

# my_packages <- c("rgeos","raster","tidyverse","magrittr","haven","readxl","tools","usethis","RColorBrewer","sf","tidycensus","tigris","lubridate","scales")
my_packages <- c("rgeos","raster","tidyverse","magrittr", "tools","RColorBrewer","sf", "lubridate","scales")
#install.packages(my_packages, repos = "http://cran.rstudio.com")
lapply(my_packages, library, character.only = TRUE)
library(ragg)
map<-purrr::map 
select<-dplyr::select

source("code/auxiliary_scripts/general_options.R")
#-------------------------------------------- ----------------------------------------------------------------

declarations <- readRDS("data_replicate/counties_declarations_cleaned.rds")



# Minor cleaning
declarations %<>%
  mutate(County_ED_int = as.numeric(County_ED)) %>%
  mutate(County_ED_int = County_ED_int - min(County_ED_int, na.rm = T)+1) %>%
  mutate(County_SIP_int = as.numeric(County_SIP)) %>%
  mutate(County_SIP_int = County_SIP_int - min(County_SIP_int, na.rm = T)+1) 


check <- declarations %>%
  mutate(check = ifelse(County_SIP!=County_BC,1,0)) 


us.sf <- readRDS("data_replicate/1_data_intermediate/us_acs5_sf.rds")

us.sf_all <- us.sf

declarations %<>%
  mutate(State_FIPS = str_sub(FIPS,1,2))

us.sf %<>%
  mutate(State_FIPS = str_sub(GEOID,1,2))

unique(declarations$State_FIPS)
unique(us.sf$State_FIPS)[ !(unique(us.sf$State_FIPS) %in% unique(declarations$State_FIPS))]

message("will need to add connecticut, Rhode Island and Puerto Rico later on")


counties_no_data <- us.sf_all %>%
  filter(!(GEOID %in% unique(declarations$FIPS))) %>%
  mutate(Miss = "No county government")


#----- merge
merged <- us.sf %>%
  left_join(declarations, by = c("FIPS", "County_name", "State_name", "State_FIPS")) %>%
  mutate(County_ED_month = month(County_ED)) %>%
  mutate(County_ED_week = case_when(County_ED < as.Date("2020-03-11") ~ "February 14 - March 10", #WHO declares pandemic
                                    County_ED >= as.Date("2020-03-11") & County_ED < as.Date("2020-03-16") ~ "March 11 - 15",
                                    County_ED >= as.Date("2020-03-16") & County_ED < as.Date("2020-03-21") ~ "March 16 - 20",
                                    County_ED >= as.Date("2020-03-21") & County_ED < as.Date("2020-04-01") ~ "March 21 - 31",
                                    County_ED >= as.Date("2020-04-01") ~ "April - June 11",
                                    !(GEOID %in% unique(declarations$FIPS)) ~ "No county government",
                                    TRUE ~ as.character(County_ED)
                                    )
         ) %>%
  mutate(County_ED_week = factor(County_ED_week, levels = c("February 14 - March 10","March 11 - 15",
                                                               "March 16 - 20","March 21 - 31","April - June 11",
                                                            "No county government")))


#---- Counts for paper

counts_for_paper <- merged %>% 
  st_drop_geometry() %>%
  filter(!is.na(County_ED)|!is.na(County_SIP))

nrow(counts_for_paper)/nrow(merged)
sum(counts_for_paper$Population_county)/sum(merged$Population_county)

#------- County SIP binning

merged %<>%
  mutate(County_SIP_week = case_when(County_SIP < as.Date("2020-03-23") ~ "March 16 - 22", 
                                    County_SIP >= as.Date("2020-03-23") & County_SIP < as.Date("2020-03-30") ~ "March 23 - 29",
                                    County_SIP >= as.Date("2020-03-30") & County_SIP < as.Date("2020-04-05") ~ "March 30 - April 4",
                                    County_SIP >= as.Date("2020-04-05") & County_SIP <= as.Date("2020-04-30") ~ "April 4 - June 1",
                                    !(GEOID %in% unique(declarations$FIPS)) ~ "No county government",
                                    TRUE ~ as.character(County_SIP))
  ) %>%
  mutate(County_SIP_week = factor(County_SIP_week, levels = c("March 16 - 22","March 23 - 29",
                                                            "March 30 - April 4","April 4 - June 1",
                                                            "No county government")))
table(merged$County_SIP_week)
#-------------------------------------------- plot ------------------------------------------------------------

library("viridis")
palette_v <- viridis(5, option = 'viridis')
palette_v <- c(palette_v,"grey60")
breaks_col <- c("February 14 - March 10","March 11 - 15",
                "March 16 - 20","March 21 - 31","April - June 11",
                "No county government")

size_lines = 0.025

mainland <- ggplot(data = merged) + 
  geom_sf( aes(fill = County_ED_week), size = size_lines) +
  scale_fill_manual(values = palette_v, breaks = breaks_col, na.value = "grey93")+
  coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), ylim = c(-2300000,730000)) + #contiguous US only
  labs(fill = "Emergency Declaration ",
       color = "")+
  #guides(fill = FALSE, color = FALSE)+
  theme_jo_small +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="bottom",
        legend.key.width = unit(2, "line"))

  
# mainland
ggsave(mainland, filename= "output_replicate/fig_A3_map_county_emergency_declarations.png", 
       width = 17, height = 13, units = "cm",dpi = 150 )





#-------------------------------------------- Shelter in place ----------------------------------------------------------------
palette_v <- viridis(4, option = 'viridis')
palette_v <- c(palette_v,"grey60")
breaks_col <- c("March 16 - 22","March 23 - 29",
                "March 30 - April 4","April 4 - June 1",
                "No county government")

mainland_SIP <- ggplot(data = merged) + 
  geom_sf( aes(fill = County_SIP_week), size = size_lines) +
  scale_fill_manual(values = palette_v, breaks = breaks_col, na.value = "grey93")+
  
  coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), ylim = c(-2300000,730000)) + #contiguous US only
  labs(fill = "Shelter-in-Place ",
    color = "")+
  theme_jo_small +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="bottom",
        legend.key.width = unit(2, "line"))

# mainland_SIP

ggsave(mainland_SIP, width = 17, height = 13, units = "cm",dpi = 160,
       filename ="output_replicate/fig_A4_map_county_sip.png")

