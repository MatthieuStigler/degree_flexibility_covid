#' ---
#' description: "State declarations over time"
#' author: Matthieu
#' date: 2020-04-11
#' run_final: TRUE
#' ---

my_packages <- c("tidyverse", "sf", "ggrepel")
lapply(my_packages, library, character.only = TRUE)

map<-purrr::map 
select<-dplyr::select
if(!require(matPkg)) remotes::install_github("MatthieuStigler/matPkg", upgrade = "never")
source("code/auxiliary_scripts/general_options.R")
#-------------------------------------------- ----------------------------------------------------------------

################################
#'## Read data
################################

declar_stat_old <- readRDS("data_replicate/0_data_raw/clean_states_declarations.rds")
declar_counties <- readRDS("data_replicate/declarations_counties_states_dates.rds")
us_states_df <- read_csv("data_replicate/1_data_intermediate/US_states_dataOnly.csv")
table_covars <- read_rds("data_replicate/table_covariates_reg.rds")

## county dat for state pop
cnt_dat <- read_rds("data_replicate/1_data_intermediate/us_acs5_sf.rds") %>% 
  mat_st_to_df()


################################
#'## Prepare
################################

## declar_counties: add pop
declar_counties_pop <- declar_counties %>% 
  left_join(cnt_dat %>% 
              select(FIPS, Population_county), by = "FIPS")

# CHECK: unmatched? Ok!
declar_counties_pop %>% 
  filter(is.na(Population_county)) %>% 
  mat_check_0row()

################################
#'## County to state
################################

declar_stat <- declar_counties %>% 
  select(starts_with("State")) %>% 
  distinct() 

## makre sure no duplis?!
declar_stat %>% 
  add_count(State_name,State_abb) %>% 
  filter(n>1) %>% 
  mat_check_0row()



################################
#'## State pop
################################

dat_pop <- cnt_dat %>% 
  group_by(State_name) %>% 
  summarise(population=sum(Population_county)) %>% 
  ungroup() %>% 
  mutate(population_perc=100*population/sum(population)) %>% 
  arrange(desc(population_perc)) %>% 
  mutate(State_name=if_else(State_name=="District Of Columbia",
                            "District of Columbia",
                            State_name)) %>% 
  left_join(us_states_df %>% 
              select(State_name, State_abb), by = c("State_name"))

dat_pop 

## CHECK: all there? OK (after correcting DC)
dat_pop %>% 
  filter(is.na(State_abb)) %>% 
  mat_check_0row()


## add to declar state
declar_stat_pop <- declar_stat %>% 
  left_join(dat_pop %>% 
              select(-State_name), by = c("State_abb"))

## CHECK: all merged? OK
declar_stat_pop %>% 
  filter(is.na(population)) %>% 
  mat_check_0row()



################################
#'## Clean state
################################

declar_stat_l <- declar_stat_pop %>% 
  rename_all(~str_replace(., "SAH", "SIP")) %>% 
  select(State_name, State_abb, matches("^State_(ED$|BC$|SIP$|SIP_BC)|StatePol"), starts_with("population")) %>% 
  gather(Declaration, Date, matches("^State_(ED|SIP|BC)|StatePol")) %>% 
  filter(!is.na(Date)) %>% 
  left_join(us_states_df %>% 
              select(State_abb, State_n_cnty), by = "State_abb") 

declar_stat_l %>% 
  count(Declaration)


################################
#'## Counties
################################

declar_counties_pop %>% 
  filter(is.na(Population_county))


counties_cumsum <- declar_counties_pop %>% 
  select(FIPS, County_ED, County_SIP, Population_county) %>% 
  mutate(pop_perc=100* Population_county/sum(Population_county, na.rm=TRUE)) %>% 
  gather(Declaration, date,  starts_with("County_")) %>% 
  filter(!is.na(date)) %>% 
  filter(!is.na(pop_perc)) %>% 
  arrange(date) %>% 
  group_by(date, Declaration) %>% 
  summarise(pop_perc =sum(pop_perc)) %>% 
  ungroup() %>% 
  group_by(Declaration) %>% 
  mutate(pop_perc =cumsum(pop_perc)) %>% 
  ungroup()

counties_cumsum

## last vals?
counties_cumsum %>% 
  group_by(Declaration) %>% 
  slice_tail()


################################
#'## State cumsum
################################

## 
dates_states <- declar_stat_l %>% 
  group_by(Declaration, Date) %>% 
  arrange(Date) %>% 
  summarise(n_states = n(),
            population_perc=sum(population_perc)) %>% 
  ungroup() %>% 
  group_by(Declaration) %>% 
  mutate(n_states_cumul = cumsum(n_states),
         population_cumul=cumsum(population_perc)) %>% 
  ungroup() 

dates_states


move_ypos <- function(df, decl, date, n_above, value=2.5){
  date <- as.Date(date)
  df %>% 
    mutate(pos_y=  case_when(Declaration ==decl & Date == date & pos<=n_above~pos_y+(n_above+2)*value,
                             Declaration ==decl & Date == date~pos_y+(n_above)*value,
                             TRUE ~pos_y))
}
#
repel_manual <- dates_states %>%
  select(Declaration, Date, n_states_cumul) %>%
  left_join(declar_stat_l %>%
              select(State_abb, Date, Declaration),
            by = c("Date", "Declaration")) %>%
  group_by(Declaration, Date) %>%
  mutate(n_this_day = n(),
         pos = 1:n(),
         pos_rel = 100* pos/n()) %>%
  ungroup() %>%
  mutate(pos_y = n_states_cumul -pos*if_else(Date<as.Date("2020-03-08"), 2.5, 2.9)) %>% 
  move_ypos("StatePol_Resclo", "2020-03-12", 6) %>% 
  move_ypos("StatePol_Resclo", "2020-03-13", 7) %>% 
  move_ypos("StatePol_SIP", "2020-03-23", 3) %>% 
  mutate(pos_y2 = pmax(pos_y, -2))

repel_manual %>% 
  count(Declaration, Date) %>% 
  filter(Date < as.Date("2020-03-25"))




state_cum_plot <- function(df=dates_states, df_pos=repel_manual, size = 2, geom = geom_text,
                           y_var=n_states_cumul, add_label=TRUE, ...) {
  res <-    df%>% 
    ggplot(aes(x = Date, y={{y_var}})) +
    geom_line() +
    geom_point() 
  
  if(add_label){
    res <- res+
      geom(aes(x=Date, y = pos_y, label = State_abb),
           data=df_pos, size = size, ...)
  }
  #ggtitle("Timing of state emergency decision") +
  
  res+
    ylab("Number of states (cumulated)") +
    theme_jo77
}

## Standard: 3 dec
pl_n_states_cumul <- dates_states %>% 
  filter(str_detect(Declaration, "StatePol")) %>% 
  state_cum_plot(df_pos=repel_manual %>% 
                   filter(str_detect(Declaration, "StatePol")),
                 size =1.5)+
  facet_wrap(Declaration~., nrow=3, labeller = as_labeller(policy.facet.lab)) +
  scale_y_continuous(breaks = seq(0, 50, by = 25))

# pl_n_states_cumul


## Talk: Emergency and SIP
pl_n_states_2 <- dates_states %>% 
  filter(Declaration!="StatePol_Resclo") %>%
  filter(str_detect(Declaration, "StatePol")) %>% 
  state_cum_plot(df_pos=repel_manual %>% 
                   filter(str_detect(Declaration, "StatePol") & Declaration!="StatePol_Resclo"),
                 size =1.5)+
  facet_wrap(Declaration~., nrow=3, labeller = as_labeller(policy.facet.lab)) +
  scale_y_continuous(breaks = seq(0, 50, by = 25))

# pl_n_states_2



## 3, no facet
pl_n_states_cumul_noFac <- dates_states %>% 
  filter(str_detect(Declaration, "StatePol")) %>% 
  state_cum_plot(add_label=FALSE)+
  aes(group=Declaration, color=Declaration)

# pl_n_states_cumul_noFac


## new: 2 dec, use SIP_BC
pl_n_states_cumul_2Dec <- dates_states %>% 
  filter(Declaration%in%c("StatePol_SIP", "StatePol_ED","StatePol_Resclo")) %>% 
  state_cum_plot(df_pos=repel_manual %>% 
                   filter(Declaration%in%c("StatePol_SIP", "StatePol_ED","StatePol_Resclo")))+
  facet_wrap(Declaration~., nrow=2, labeller = as_labeller(policy.facet.lab))

# pl_n_states_cumul_2Dec

################################
#'## State separately
################################


repel_manual_SIP <- repel_manual %>% 
  filter(Declaration=="State_SIP") %>% 
  mutate(pos_y = n_states_cumul  - 1.8* pos)

pl_n_states_cumul_SIP <- state_cum_plot(df=dates_states %>% 
                                          filter(Declaration=="State_SIP"), 
                                        df_pos=repel_manual_SIP,
                                        size = 3, geom = geom_text)+
  facet_wrap(Declaration~., nrow=3)

# pl_n_states_cumul_SIP


## BC
repel_manual_BC <- repel_manual %>% 
  filter(Declaration=="State_BC") %>% 
  mutate(pos_y = n_states_cumul  - 1* pos)

pl_n_states_cumul_BC <- state_cum_plot(df=dates_states %>% 
                                         filter(Declaration=="State_BC"), 
                                       df_pos=repel_manual_BC,
                                       size = 5, geom = geom_label)+
  facet_wrap(Declaration~., nrow=3)

# pl_n_states_cumul_BC

## ED
repel_manual_ED <- repel_manual %>% 
  filter(Declaration=="State_ED") %>% 
  mutate(pos_y = n_states_cumul  - 3.3* pos)

pl_n_states_cumul_ED <- state_cum_plot(df=dates_states %>% 
                                         filter(Declaration=="State_ED"), 
                                       df_pos=repel_manual_ED,
                                       size = 1.8, geom = geom_label,
                                       label.padding =  unit(0.13, "lines"))+
  facet_wrap(Declaration~., nrow=3)

# pl_n_states_cumul_ED



################################
#'## With population instead
################################

repel_pop <- dates_states %>%
  select(Declaration, Date, n_states_cumul, population_cumul) %>%
  left_join(declar_stat_l %>%
              select(State_abb, Date, Declaration),
            by = c("Date", "Declaration")) %>%
  group_by(Declaration, Date) %>%
  mutate(n_this_day = n(),
         pos = 1:n(),
         pos_rel = 100* pos/n()) %>%
  ungroup() %>%
  mutate(pos_y = population_cumul -pos*if_else(Date<as.Date("2020-03-08"), 6, 7),
         pos_y2 = pmax(pos_y, -2)) %>% 
  move_ypos("StatePol_Resclo", "2020-03-12", 6, value=5) %>% 
  move_ypos("StatePol_Resclo", "2020-03-13", 7, value=5) %>% 
  move_ypos("StatePol_SIP", "2020-03-23", 3, value=5) %>% 
  filter(Declaration%in%c("StatePol_SIP", "StatePol_ED", "StatePol_Resclo"))

# repel_pop

pl_pop_cumul <- dates_states %>% 
  filter(Declaration%in%c("StatePol_SIP", "StatePol_Resclo","StatePol_ED")) %>% 
  state_cum_plot(df_pos=repel_pop,
                 y_var = population_cumul)+
  facet_wrap(Declaration~., nrow=3, labeller = as_labeller(policy.facet.lab))+
  ylab("Population share (cumulated)")+
  mat_gg_scale_Y_isperc+
  ggtitle("Timing of state emergency decision (by pop)")

# pl_pop_cumul

last_state_date <- range(dates_states$Date)

## add county
counties_cumsum_adj <- counties_cumsum %>% 
  mutate(Declaration= case_when(Declaration=="County_SIP" ~"StatePol_SIP", 
                                Declaration=="County_ED" ~"StatePol_ED",
                                TRUE~"ERROR"))
pl_pop_cumul_county <- pl_pop_cumul +
  geom_line(aes(x=date, y=pop_perc), data=counties_cumsum_adj %>% 
              filter(between(date, last_state_date[1], last_state_date[2])),
            linetype=2, alpha=0.4)

# pl_pop_cumul_county

################################
#'## Export
################################


## 3 Declarations
ggsave(pl_n_states_cumul, 
       filename = "output_replicate/fig_A1_state_declar_cumsum_3Dec.png", width = 7, height = 5)


