#' ---
#' description: "Plot all means of mobility measures"
#' author: Jo
#' run_final: TRUE
#' date: 2021-04-21
#' ---

my_packages <- c("tidyverse","magrittr")
sapply(my_packages, require, character.only = TRUE)

if(!require(matPkg)) remotes::install_github("MatthieuStigler/matPkg", upgrade = "never")

map<-purrr::map
select<-dplyr::select

source("code/auxiliary_scripts/general_options.R")

col_day = "#33658a"
col_weekend = "#f26419"  

#------------------------------------------------------------------------------------------------------------

df <- readRDS("data_replicate/2_data_final/merged_panel_did.rds")


#-----  Put outcome in long
outcome_vars <- readRDS("data_replicate/table_responses_names.rds") 


df_long <- df %>%
  group_by(Date) %>%
  summarize_if(is.numeric, ~mean(., na.rm=T)) %>%
  pivot_longer(c(outcome_vars$list_var), names_to = "Outcome") %>%
  left_join(outcome_vars, by = c("Outcome" = "list_var")) %>%
  select(Date, Source_name_signed,list_names,Outcome,value) %>%
  mutate(Day_type = weekdays(Date)) %>%
  mutate(Day_type = if_else(Day_type %in% c("Saturday","Sunday"),"Weekends","Weekdays")) 
  
## Now plot
pl_outcomes_levels <- ggplot(df_long, aes(x = Date, y = value )) +
  geom_line()+
  geom_point(aes(color = Day_type))+
  scale_color_manual(values = c("Weekdays" = col_day, "Weekends" = col_weekend))+
  facet_wrap(~Source_name_signed, scales = "free") +
  theme_jo_small+
  ylab("Outcome")+
  labs(color = "")  +
  facet_wrap(~Source_name_signed, scales = "free", ncol=4)+
  theme(strip.text.x = element_text(size = 10))

# pl_outcomes_levels

################################
#'## export
################################

ggsave(pl_outcomes_levels, width = 9, height = 7, dpi = 100,
       filename = "output_replicate/fig_1_plot_all_outcomes_levels.png")
