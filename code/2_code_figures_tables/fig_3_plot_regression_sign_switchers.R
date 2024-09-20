#' ---
#' description: "Plot 2way sign switchers"
#' date: 2021-04-20
#' author: Jo
#' run_final: TRUE
#' ---


#rm(list = ls()) 
my_packages <- c("tidyverse","magrittr","lfe","stargazer", "broom", "future", "furrr", "Formula")
sapply(my_packages, require, character.only = TRUE)
sapply(my_packages, function(x) packageVersion(x) %>%  as.character)

source("code/auxiliary_scripts/general_options.R")

reg_2wayFE_Oct_coefs <- read_rds("data_replicate/3_data_analysis_output/2wayFE_coefs_all.rds")
table_vars <- read_rds("data_replicate/1_data_intermediate/vars_names_and_formulas/table_responses_names.rds")

table(reg_2wayFE_Oct_coefs$list_names)
table(reg_2wayFE_Oct_coefs$list_var)
table(reg_2wayFE_Oct_coefs$formu_legend)
table(reg_2wayFE_Oct_coefs$term)

reg_2wayFE_Oct_coefs %<>%
  left_join(table_vars)


all_controls <- reg_2wayFE_Oct_coefs %>%
  filter(formu_legend == "All policies, weather, cases and deaths dummy") %>%
  filter( !(term %in% c("Cases_dummy","Deaths_dummy","City_earliest_policy_pop_share","prcp","snow","tmean"))) %>%
  mutate(is_signif_5 = case_when(p_value < .05 ~ "Significant at 5% level",
                                 p_value > .05 ~ "Not significant at 5% level")) %>%
  mutate(is_positive = case_when(estimate > 0 ~ "Positive",
                                 estimate < 0 ~ "Negative",
                                 TRUE ~"ERROR")) %>%
  mutate(term_cleaned = str_replace(term,"_"," ")) %>% 
  mutate(term_cleaned = str_replace(term_cleaned,"Resclo","ERC")) %>% 
  mutate(term_cleaned = str_replace(term_cleaned,"SIP","SIPO")) %>% 
  mutate(term_cleaned = str_remove(term_cleaned,"Pol")) %>%
  mutate(Policy_and_log = if_else(has_log, paste0(term_cleaned, ", ","log"), paste0(term_cleaned, ", ","level")
                                  ))

table(all_controls$term)
table(all_controls$term_cleaned)
table(all_controls$Policy_and_log)





################################
#### ALter version
################################

## Add variables to df: case indicates the 4 possibilities
all_controls_alter <-  all_controls %>%
  mutate(is_signif_5_lgl =p_value < .05,
         transfo = if_else(has_log, "Log", "Level"),
         case = case_when(is_positive=="Negative" & is_signif_5_lgl ~"Negative, p-value < .05",
                          is_positive=="Negative" & !is_signif_5_lgl ~"Negative, p-value > .05",
                          is_positive=="Positive" & !is_signif_5_lgl ~"Positive, p-value > .05",
                          is_positive=="Positive" & is_signif_5_lgl ~"Positive, p-value < .05",
                          TRUE ~ "Error"))

all_controls_alter %>% 
  distinct(is_signif_5_lgl, is_positive, case)


## colors (order matters!!!)
cols <-  c("Negative, p-value < .05" = "red3",
           "Negative, p-value > .05" = alpha("red3", 0.35),
           "Positive, p-value > .05" = alpha("dodgerblue",0.35),
           "Positive, p-value < .05" = "dodgerblue")


# Plot now
p12 <- ggplot(all_controls_alter, aes(x = transfo, y = Source_name_signed)) +
  geom_point(aes(color = case), size = 7)+
  facet_grid(.~term_cleaned)+
  scale_color_manual(values = cols) +
  labs(color = "", fill = "")+
  theme_jo_small +
  ylab("Outcome") +
  xlab("Policy and outcome transformation") +
  theme(panel.grid.major = element_blank())


ggsave(p12, width = 7, height = 7, dpi=150,
       filename="output_replicate/fig_3_facet_coefs_sign_logs_levels_all_policies.png")
