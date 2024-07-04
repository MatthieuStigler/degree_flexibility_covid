#' ---
#' description: "Standard event studies, all outcomes, all specs and F tests"
#' runMat: FALSE
#' run_final: TRUE
#' date: 2021-04-21
#' ---


my_packages <- c("tidyverse","magrittr", "broom","cowplot","data.table","car")
library(viridis)

sapply(my_packages, require, character.only = TRUE)

if(!require(matPkg)) remotes::install_github("MatthieuStigler/matPkg", upgrade = "never")

map<-purrr::map #For the nice iterations
select<-dplyr::select

source("code_replicate/auxiliary_scripts/888_event_code.R")
source("code_replicate/auxiliary_scripts/general_options.R")


################################
#'## Read data
################################

tidy_all_cleaned <- readRDS("data_replicate/all_regs_tidy_binded.rds")


################################
#'## Prep data
################################

F_tests1 <- tidy_all_cleaned %>% 
  select(Outcome_var,Source,Ftest,Policies_in_regression_f,specification,Weights,dep_var) %>%
  distinct() %>%
  mutate(specification_weighted = if_else(Weights,paste0(specification,", weighted"),specification)) %>%
  filter(Source!="Google")

table(F_tests1$Outcome_var)
table(F_tests1$specification)
table(F_tests1$specification_weighted)
F_tests1 %>% count(Outcome_var) %>% count(n)
F_tests1 %>% count(Outcome_var) %>% count(n)


## Summarise F tests: % of rejected (i.e. no // teends!)
F_tests <- F_tests1 %>%
  mutate(pval_05 = if_else(Ftest < 0.05, 1,0)) %>%
  mutate(pval_01 = if_else(Ftest < 0.1, 1,0)) %>%
  #mutate(Policies_in_regression_f = factor(Policies_in_regression, 
  #                                         levels = c("County ED", "County SIP", "State ED", 
  #                                                    "State SIP","State ERC", "Simultaneous policies"))) %>%
  group_by(Policies_in_regression_f, specification_weighted) %>%
  summarise(Count_p_values_05 = sum(pval_05),
            Count = n()) %>%
  mutate(Share_p_values_05_c = as.character(round(Count_p_values_05/Count,2)), 
         Count_p_values_05_c = as.factor(Count_p_values_05) ) %>% 
  ungroup()
  
F_tests

sum(F_tests$Count_p_values_05)

# F_tests %>% count(Outcome_var) %>% count(n) #6
# F_tests %>% count(specification) %>% count(n) #6
F_tests %>% count(specification_weighted) %>% count(n) #6


################################
#'## Plot: heatmap
################################


sum(F_tests$Count)
sum(F_tests$Count_p_values_05)
sum(F_tests$Count)-sum(F_tests$Count_p_values_05)


pl_heatmap_Ftests <- F_tests %>% 
  mutate(Share_p_values_05_c = paste0(round(100*Count_p_values_05/Count), "%")) %>% 
  ggplot(aes(Policies_in_regression_f, specification_weighted, fill= Count_p_values_05_c)) + 
  geom_tile() +
  geom_text(aes(label = Share_p_values_05_c))+
  viridis::scale_fill_viridis(discrete = TRUE,alpha = .8) +
  ylab("Outcome transformation") + 
  xlab("Policies in regression") +
  labs(fill = "Count p-values < .05: ") +
  theme_jo77+
  theme(axis.text.x=element_text(angle=45,hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  guides(fill=guide_legend(nrow=1))
  # scale_x_discrete(guide = guide_axis(n.dodge = 2))
  
# pl_heatmap_Ftests

ggsave(pl_heatmap_Ftests, height = 7, width = 7, dpi = 150,
       filename = "output_replicate/fig_A15_heatmap_all_outcomes_transformations_Ftests.png")

################################
#'## Plots: event plots
################################
  
  
#--- Only one outcome: Completely home
completely_home <- tidy_all_cleaned %>%
  filter(str_detect(dep_var, "Completely")) %>%
  filter(!is.na(Treatment_var)) %>%
  mutate(Outcome_var_m = str_trim(str_remove_all(Outcome_var, "Completely|completely|Home|home")),
         transfo_short = str_extract(Outcome_var_m, "\\(.+\\)"),
         weight_name = if_else(Weights, "Weighted", "No w.")) 
  
table(completely_home$Treatment_var)  
table(completely_home$Outcome_var_m)  
table(completely_home$transfo_short)  
table(completely_home$Weights)  
table(completely_home$Policies_in_regression_f)  


#---------------- Plot Completely home ----------------
  
library(RColorBrewer)
library(ggh4x)
my_palette <- brewer.pal(name="Set1",n=5)#[-c(6,7)]
my_palette <- c(my_palette,"#000000")

pl_event_comp_home <-  evt_plot(df = completely_home, 
         color = Policies_in_regression_f, errorbar = TRUE) + 
  # facet_grid(Outcome_var_m~Treatment_var, scales = "free") + #, fac2_var = Regression) 
  facet_nested(transfo_short +weight_name~Treatment_var, scales = "free",
               nest_line = element_line(colour = "black", linetype = 1)) + #, fac2_var = Regression) 
  labs(color = "Policy")+
  #geom_point(data = completely_home,x = -1, y = 0, color = "black", alpha = .5)+
  scale_color_manual(values = my_palette)+
  theme(strip.text.y = element_text(size = 10))
  

# pl_event_comp_home

ggsave(pl_event_comp_home,
       filename = "output_replicate/fig_4_completely_home_events_4_transformations_weighting_simultaneous.png", 
       height = 10, width = 10, dpi = 80)
  
  


#======== Standard event study plots, all outcomes

all_out_cleaned <- tidy_all_cleaned %>%
  filter(!is.na(Treatment_var)) %>% #Remove controls
  filter(Relative_time != -21 & Relative_time != 21) %>% #Remove end-points
  filter(Policies_in_regression_f=="Simultaneous policies")

table(all_out_cleaned$Relative_time)
table(all_out_cleaned$dep_var)

#-- get correct names

vars_in_long <- readRDS("data_replicate/table_responses_names_long.rds")

all_coefs <- all_out_cleaned %>%
  left_join(vars_in_long %>%
              select(list_names,list_var, Source,Source_short, Sign_scaling,Source_name_signed) %>%
              distinct(list_var, .keep_all = T), 
            by = c("dep_var"="list_var")) %>%
  mutate(estimate_signif = if_else(!is.na(estimate_signif),estimate,estimate_signif)) %>% 
  mutate(estimate_not_signif = if_else(!is.na(estimate_not_signif),estimate,estimate_not_signif))  
  

if(nrow(all_coefs)!=nrow(all_out_cleaned)) stop("error")

#-- All logs

out_logs <- all_coefs %>%
  filter(str_detect(Outcome_var,"log") ) %>%
  filter(Weights==F)

## plot
evt_plot_logs <- evt_plot(df = out_logs, color = Treatment_var, errorbar = T) + 
  facet_wrap(~Source_name_signed, scales = "free") + #, fac2_var = Regression) 
  labs(color = "Policy")+
  geom_point(x = -1, y = 0, color = "black")+
  scale_color_brewer(palette = "Set1") +
  facet_wrap(~Source_name_signed, scales = "free", ncol=3)+
  theme(strip.text.x = element_text(size = 10))

## save/export
ggsave(evt_plot_logs,width = 10, height = 8, dpi = 90, 
       filename = "output_replicate/fig_5_all_log_outcomes.png")

#-- All levels

out_level <- all_coefs %>%
  filter(specification=="level") %>%
  #filter(!str_detect(Outcome_var,"gm_") ) %>%
  filter(Weights==F)

table(out_level$dep_var)

evt_plot(df = out_level, color = Treatment_var, errorbar = T) + 
  facet_wrap(~Source_name_signed, scales = "free") + #, fac2_var = Regression) 
  labs(color = "Policy")+
  geom_point(x = -1, y = 0, color = "black")+
  scale_color_brewer(palette = "Set1")

ggsave("output_replicate/fig_A14_all_level_outcomes.png",width = 10, height = 8, dpi=90)
  
