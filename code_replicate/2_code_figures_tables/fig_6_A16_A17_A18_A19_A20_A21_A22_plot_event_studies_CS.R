#' ---
#' description: "Plot CS models"
#' date: 2021-04-25
#' author: Jo
#' run_final: TRUE
#' ---


#---------------------------------------------------------------------------------------------------------------
#---------- This code plots the alternative Callaway Santanna estimators
#---------------------------------------------------------------------------------------------------------------

library(matPkg) # remotes::install_github("MatthieuStigler/matPkg", upgrade = "never")
my_packages <- c("tidyverse","magrittr")
lapply(my_packages, library, character.only = TRUE)
source("code_replicate/auxiliary_scripts/888_event_code.R")
source("code_replicate/auxiliary_scripts/general_options.R")

#====================================================================================

# pathin = "Output/regression_event_study_CS/saved_objects_cs/"
pathin = "data_replicate/regression_event_study_CS/saved_objects_cs/"
#pathout = "Output/regression_event_study_CS/plots_did_CS/all_cs_plots/"



## read all "aggregated_att", from did::aggte(did::att_gt())
all_event_studies_aggregated <- list.files("data_replicate/event_studies", full.names = TRUE, pattern = ".rds") %>% 
  as_tibble() %>% #head(10) %>%
  mutate(Control_group_CS = if_else(str_detect(value,"nevertreated"), "Never treated","Not yet treated")) %>%
  mutate(df = map2(value,Control_group_CS, ~readRDS(.x) %>% mutate(Control_group_CS = .y))) 

## clean/rename data
binded_tidy <- bind_rows(all_event_studies_aggregated$df) %>%
  filter(Time > -22 & Time < 22) %>% # go to 21
  mutate(Relative_time = Time) %>%
  mutate(estimate = ATT) %>%
  mutate(std.error = SE) %>%
  mutate(t.stat = ATT/SE) %>%
  mutate(estimate_signif = if_else(abs(t.stat)>=1.96, ATT, as.numeric(NA))) %>%
  mutate(estimate_not_signif = if_else(abs(t.stat)<1.96, ATT, as.numeric(NA))) 

glimpse(binded_tidy)
table(binded_tidy$specification)
table(binded_tidy$Control_group_CS, useNA="ifany")
table(binded_tidy$Included_covariates, useNA="ifany")
table(binded_tidy$Treatment_var)
table(binded_tidy$Outcome_var)
n_out_vars <- n_distinct(str_remove_all(binded_tidy$Outcome_var, "_pct|_rolling|_diff|_log"))
n_out_vars_transo <- n_distinct(binded_tidy$Outcome_var)

# we have:
# - ~4 transfo x 21 vars (not always) => 62 (not 84)
# - 2 covariates spec
# - 3 treatment
# - 43 time periods

all.equal(2*3*43*n_out_vars_transo, nrow(binded_tidy))

## data is unique at: 
mat_is_unique_combo(binded_tidy, specification, Included_covariates,
                    Treatment_var, Outcome_var, Time)

## Check vals for one group: 43 Time values
n_distinct(binded_tidy$Time)
mat_head_group(binded_tidy, specification, Included_covariates,
               Treatment_var, Outcome_var, n_head=1)


## add info
#vars_in_long <- readRDS("Output/vars_names_and_formulas/table_responses_names_long.rds")
vars_in_long <- readRDS("data_replicate/table_responses_names_long.rds")

all_coefs <- binded_tidy %>%
  left_join(vars_in_long %>%
              filter(Source_short != "gm" | specification == "level" ) %>%
              select(list_names,list_var, Source,Source_short,Sign_scaling,Source_name_signed), 
            by = c("Outcome_var"="list_var")) %>% 
  mutate(Treatment_var = str_replace(Treatment_var,"_"," ")) %>% #move elsewhere?
  mutate(Treatment_var = str_replace(Treatment_var,"Resclo","ERC")) %>% #move elsewhere?
  mutate(Treatment_var = str_replace(Treatment_var,"SIP","SIPO")) %>%  #move elsewhere?
  mutate(Treatment_var = str_remove(Treatment_var,"Pol")) %>%  #move elsewhere?
  mutate(Treatment_var = str_remove(Treatment_var,"_numeric")) %>%  #move elsewhere?
  mutate(Included_covariates = if_else(is.na(Included_covariates),"No controls",Included_covariates)) %>%
  filter(Control_group_CS == "Not yet treated") %>%
  mutate(Outcome_var_c = str_replace_all(Outcome_var,"_"," "))
  
glimpse(all_coefs)
table(all_coefs$Outcome_var)

all_coefs

#============================ PLOTS ========================================================
library(RColorBrewer)
my_palette <- brewer.pal(name="Set1",n=5)#[-c(6,7)]
my_palette <- c(my_palette,"#000000")


#------ Completely home only
evt_plot_comp_home_4 <- evt_plot(df = all_coefs %>% filter(str_detect(Outcome_var,"Completely_home")), 
         color = Treatment_var, linetype=Included_covariates, errorbar = T) + 
  facet_grid(Outcome_var_c~Treatment_var, scales = "free") + #, fac2_var = Regression) 
  labs(color = "Policy",linetype = "Included covariates")+
  scale_color_manual(values = my_palette)+
  guides(color="none")+
  theme(strip.text.y = element_text(size = 10))

# evt_plot_comp_home_4 

ggsave(evt_plot_comp_home_4,
       filename = "output_replicate/fig_6_did_cs_completely_home_4_transformations.png", height = 10, width = 10,dpi=100)

# Device exposure only
evt_plot_device_exp_4 <- evt_plot(df = all_coefs %>% filter(str_detect(Outcome_var,"Device_exposure")), 
         color = Treatment_var, linetype=Included_covariates, errorbar = T) + 
  facet_grid(Outcome_var_c~Treatment_var, scales = "free") + #, fac2_var = Regression) 
  labs(color = "Policy",linetype = "Included covariates")+
  scale_color_manual(values = my_palette)

# evt_plot_device_exp_4

# ggsave(evt_plot_device_exp_4,
#        filename = "output_replicate/did_cs_device_exposure_4_transformations.png", height = 10, width = 10)


#--- Aggregated event studies, all outcomes
names_out <- tibble(type=c("rolling", "level","log","diff"),
                    paper_num = paste0("A", 17:20))

for (i in  1:nrow(names_out)){
  
  pl_i <- evt_plot(df = all_coefs %>% filter(specification==names_out$type[i] & Included_covariates=="No controls"),
                   color = Treatment_var, errorbar = TRUE) + 
    facet_wrap(~Source_name_signed, scales = "free") + #, fac2_var = Regression) 
    scale_color_manual(values = my_palette) +
    labs(color = "Policy")
  ggsave(plot = pl_i,
         filename=paste0("output_replicate/fig_", names_out$paper_num[i], "_did_cs_all_",names_out$type[i],"_no_controls.png"),
         width = 10, height = 8)
  
  # -- With covariates
  # evt_plot(df = all_coefs %>% filter(specification==v & 
  #                                      Included_covariates=="prcp + snow + tmean + Cases_dummy + Deaths_dummy") 
  #          ,color = Treatment_var,errorbar = T, fac1_var = Outcome_var) + 
  #   facet_wrap(~Source_name_signed, scales = "free") + #, fac2_var = Regression) 
  #   scale_color_manual(values = my_palette)+
  #   labs(color = "Policy")
  # ggsave(paste0("output_replicate/did_cs_all_",v,"_covid_covariates.png"),width = 10, height = 8)
  
}



#======= All, by cohorts =====================

## read from did::att_gt()
all_event_studies_by_group <- list.files(paste0(pathin,"all_atts"), full.names = TRUE, pattern = ".rds") %>% 
  as_tibble() %>%
  mutate(df = map(value, ~readRDS(.x)))

library(data.table)

binded_tidy_by_groups <- rbindlist(all_event_studies_by_group$df, fill = TRUE) %>%
  mutate(Relative_time = Time-Group) %>%
  filter(Relative_time > -22 & Relative_time < 22) %>% # go to 21
  mutate(estimate = ATT) %>%
  mutate(std.error = SE) %>%
  mutate(t.stat = ATT/SE) %>%
  mutate(estimate_signif = if_else(abs(t.stat)>=1.96, ATT, as.numeric(NA))) %>%
  mutate(estimate_not_signif = if_else(abs(t.stat)<1.96, ATT, as.numeric(NA))) 

binded_tidy_by_groups

table(binded_tidy_by_groups$specification)
table(binded_tidy_by_groups$Included_covariates)

glimpse(binded_tidy_by_groups)
summary(binded_tidy_by_groups$P_val_pre_test_parallel_trends)
table(binded_tidy$Included_covariates)


all_coefs_all_groups <- binded_tidy_by_groups %>%
  as_tibble() %>%
  left_join(vars_in_long %>%
              filter(Source_short != "gm" | specification == "level" ) %>%
              select(list_names,list_var, Source,Source_short,Sign_scaling,Source_name_signed), 
            by = c("Outcome_var"="list_var")) %>% 
  mutate(Treatment_var = str_replace(Treatment_var,"_"," ")) %>% #move elsewhere?
  mutate(Treatment_var = str_replace(Treatment_var,"Resclo","ERC")) %>% #move elsewhere?
  mutate(Treatment_var = str_replace(Treatment_var,"SIP","SIPO")) %>%  #move elsewhere?
  mutate(Treatment_var = str_remove(Treatment_var,"Pol")) %>%  #move elsewhere?
  mutate(Treatment_var = str_remove(Treatment_var,"_numeric")) %>%  #move elsewhere?
  mutate(Included_covariates = if_else(is.na(Included_covariates),"No controls",Included_covariates)) %>%
  filter(Control_group_CS == "notyettreated") %>%
  mutate(Outcome_var_c = str_replace_all(Outcome_var,"_"," ")) %>%
  mutate(Group_f = as.character(as.Date(Group, origin = "1970-01-01"))) 
  
all_coefs_all_groups

#-- Plot all ATTS
table(all_coefs_all_groups$Outcome_var)
table(all_coefs_all_groups$Treatment_var)
table(all_coefs_all_groups$Included_covariates)

        #Atts_plots <- all_coefs_all_groups %>%  
        #  filter(specification=="level" & 
        #           Treatment_var=="State SIPO" & 
        #           Included_covariates=="No controls") #& Included_covariates=="prcp + snow + tmean + Cases_dummy + Deaths_dummy") 
        #
        #evt_plot(df = Atts_plots, color = Group_f,errorbar = T,fac1_var = Outcome_var_c) + 
        #  facet_wrap(~Source_name_signed, scales = "free") + #, fac2_var = Regression) 
        #  labs(color = "State SIPO treatment group")
        #ggsave(paste0(pathout,"did_cs_all_atts_level_no_controls_state_SIP.png"),width = 10, height = 10)

#------ Just completely home

Atts_plots_completely_home <- all_coefs_all_groups %>%  
  filter(str_detect(Outcome_var,"Completely_home"))

pl_did_cs_completely_home_all_cohorts_transformations <- Atts_plots_completely_home %>% 
  filter(Included_covariates == "No controls") %>% 
  evt_plot(color = Group_f,  errorbar = T) + 
  facet_grid(Outcome_var_c~Treatment_var, scales = "free") + #, fac2_var = Regression) 
  labs(color = "Treatment group")+
  guides(color=guide_legend(ncol=10))

ggsave(pl_did_cs_completely_home_all_cohorts_transformations, height = 10, width = 12,dpi=100,
       filename = "output_replicate/fig_A16_did_cs_completely_home_all_cohorts_transformations.png")


################################
#'## Pre-trends?
################################

## Extract pre-tests from raw did::att_gt() ?
pre_tests <- all_coefs_all_groups %>% 
  distinct(specification, Included_covariates,
           Treatment_var, Outcome_var, W, Wpval)

## BUT many NAs!? :-(
mean(is.na(pre_tests$W))

## construct from did::aggte(did::att_gt())
all_coefs_pre <- all_coefs %>% 
  filter(Time<0) %>% 
  mutate(is_rejected = abs(t.stat)>=1.96,
         p_val = 2 * pnorm(abs(t.stat), lower.tail = FALSE)) 

## Just visu
all_coefs_pre %>% 
  mat_head_group(Included_covariates,
                 Treatment_var, Outcome_var, Source_name_signed, n_head=1) %>%
  select(Time, ATT, SE,t.stat, p_val, is_rejected) 

## Now aggregate by spec-outcome, averaging out cohorts
pre_tests_rejected_CS_raw <- all_coefs_pre%>% 
  group_by(specification, Included_covariates,
           Treatment_var, Outcome_var, Source_name_signed, Source) %>% 
  summarise(perc_reject = mean(is_rejected)) %>% 
  ungroup()

pre_tests_rejected_CS_raw

## aggregate further, averaging out outcome variable
pre_tests_rejected_CS_byOutcome <- pre_tests_rejected_CS_raw%>% 
  group_by(Treatment_var, Source_name_signed, Source) %>% 
  summarise(n_cases = n(),
            sum_reject_5 =sum(perc_reject>0.05),
            sum_reject_10 =sum(perc_reject>0.1)) %>% 
  ungroup()

pre_tests_rejected_CS_byOutcome


## aggregate further by transfo
pre_tests_rejected_CS_byTransfo <- pre_tests_rejected_CS_raw%>% 
  filter(Source!="Google") %>% 
  mutate(transfo = paste0(str_to_title(specification), ": ",
                         if_else(Included_covariates=="No controls", "No controls", "Controls"))) %>% 
  group_by(Treatment_var, transfo, specification) %>% 
  summarise(n_cases = n(),
            mean_reject_5 =mean(perc_reject>0.05),
            mean_reject_10 =mean(perc_reject>0.1)) %>% 
  ungroup() %>% 
  mutate(mean_reject_5_char = paste0(round(100*mean_reject_5), "%"),
         mean_reject_10_char = paste0(round(100*mean_reject_10), "%"),
         mean_reject_both_char = paste0(round(100*mean_reject_5), "/", round(100*mean_reject_10), " (%)"))

## how many outcomes per cell? 14
pre_tests_rejected_CS_byTransfo %>% 
  count(n_cases)


## plot
pl_heat_CS_byTransfo <- pre_tests_rejected_CS_byTransfo %>% 
  ggplot(aes(x=Treatment_var, y = transfo, fill = mean_reject_5))+
  geom_tile()+
  geom_text(aes(label=mean_reject_5_char))+
  viridis::scale_fill_viridis(alpha = .8, labels = scales::percent)+
  # facet_grid(specification~., scales = "free")
  labs(fill ="Percentage of outcome with at least 5% of cohort-specic parallel-trend tests rejected :")+
  ylab("Outcome transformation") + 
  xlab("Policy in regression")+
  # labs(fill = "Count p-values < .05") +
  theme_jo77+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.y = element_text(hjust=0), # left align Y
        axis.ticks.y = element_blank(),
        legend.position = "none")
  
# pl_heat_CS_byTransfo+
#   ggtitle("% of pre-test rejected at 5%/10% ")
  
ggsave(pl_heat_CS_byTransfo, filename = "output_replicate/fig_A22_CS_pre_trends_heatmap.png",
       height = 10, width = 12,dpi=300)


################################
#'## Overall DiD
################################



## get ATT overall
ATT_dynamic <- all_coefs %>% 
  distinct(Overall_ATT, Overall_SE, specification, Included_covariates,
           Treatment_var, Outcome_var, Source_name_signed) %>% 
  mutate(t_stat = Overall_ATT/Overall_SE,
         p_val = 2 * pnorm(abs(t_stat), lower.tail = FALSE),
         sign = sign(Overall_ATT),
         is_signif = p_val<0.05,
         categ = case_when(sign==-1 & is_signif~ "A",
                           sign==-1 & !is_signif~ "B",
                           sign==1 & !is_signif~ "C",
                           sign==1 & is_signif~ "D"))

ATT_dynamic

## count
ATT_dynamic_cnt <- ATT_dynamic %>% 
  mutate(Outcome_var = str_remove_all(Outcome_var,"_pct|_rolling|_diff|_log")) %>% 
  count(Treatment_var, Source_name_signed,categ, is_signif)

ATT_dynamic_cnt


### plot
col2rgb("dodgerblue")
col2rgb("red3")

blue <- rgb(30, 144, 255, maxColorValue = 255)
blue_light <- rgb(30, 144, 255, maxColorValue = 255, alpha=0.35*255)
red <- rgb(205, 0, 0, maxColorValue = 255)
red_light <- rgb(205, 0, 0, maxColorValue = 255, alpha=0.35*255)

tentative_plot <- ATT_dynamic_cnt %>%  
  # filter(Treatment_var=="County ED") %>% 
  ggplot(aes(x=categ, y=Source_name_signed, size = n, color=categ))+
  geom_point()+
  scale_color_manual(values = c("A" = red,
                                "B" = red_light,
                                "C" = blue_light,
                                "D" = blue),
                     guide = 'none')+
  scale_x_discrete(labels = c("A"="<0, sig", "B"= "<0, not", "C" = ">0, not", "D"=">0, sig"))+
  facet_wrap(~Treatment_var)+
  xlab("Coefficient sign and significance")+
  ylab("Outcome")+
  theme(legend.position = "bottom")+
  labs(size="Number of coefficients: ")

# tentative_plot

ggsave(tentative_plot, filename = "output_replicate/fig_A21_did_cs_overall_ATT_sign.png",
       height = 10, width = 12,dpi=300)
