#' ---
#' description: "Plot Goodman-Bacon estimates"
#' date: 2021-04-25
#' author: Jo
#' run_final: TRUE
#' ---


my_packages <- c("tidyverse","magrittr","RColorBrewer","ggrepel","lubridate")
#install.packages(my_packages, repos = "http://cran.rstudio.com")
lapply(my_packages, library, character.only = TRUE)

map<-purrr::map #To avoid conflicts
select<-dplyr::select

options(stringsAsFactors = FALSE) ## Do not load strings as factors
source("code/auxiliary_scripts/general_options.R")


#-----------------------------------------------------------------------------------------------------------------
vars_in_long <- readRDS("data_replicate/table_responses_names_long.rds")

all_decomps <- readRDS("data_replicate/3_data_analysis_output/all_bacon_decomps_individual_policies.rds")


test <- all_decomps[[1]]
table(test$type)


sum(test$weight*test$estimate)

test %<>%
  filter(type!="Later vs Earlier Treated") %>%
  mutate(cleaned_weight = weight/(sum(weight)))
  
sum(test$cleaned_weight*test$estimate)

all_diff <- tibble(Outcome = rep(NA, length(all_decomps)),
                   Indep_var = rep(NA, length(all_decomps)),
                   FE = rep(NA, length(all_decomps)),
                   FE_cleaned = rep(NA, length(all_decomps)))


#------- Doing it for all outcomes and policies

for (i in 1:length(all_decomps)) {
  message(i)
  reg_i <- all_decomps[[i]]
  
  FE_standard <- sum(reg_i$weight*reg_i$estimate)
  
  reg_i %<>%
    filter(type!="Later vs Earlier Treated") %>%
    mutate(cleaned_weight = weight/(sum(weight)))
  
  FE_cleaned <- sum(reg_i$cleaned_weight*reg_i$estimate)
  
  all_diff$Outcome[[i]] <- reg_i$Dep_var[[1]]
  all_diff$Indep_var[[i]] <- reg_i$Indep_var[[1]]
  all_diff$FE[[i]] <- FE_standard
  all_diff$FE_cleaned[[i]] <- FE_cleaned
                     
}

glimpse(all_diff)
table(all_diff$Outcome)

library(rcartocolor)

all_diff_subset <- all_diff %>%
  left_join(vars_in_long %>%
              filter(Source_short != "gm" | specification == "level" ) %>%
              select(list_names,list_var, Source,Source_short,Sign_scaling,Source_name_signed,specification), 
            by = c("Outcome"="list_var")) %>% 
  mutate(Indep_var = str_replace(Indep_var,"_"," ")) %>% #move elsewhere?
  mutate(Indep_var = str_replace(Indep_var,"Resclo","ERC")) %>% #move elsewhere?
  mutate(Indep_var = str_replace(Indep_var,"SIP","SIPO")) %>%  #move elsewhere?
  mutate(Indep_var = str_remove(Indep_var,"Pol")) %>%  #move elsewhere?
    mutate(Diff_FE_decomp = FE_cleaned-FE) %>%
  mutate(Ratio_FE_cleaned = FE_cleaned/FE) 


glimpse(all_diff_subset)

plot_individual_pol <- ggplot(all_diff_subset, aes(x = Indep_var, y = Ratio_FE_cleaned) )+
    geom_col(aes(color = Source_name_signed, fill = Source_name_signed),width=0.5,    
           position=position_dodge(0.5))+
  geom_hline(yintercept = 0)+
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey50")+
  scale_fill_manual(values = colorRampPalette(carto_pal(12,"Bold"))(14))+
  scale_color_manual(values = colorRampPalette(carto_pal(12,"Bold"))(14))+
  facet_grid(specification~., scales = "free")+
  labs(color = "",fill="")+
  xlab("Policy")+
  ylab("Ratio of cleaned two-way FE over regular 2-way FE")+
  theme_jo_small2

ggsave(plot_individual_pol, width = 11.5, height = 8,
       filename = "output_replicate/fig_A24_plot_individual_policies_ratios_FE_bacondecomp.png")
