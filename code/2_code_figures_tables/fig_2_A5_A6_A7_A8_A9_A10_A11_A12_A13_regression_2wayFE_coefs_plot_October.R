#' ---
#' description: "Coef plots for 2 way FE"
#' date: 2020-10-01
#' author: Matthieu
#' run_final: TRUE
#' ---

my_packages <- c("tidyverse")
sapply(my_packages, require, character.only = TRUE)

map<-purrr::map #For the nice iterations
select<-dplyr::select
library(RColorBrewer)
if(!require(matPkg)) remotes::install_github("MatthieuStigler/matPkg", upgrade = "never")
source("code/auxiliary_scripts/general_options.R")
gg_save_height = gg_save_height+2
gg_save_width = gg_save_width+2
#-------------------------------------------- ----------------------------------------------------------------

################################
#'## Read data
################################

reg_2wayFE_Oct_coefs <- read_rds("data_replicate/3_data_analysis_output/2wayFE_coefs_all.rds")
glimpse(reg_2wayFE_Oct_coefs)
table_vars <- read_rds("data_replicate/table_responses_names.rds")
glimpse(table_vars)

formus_df <- readRDS("data_replicate/table_formulas_reg.rds") %>%
  select(formu_num,formu_legend)

################################
#'## Prepare and Check data
################################

## unique at?
reg_2wayFE_Oct_coefs %>% 
  mat_is_unique_combo(list_var, formu_num, term, has_log)

## how many regs?
reg_2wayFE_Oct_coefs %>% 
  distinct(formu_num)

## add infos
reg_2wayFE_Oct_coefs_c <- reg_2wayFE_Oct_coefs %>% 
  mutate(formu_char = paste("f", formu_num)) %>% 
  left_join(table_vars %>% 
              select(list_var, list_var_log, is_main, Category, Sign_scaling,Source_short, Source_name_signed), by = c("list_var"="list_var","list_var_log"="list_var_log" )) %>% 
  mutate(list_var= str_replace_all(list_var, "_", " ")) %>%
  mutate(formu_legend_fac = factor(formu_legend, levels=c("State policies (ED+SIPO+ERC)",
                                                          "County policies (ED+SIPO)",
                                                          "All policies", 
                                                          "All policies and weather",
                                                          "All policies, weather, cases dummy", 
                                                          "All policies, weather, deaths dummy",
                                                          "All policies, weather, cases and deaths dummy"))) %>%
  mutate(term_cleaned = str_replace(term,"_"," ")) %>% 
  mutate(term_cleaned = str_replace(term_cleaned,"Resclo","ERC")) %>% 
  mutate(term_cleaned = str_replace(term_cleaned,"SIP","SIPO")) %>% 
  mutate(term_cleaned = str_remove(term_cleaned,"Pol")) 
  

reg_2wayFE_Oct_coefs %>% count(list_var)

glimpse(reg_2wayFE_Oct_coefs_c)
table(reg_2wayFE_Oct_coefs_c$formu_legend )
table(reg_2wayFE_Oct_coefs_c$term)

  

################################
#'## Plots
################################


pl_coef_state_ED <- reg_2wayFE_Oct_coefs_c %>%
  filter(term=="StatePol_ED" & !has_log) %>% 
  mat_plot_coefs_tidy(fill_var = formu_legend_fac, #formu_char 
                      fac1_var = list_var, 
                      scales = "fixed") +
  facet_wrap(Source_name_signed~., nrow=6, scales = "free")+
  geom_hline(yintercept = 0, linetype=2) +
  ggtitle("Stability of State ED over specifications, level") +
  labs(fill = "Specification:", color = "Specification:")+
  guides(colour = guide_legend(nrow = 2),
         fill = guide_legend(nrow = 2))+
  ylab("Estimated Policy Impact")+
  theme_jo_small+
  scale_fill_brewer(palette = "Spectral")+
  scale_color_brewer(palette = "Spectral")+
  theme(strip.text.x = element_text(size = 8),
        axis.title.x=element_blank(),
        axis.text.x=element_blank())+
  xlab(NULL)

# pl_coef_state_ED

####### Do other policies in log

policies_df <- c("StatePol_ED","StatePol_SIP","StatePol_Resclo","County_ED","County_SIP") %>%
  as_tibble() %>%
  mutate(Policy_name = str_replace_all(value,"_"," ") %>% 
    str_replace_all("StatePol","State") %>% 
    str_replace_all("SIP","SIPO") %>% 
    str_replace_all("Resclo","ERC") 
  ) 

policies_df_add <- policies_df %>% 
  mutate(number_paper_logs = c("2", paste0("A", 5:8)),
         number_paper_levs = paste0("A", 9:13))

for (i in 1:nrow(policies_df_add)) {

  ## levels
  stability_plot <- pl_coef_state_ED %+%
    filter(reg_2wayFE_Oct_coefs_c,term==policies_df_add[[i,1]] & has_log & Source_short!="gm")+
    ggtitle( paste0("Stability of ", policies_df_add[[i,2]], " over specifications, logs") )
  
  #stability_plot
  ggsave(stability_plot, height = gg_save_height, width = gg_save_width,
         filename = paste0("output_replicate/fig_", policies_df_add[[i,"number_paper_logs"]],  
                           "_coef_",policies_df_add[[i,1]],"_log.png"),
         dpi=100 )
  
  ### logs
  stability_plot_lev <- pl_coef_state_ED %+%
    filter(reg_2wayFE_Oct_coefs_c,term==policies_df[[i,1]] & !has_log)+
    ggtitle( paste0("Stability of ", policies_df[[i,2]], " over specifications, levels") )
  
  #stability_plot
  ggsave(stability_plot_lev, height = gg_save_height, width = gg_save_width,
         filename = paste0("output_replicate/fig_", policies_df_add[[i,"number_paper_levs"]],  
                "_coef_",policies_df_add[[i,1]],"_level.png"),
         dpi=100 )
}
