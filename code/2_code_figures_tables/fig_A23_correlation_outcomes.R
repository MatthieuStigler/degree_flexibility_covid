#' ---
#' description: "correlation tables"
#' date: 2021-08-25
#' author: Jo
#' run_final: TRUE
#' ---


my_packages <- c("tidyverse","magrittr","lfe","stargazer", "broom", "future", "furrr", "Formula")
sapply(my_packages, require, character.only = TRUE)
sapply(my_packages, function(x) packageVersion(x) %>%  as.character)

if(!require(matPkg)) remotes::install_github("MatthieuStigler/matPkg", upgrade = "never")

map<-purrr::map #For the nice iterations
select<-dplyr::select

source("code/auxiliary_scripts/general_options.R")
extrafont::loadfonts()
options(scipen=999)

################################
#'## Read data
################################

data_reg <- readRDS("data_replicate/2_data_final/merged_panel_did.rds")

table_vars <- read_rds("data_replicate/1_data_intermediate/vars_names_and_formulas/table_responses_names.rds")

vars_level <- unique(table_vars$list_var)
outcomes_levels <- table_vars %>% select(list_var,list_names) %>% distinct()

data_outcomes <- data_reg %>%
  select(vars_level) %>%
  rename_at(vars(outcomes_levels$list_var),vars(outcomes_levels$list_names))

correlations_table <- cor(data_outcomes, method = "pearson",use = "complete.obs")

# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
upper_tri <- get_upper_tri(correlations_table)
library("reshape2")# testing


melted_cormat <- melt(upper_tri, na.rm = TRUE) 
  

pl_heat_mp_correl <- ggplot(data = melted_cormat, 
       aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-0.2,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  xlab("Outcome 1")+
  ylab("Outcome 2")+
  coord_fixed()+
  theme_jo_small+
  theme(panel.background = element_rect(fill = "white"))

ggsave(pl_heat_mp_correl, width = 7, height = 7, dpi=100,
       filename =   "output_replicate/fig_A23_heat_map_correlations.png")


