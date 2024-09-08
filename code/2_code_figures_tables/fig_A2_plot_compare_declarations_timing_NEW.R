#' ---
#' description: ""
#' runMat: TRUE
#' run_final: TRUE
#' date: 2020-05-24
#' ---

library(ggforce)
my_packages <- c("tidyverse","magrittr","RColorBrewer","ggrepel","sf","lubridate","stargazer","cowplot")
lapply(my_packages, library, character.only = TRUE)

map<-purrr::map #For the nice iterations
select<-dplyr::select
if(!require(matPkg)) remotes::install_github("MatthieuStigler/matPkg", upgrade = "never")

source("code/auxiliary_scripts/general_options.R")


#-----------------------------------------------------------------------------------------------------------

## read data
df_declare <- readRDS("data_replicate/1_data_intermediate/declarations_counties_states_cases_dates.rds") 
glimpse(df_declare)

  


## selectr policies
all_pols <- c("StatePol_ED","StatePol_Resclo", "StatePol_SIP",
              "County_ED","County_SIP",
              "Date_first_reported_case", "Date_first_reported_death")

## quick check: first dates
df_declare %>% 
  summarise(across(all_of(all_pols), min, na.rm=TRUE))

## why NA for first death?
df_declare %>% 
  select(FIPS, Date_first_reported_death) %>% 
  mutate(seems_na = is.na(as.character(Date_first_reported_death)),
         is_na = is.na(Date_first_reported_death),
         is_finite = is.finite(Date_first_reported_death))


## combination of policies
all_combin <- combn(all_pols, 2) %>% t() %>% 
  magrittr::set_colnames(c("Var1", "Var2")) %>% 
  as_tibble() %>% 
  mutate(Var1 =factor(Var1, levels=all_pols))

all_combin


compute_diff <- function(Var1_name, Var2_name, df=df_declare) {
  
  ## prep data
  df_prep <- df %>% 
    select(FIPS, all_of(c(Var1_name, Var2_name))) %>% 
    rename("Var1"=!!rlang::sym(Var1_name),
           "Var2"=!!rlang::sym(Var2_name))

  ## get diff and sign(diff)
  df_diff <- df_prep %>%
    mutate(diff= as.integer(Var2-Var1),
           diff_sign =case_when(diff>=0~1L,
                                diff<0~-1L,
                                # diff==0~0L,
                                TRUE~NA_integer_))

  ## summarise: mean, median  and count
  df_diff %>%
    mutate(median_date = median(Var1, na.rm=TRUE)) %>% 
    group_by(diff_sign) %>%
    summarise(median_date = unique(median_date),
                across(diff, lst(mean, median)),
              count = n(), .groups = 'drop') %>%
    ungroup() %>% 
    complete(median_date, diff_sign=c(-1,1),
             fill=list(count=0, diff_median=0)) %>% 
    mutate(median_av = as.Date(median_date+diff_median,origin = "1970-01-01"),
           median_av_midpoint = as.Date(median_date+diff_median/2,origin = "1970-01-01"),
           mean_pos_txt = ifelse(diff_median!=0, paste0(round(diff_median)," days"), NA),
           mean_pos_txt_short = if_else(abs(round(diff_median))<=4,
                                        str_replace(mean_pos_txt, "days", "d."),
                                        mean_pos_txt))
      
}

## test on 1
compute_diff(Var1_name="StatePol_ED", Var2_name="StatePol_SIP")

## run on all
smrys_df <- all_combin %>% 
  # head(5) %>%
  mutate(data=map2(as.character(Var1), Var2, compute_diff)) %>% 
  unnest(data)

smrys_df

## clean further
clean_names <- function(x) {
  str_replace_all(x, "_", " ") %>% 
    str_remove("Pol") %>% 
    str_replace("SIP","SIPO") %>%
    str_replace("Resclo","ERC") %>% 
    str_replace("Date first reported", "First")
}

clean_names(all_pols)

smrys_df_clean <- smrys_df %>% 
  filter(!is.na(diff_sign)) %>% 
  mutate(diff_sign= factor(diff_sign),
         across(c(Var1, Var2), ~clean_names(.) %>% factor(levels = clean_names(all_pols)))) %>% 
  mutate(Var2=factor(Var2, levels = rev(clean_names(all_pols))),
         diff_sign_num = as.numeric(as.character(diff_sign))) %>% 
  arrange(Var1, desc(Var2)) 

smrys_df_clean


################################
#'## Plot
################################


smrys_df %>% 
  distinct(Var1, Var2)

smrys_df$Var1 %>% levels

col_pos = "darkorange3"
col_neg = "cornflowerblue"
y_nudge <- 0.3
size = size_pol = 4
x_nudge = 1.5

## Lot of manual scaling needed, also Remove "cases"
smrys_df_clean_keep <- smrys_df_clean %>% 
  filter(!str_detect(Var1, "case|death") & !str_detect(Var2, "case|death")) %>% 
  mutate(position= if_else(diff_sign_num==-1, "right", "left"),
         offset = if_else(diff_sign_num==-1, -0.2, 0.2),
         n_char_countyNum = nchar(count),
         text_count_pos = median_av+0.5*diff_sign_num,# unused
         text_Var_pos = median_av+1.5*diff_sign_num*n_char_countyNum,
         text_Var_pos = case_when(n_char_countyNum==1~1.5*diff_sign_num,
                                  n_char_countyNum==2~0.9*diff_sign_num *n_char_countyNum,
                                  n_char_countyNum==3~0.8*diff_sign_num *n_char_countyNum,
                                  n_char_countyNum==4~0.7*diff_sign_num *n_char_countyNum
         ) + median_av)

## Main PLOT
lolliplot <-  smrys_df_clean_keep%>% 
  ggplot() +
  geom_segment(aes(y=Var2, yend=Var2, x=median_date, xend=median_av, color = diff_sign), size = 1.5)+
  geom_point(aes(y = Var2, x = median_av, color = diff_sign) , size = 2) +
  ggforce::facet_col(~Var1, scales = "free_y", space = "free")+
  scale_color_manual(values = c(col_neg, col_pos))+
  geom_point(aes(y = Var2, x = median_date), size = 2)+
  geom_label(aes(y = Var2, x = text_count_pos, label = count, col = diff_sign,
                 hjust=position)
             )+
  # Add var name
  geom_text(aes(y = Var2, x = text_Var_pos, label = Var2, col = diff_sign,
                hjust=position), 
            size = size_pol,fontface = "bold") +
  ## count median days, on top of label
  geom_text(aes(y = Var2, x = median_av_midpoint,label = mean_pos_txt_short, col = diff_sign),
             nudge_y = y_nudge,
            size = 3)+
  theme_jo77+
  theme(legend.position = "none")+
  theme(strip.text = element_text(size = 14, face = "bold")) +
  xlab(NULL)+
  theme(
    axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  scale_x_continuous(expand = c(.1, .1))


# lolliplot 

################################
## export
################################

ggsave(lolliplot, filename = "output_replicate/fig_A2_lollipop_LATEST.png",
       width = 13, height = 7,dpi=100)