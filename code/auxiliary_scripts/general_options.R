## Plotting themes for sexy figures


options(stringsAsFactors = FALSE) ## Do not load strings as factors
options("lfe.threads"=12)
getOption("lfe.threads")

gg_save_width <- 8
gg_save_height <- 5

gg_width_Tex_third <- 6.3
gg_height_Tex_third <- 2

gg_width_Tex_half <- 6.3
gg_height_Tex_half <- 3.3


################################
#'## Get cuter names for figures
################################

table_covars <- readr::read_rds("data_replicate/1_data_intermediate/vars_names_and_formulas/table_covariates_reg.rds")
tab_covar_keep <- table_covars |>
  dplyr::filter(stringr::str_detect(covariate_name, "StatePol|County_ED$|County_SIP$")) |>
  dplyr::select(covariate_name, covariate_name_clean)  
policy.facet.lab <- tab_covar_keep$covariate_name_clean |>
  rlang::set_names(tab_covar_keep$covariate_name)

rm(table_covars, tab_covar_keep)

library(ggplot2)
font_plot = "Arial Narrow" #"CMU Serif"
theme_jo <-   theme(plot.title = element_text(hjust=0, size=30, margin=margin(b=10), family=font_plot, face="bold"),
                    plot.subtitle = element_text(hjust=0, size=26, margin=margin(b=15), family=font_plot, face="plain"),
                    plot.caption=element_text(hjust=1, size=22, margin=margin(t=10), family=font_plot, face="italic"),
                    text=element_text(family=font_plot),
                    axis.text.x=element_text(size=24, margin=margin(t=0)),
                    axis.text.y=element_text(size=24, margin=margin(r=0)),
                    axis.title=element_text(size=24, family=font_plot),
                    axis.title.x=element_text(size=24, family=font_plot, face="plain"), #hjust=xj,
                    axis.title.y=element_text(size=24, family=font_plot, face="plain"), #hjust=yj, 
                    axis.title.y.right=element_text(size=24, angle=90, family=font_plot, face="plain"), #hjust=yj,
                    strip.text=element_text(hjust=.5, size=20, face="plain", family=font_plot),
                    plot.margin=margin(30, 30, 30, 30),
                    #plot.margin = unit(c(0.05,0.05,0.05,0.05), "inches"),
                    legend.title=element_text(size=22),
                    legend.text=element_text(size=22),
                    legend.position="bottom",
                    legend.box = "horizontal",
                    panel.background = element_rect(fill = "white"),
                    # panel.grid=element_line(color="#cccccc", size=0.2),
                    # panel.grid.major=element_line(color="#cccccc", size=0.2),
                    # panel.grid.minor=element_line(color="#cccccc", size=0.15),
                    panel.grid=element_line(color="#cccccc", size=0.2),
                    panel.grid.major=element_line(color="#cccccc", size=0.2),
                    panel.grid.minor=element_line(color="#cccccc", size=0.15),
                    )  

fips_num_to_char <- function(x) str_pad(x, width=2, side = "left", pad = "0")
if(FALSE) {
  fips_num_to_char(c(1, 10, 100))
}



theme_jo77 <-   theme(plot.title = element_text(hjust=0, size=20, margin=margin(b=10), family=font_plot, face="bold"),
                      plot.subtitle = element_text(hjust=0, size=16, margin=margin(b=15), family=font_plot, face="plain"),
                      plot.caption=element_text(hjust=1, size=12,margin=margin(t=10), family=font_plot, face="italic"),
                      text=element_text(family=font_plot),
                      axis.text.x=element_text(size=12, margin=margin(t=0)),
                      axis.text.y=element_text(size=12, margin=margin(r=0)),
                      axis.title=element_text(size=12, family=font_plot),
                      axis.title.x=element_text(size=12, family=font_plot, face="plain"), #hjust=xj,
                      axis.title.y=element_text(size=12, family=font_plot, face="plain"), #hjust=yj, 
                      axis.title.y.right=element_text(size=12, angle=90, family=font_plot, face="plain"), #hjust=yj,
                      strip.text=element_text(hjust=.5, size=12, face="plain", family=font_plot),
                      plot.margin=margin(30, 30, 30, 30),
                      #plot.margin = unit(c(0.05,0.05,0.05,0.05), "inches"),
                      legend.title=element_text(size=12),
                      legend.text=element_text(size=12),
                      legend.position="bottom",
                      legend.box = "horizontal",
                      panel.background = element_rect(fill = "white"),
                      panel.grid=element_line(color="#cccccc", size=0.2),
                      panel.grid.major=element_line(color="#cccccc", size=0.2),
                      panel.grid.minor=element_line(color="#cccccc", size=0.15))

theme_jo_small <-   theme(plot.title = element_text(hjust=0, size=16, margin=margin(b=10), family=font_plot, face="bold"),
                          plot.subtitle = element_text(hjust=0, size=14, margin=margin(b=15), family=font_plot, face="plain"),
                          plot.caption=element_text(hjust=1, size=8,margin=margin(t=8), family=font_plot, face="italic"),
                          text=element_text(family=font_plot),
                          axis.text.x=element_text(size=8, margin=margin(t=0)),
                          axis.text.y=element_text(size=8, margin=margin(r=0)),
                          axis.title=element_text(size=8, family=font_plot),
                          axis.title.x=element_text(size=12, family=font_plot, face="plain"), #hjust=xj,
                          axis.title.y=element_text(size=12, family=font_plot, face="plain"), #hjust=yj, 
                          axis.title.y.right=element_text(size=8, angle=90, family=font_plot, face="plain"), #hjust=yj,
                          strip.text=element_text(hjust=.5, size=8, face="plain", family=font_plot),
                          #plot.margin=margin(30, 30, 30, 30),
                          #plot.margin = unit(c(0.05,0.05,0.05,0.05), "inches"),
                          legend.title=element_text(size=8),
                          legend.text=element_text(size=8),
                          legend.position="bottom",
                          legend.box = "horizontal",
                          panel.background = element_rect(fill = "white"),
                          panel.grid=element_line(color="#cccccc", size=0.2),
                          panel.grid.major=element_line(color="#cccccc", size=0.2),
                          panel.grid.minor=element_line(color="#cccccc", size=0.15))
#guides(color = guide_legend(override.aes = list(fill = "white"))) 

tjs2_size <- 11
tjs2_size_title <- tjs2_size+2

theme_jo_small2 <-   theme(plot.title = element_text(hjust=0, size=16, margin=margin(b=10), family=font_plot, face="bold"),
                          plot.subtitle = element_text(hjust=0, size=14, margin=margin(b=15), family=font_plot, face="plain"),
                          plot.caption=element_text(hjust=1, size=tjs2_size,margin=margin(t=8), family=font_plot, face="italic"),
                          text=element_text(family=font_plot),
                          axis.text.x=element_text(size=tjs2_size, margin=margin(t=0)),
                          axis.text.y=element_text(size=tjs2_size, margin=margin(r=0)),
                          axis.title=element_text(size=tjs2_size, family=font_plot),
                          axis.title.x=element_text(size=tjs2_size_title, family=font_plot, face="plain"), #hjust=xj,
                          axis.title.y=element_text(size=tjs2_size_title, family=font_plot, face="plain"), #hjust=yj, 
                          axis.title.y.right=element_text(size=tjs2_size, angle=90, family=font_plot, face="plain"), #hjust=yj,
                          strip.text=element_text(hjust=.5, size=tjs2_size, face="plain", family=font_plot),
                          #plot.margin=margin(30, 30, 30, 30),
                          #plot.margin = unit(c(0.05,0.05,0.05,0.05), "inches"),
                          legend.title=element_text(size=tjs2_size),
                          legend.text=element_text(size=tjs2_size),
                          legend.position="bottom",
                          legend.box = "horizontal",
                          panel.background = element_rect(fill = "white"),
                          panel.grid=element_line(color="#cccccc", size=0.2),
                          panel.grid.major=element_line(color="#cccccc", size=0.2),
                          panel.grid.minor=element_line(color="#cccccc", size=0.15))
  