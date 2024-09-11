#' ---
#' title: "Rerun all files"
#' author: "Matthieu"
#' date: 2024-09-08
#' ---

library(tidyverse)
library(collateral)

################################
#'## List scripts
################################

files_df <- tibble(full_path=list.files("code", pattern= "\\.R$", recursive = TRUE, full.names=TRUE)) %>% 
  mutate(file = basename(full_path))

## need to exclude some:
files_keep <- files_df %>% 
  ## exclude safegraph files
  filter(!str_detect(file, "clean_safegraph")) %>% 
  ## exclude cuebiq files
  filter(!str_detect(file, "cuebiq")) %>% 
  ## exclude files calling above
  filter(!str_detect(file, "1_2_merge_main_datasets")) %>% 
  ## file need API key:
  filter(file!="0_clean_map_us_counties.R") %>%
  ## exclude own file
  filter(file!="master_run_all.R")


## order files now
files_keep_order <- files_keep %>% 
  mutate(folder = basename(dirname(full_path)),
         order = case_when(file=="0_clean_map_us_counties.R"~1,
                           file == "0_clean_us_states.R"~2,
                           TRUE ~NA)) %>% 
  arrange(folder, order, file)

files_keep_order


################################
#'## Run
################################

## aux fun
source_throw <- function (path, echo = TRUE, all.names = TRUE) {
  gc()
  mem_before <- pryr::mem_used()
  pkgs_before <- .packages()
  env_random <- new.env(parent = .GlobalEnv)
  sys <- purrr::safely(~suppressMessages(sys.source(., 
                                                    envir = env_random, 
                                                    keep.source = FALSE, 
                                                    keep.parse.data = FALSE)))(path)
  ls_env <- ls(envir = env_random, all.names = all.names)
  ggplot2::set_last_plot(NULL)
  rm(list = ls_env, envir = env_random)
  rm(env_random)
  
}


### Now run
out <- files_keep_order %>% 
  ## don't download every time
  filter(file!="0_clean_us_states.R") %>% 
  # head(10) %>%
  mutate(run_result = map_safely(full_path, ~source_throw(.)))
out


## check errors
has_errors(out$run_result)

write_rds(out, "/home/covid19/degree_flexibility_covid_meta/results_rerun_raw.rds")
# out <- read_rds("/home/covid19/degree_flexibility_covid_meta/results_rerun_raw.rds")

if(any(has_errors(out$run_result))){
  errors <- out %>% 
    filter(has_errors(run_result)) %>% 
    mutate(res_error = map_chr(run_result, ~pluck(., "error") %>% as.character)) %>% 
    select(file, res_error)
  
  errors
  errors$res_error
}





