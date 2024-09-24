#' ---
#' title: "Rerun all files"
#' author: "Matthieu"
#' date: 2024-09-08
#' ---

library(tidyverse)
library(tictoc)

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
  ## exclude file need API key:
  filter(file!="0_clean_map_us_counties.R") %>%
  ## exclude own file
  filter(file!="master_run_all.R")

## exclude heavy files
run_heavy <- FALSE
if(!run_heavy){
  files_keep <- files_keep %>% 
    filter(!str_detect(file, "2_regression_event|4_bacon_decomp_in|3_events_all_outc"))
}

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
  cat("Running file: ", path, "\n")
  tic()
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
  a <- toc()
  sys$run_time_elapsed <- a$toc-a$tic
  sys
}

out_TEST1 <- source_throw(path=files_keep_order$full_path[[3]])


### Now run
cat("Running scripts on ", as.character(Sys.Date()), "\n")

tic("Total run time")
out <- files_keep_order %>% 
  ## don't download every time
  filter(file!="0_clean_us_states.R") %>% 
  # head(3) %>%
  mutate(run_result = map(full_path, ~source_throw(.)))
out

## clean
out_c <- out %>% 
  bind_cols(purrr::transpose(pull(., run_result)) %>% 
              as_tibble)%>% 
  dplyr::select(-run_result) %>% 
  mutate(run_time_elapsed = unlist(run_time_elapsed),
         has_error = map_lgl(error,  ~length(.) > 0), 
         error = map_chr(.data$error, ~if (length(.) == 0) NA_character_
                         else paste(unique(.), collapse = " AND ")),
         run_time=Sys.time())
toc()

## check errors
any(out_c$has_error)

## print timing
out_c %>% 
  arrange(desc(run_time_elapsed))



### export
write_rds(out_c, "/home/covid19/degree_flexibility_covid_meta/results_rerun_raw.rds")

if(any(out_c$has_error)){
  out_c <- read_rds("/home/covid19/degree_flexibility_covid_meta/results_rerun_raw.rds") 
  out_c_err <- out_c%>% 
    dplyr::filter(has_error) 
  out_c_err %>% 
    dplyr::select(-file, -folder, -order, -result)
  out_c_err$error
  
  ## strange error?
  check <- source_throw(path=filter(files_keep_order, str_detect(file, "tab_3"))$full_path[[1]])
}

total_time_min <- sum(out_c$run_time_elapsed) %/% 60
total_time_hour <- total_time_min %/% (60)
min_left <- total_time_min %% (60)
paste("run in", total_time_hour, "hours", min_left, "minutes")

### heavy files
out_c %>% 
  arrange(desc(run_time_elapsed)) %>% 
  mutate(run_time_elapsed_min = run_time_elapsed%/% 60) %>% 
  dplyr::select(file, run_time_elapsed_min) %>% 
  filter(run_time_elapsed_min>10)

