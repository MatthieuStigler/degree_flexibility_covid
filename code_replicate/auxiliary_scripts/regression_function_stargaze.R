suppressMessages(require(stargazer))
require(tidyverse)



## Function tom run tables
#'@param model_name string to add to output path
#'@param list_var_data a df with columns 'list_var' and 'list_names'
#'@param echo Should show the output?
stargaze_reg_jojo <- function(out_var_chr, df_dat=df, reg_df=out, 
                              get_vars_order = NULL,
                              get_vars_names = NULL,
                              list_var_data = list_var_df, 
                              list_var_name=list_var,
                              y_name_var = list_names,
                              model_name = NULL, echo=FALSE, 
                              title = "Preliminary models",
                              path_base = "", ...){
  
  ## clean variable name
  out_var <- rlang::sym(out_var_chr)
  out_var_cute_name <- list_var_data %>% 
    filter({{list_var_name}}==out_var_chr) %>% 
    pull({{y_name_var}})
  
  ## get mean
  mean_control_state <- df_dat %>% 
    filter(!is.na({{out_var}})) %>% 
    filter(State_ED==0) %>% 
    dplyr::summarise(mean =mean({{out_var}}, na.rm=TRUE)) %>% 
    pull(mean) %>% 
    round(2)

  
  ## get regs, gaze it
  regs_li <- reg_df %>% 
    filter({{list_var_name}}==out_var_chr) %>% 
    pull(reg) 
  n_regs <- length(regs_li)
  if(n_regs==0) warning("failed to filer any reg from list_var_name")
  path_out <- paste0(path_base,out_var_chr,model_name,".tex")
  out <- invisible(capture.output(stargazer(regs_li,
                                            out = path_out, header = FALSE, 
                                            title = title,
                                            single.row = FALSE,
                                            no.space = TRUE,
                                            keep = "^((?!factor).)*$", #Remove all variables that contain string "factor"
                                            perl = TRUE,
                                            dep.var.labels = out_var_cute_name,
                                            order=paste0("^", get_vars_order , "$"),
                                            add.lines = list(c("Day FE", rep("Yes", n_regs)),
                                                             c("County FE", rep("Yes", n_regs)),
                                                             c("Mean control", rep(mean_control_state, n_regs))),
                                            covariate.labels = get_vars_names,
                                            omit.stat = c("rsq","ser", "adj.rsq"), ...)))
  if(echo) print(out)
  print(paste("Done with ", out_var_chr, " saved as:", path_out))
}


################################
#'## New version
################################

path_base = ""

stargaze_reg_df_all <- function(df_regs=out, 
                                df_covars=table_covars,
                                df_outcomes=table_vars,
                                df_data= df, 
                                has_log = TRUE,
                                outcome_column = if(has_log) "list_var_log" else "list_var",
                                path_base = "",
                                echo=FALSE, 
                                float.env="table", float=TRUE){
  outcome_column_quo <- rlang::sym(outcome_column)
  df_regs_nst <- df_regs %>%
    group_by({{outcome_column_quo}}) %>%
    nest() %>% 
    mutate(data = map2(data, {{outcome_column_quo}}, ~mutate(.x, {{outcome_column_quo}}:=.y))) # 
  
  walk(df_regs_nst$data, ~stargaze_reg_df_one(df_regs=., 
                                              df_covars=df_covars,
                                              df_outcomes=df_outcomes,
                                              df_data=df_data,
                                              outcome_column=outcome_column,
                                              path_base=path_base, 
                                              float.env=float.env, float=float))
    
    
}

#' Meta function to run many stargaze_reg_df_one
stargaze_reg_df_all_diffLogs <- function(df_regs=out, 
                                         df_covars=table_covars,
                                         df_outcomes=table_vars,
                                         df_data= df, 
                                         outcome_column = "list_var", #if(has_log) "list_var_log" else "list_var",
                                         path_base = "",
                                         echo=FALSE, 
                                         float.env="table", float=TRUE){
  outcome_column_quo <- rlang::sym(outcome_column)
  df_regs_nst <- df_regs %>%
    group_by({{outcome_column_quo}}, has_log) %>%
    nest() %>% 
    mutate(data = map2(data, {{outcome_column_quo}}, ~mutate(.x, {{outcome_column_quo}}:=.y))) 
  
  walk2(df_regs_nst$data,df_regs_nst$has_log,
        ~stargaze_reg_df_one(df_regs=.x, 
                             df_covars=df_covars,
                             df_outcomes=df_outcomes,
                             df_data=df_data,
                             has_log = .y,
                             path_base=path_base,
                             float.env=float.env, float=float))
  
  
}

#' Main function now
stargaze_reg_df_one <- function(df_regs=out, 
                                df_covars=table_covars,
                                df_outcomes=table_vars,
                                df_data= df, 
                                has_log = TRUE,
                                outcome_column = if(has_log) "list_var_log" else "list_var",
                                path_base = "",
                                title_prefix = "FE2",
                                echo=FALSE, 
                                float.env="table", float=TRUE,
                                ...
                              ){
  
  ## get outcome name
  outcome_column_quo <- rlang::sym(outcome_column)
  out_var_chr <- unique(pull(df_regs, !!outcome_column_quo))
  if(length(out_var_chr)>1) stop("Multiple outcomes in same data!")
  
  n_regs <- nrow(df_regs)
  list_names_var <- if(has_log) "list_names_log" else "list_names"
  list_names_var_quo <- rlang::sym(list_names_var)
  
  ## now retrieve outcome name
  out_var_cute_name <- df_outcomes %>% 
    filter(!!outcome_column_quo ==out_var_chr) %>% 
    pull(!!list_names_var_quo)
  
  ## title name
  title <- paste(title_prefix, out_var_cute_name)

  ## LaTeX cleaning: % to \%
  out_var_cute_name <- out_var_cute_name %>% 
    str_replace("(\\%)", "\\\\\\1")
  
  ## covariate names
  all_coefs <- unlist(map(df_regs$reg, ~names(coef(.)))) %>% 
    unique() %>% 
    enframe(name=NULL, value="coef_reg_name") %>% 
    mutate(covariate_name=    str_remove(coef_reg_name, "TRUE$")) %>% 
    left_join(df_covars, by = "covariate_name") %>% 
    mutate(order  = c(1,2,3,4,5,6,7,8,9,10,11), # didn't work
      re_order=rank(order),
           covariate_name_clean_tex= str_replace(covariate_name_clean, "> 0", "$>0$"))
    
  covar_names_cute <- arrange(all_coefs, re_order)$covariate_name_clean_tex
  covar_names_raw <- arrange(all_coefs, re_order)$covariate_name
  
  covar_order <- all_coefs$re_order
  
  
  
  ## path_out
  path_out <- paste0(path_base, out_var_chr, ".tex")
  
  
  ## get mean
  mean_control_state <- df_data %>% 
    filter(Date<as.Date("2020-02-14")) %>%
    dplyr::summarise(mean =mean(!!rlang::sym(out_var_chr), na.rm=TRUE)) %>% 
    pull(mean) %>% 
    round(2)
  
  ## get list of regs
  if(!"reg" %in% colnames(df_regs)) stop("Expecting column 'reg' in df")
  regs_li <- df_regs$reg
  
  ## now run!
  res_out <- invisible(capture.output(stargazer(regs_li,
                                                out = path_out,
                                                header = FALSE, 
                                                title = title,
                                                single.row = FALSE,
                                                no.space = TRUE,
                                                keep = "^((?!factor).)*$", #Remove all variables that contain string "factor"
                                                perl = TRUE,
                                                dep.var.labels = out_var_cute_name,
                                                add.lines = list(c("Day FE", rep("Yes", n_regs)),
                                                                 c("County FE", rep("Yes", n_regs))),
                                                covariate.labels = covar_names_cute,
                                                order=paste0("^", covar_names_raw, "$"),
                                                notes = "\\textit{Notes:} Standard errors clustered at the county level. $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01",
                                                notes.append=FALSE, notes.align="l", notes.label="",
                                                omit.stat = c("rsq","ser",  "adj.rsq"), 
                                                float.env=float.env, float=float, ...
                                                )))
  
  if(echo|any(str_detect(res_out, "Error"))) print(res_out)
  print(paste("Done with ", out_var_chr, " saved as:", path_out))
}


## Here we do cross outcomes, for same formula! 
## But formula selection has to be done prior, filtering on `out` before
stargaze_cross_formu_df_one <- function(df_regs=out, 
                                        df_covars=table_covars,
                                        df_outcomes=table_vars,
                                        df_data= df, 
                                        has_log = TRUE,
                                        outcome_column = if(has_log) "list_var_log" else "list_var",
                                        path_base = "",
                                        omit= c("factor", "tmean", "prcp", "snow"),
                                        echo=TRUE,
                                        float.env="table", float=TRUE, ...){
  
  ## find out name (temp)
  names_keep <- df_regs %>% 
    mutate(n_row=row_number()) %>% 
    select(n_row, has_log, starts_with("list_names")) %>% 
    gather(spec_type, name_outcome, starts_with("list_names")) %>% 
    filter((has_log & spec_type=="list_names_log") |(!has_log & spec_type=="list_names"))
  
  df_regs_c <- df_regs %>% 
    mutate(n_row=row_number()) %>% 
    left_join(names_keep %>% 
                select(n_row, name_outcome), by = c("n_row"))
  
  ## reg name
  form_num <- unique(df_regs_c$formu_num)
  form_cute <- paste("Formula", form_num)
  
  
  
  ## covariate names
  all_coefs <- unlist(map(df_regs_c$reg, ~names(coef(.)))) %>% 
    unique() %>% 
    enframe(name=NULL, value="coef_reg_name") %>% 
    mutate(covariate_name=    str_remove(coef_reg_name, "TRUE$")) %>% 
    left_join(df_covars, by = "covariate_name") %>% 
    mutate(re_order=rank(order))
  
  ## omit
  if(!is.null(omit)){
    omit_regexp <- paste0("^", omit, "$")
    all_coefs <- all_coefs %>% 
      filter(!covariate_name %in%omit)
  }
  
  covar_names_cute <- arrange(all_coefs, order)$covariate_name_clean
  covar_names_raw <- arrange(all_coefs, order)$covariate_name
  
  
  ## path_out
  path_out <- paste0(path_base, paste0("/formu_", form_num), ".tex")
  
  
  
  ## get list of regs
  if(!"reg" %in% colnames(df_regs)) stop("Expecting column 'reg' in df")
  regs_li <- df_regs$reg
  


  
  ## now run!
  res_out <- invisible(capture.output(stargazer(regs_li,
                                                out = path_out,
                                                header = FALSE, 
                                                title = form_cute,
                                                single.row = FALSE,
                                                no.space = TRUE,
                                                # keep = "^((?!factor).)*$", #Remove all variables that contain string "factor"
                                                omit=omit,
                                                perl = TRUE,
                                                multicolumn = FALSE,
                                                model.numbers = FALSE,
                                                dep.var.labels = df_regs_c$name_outcome,
                                                dep.var.labels.include=TRUE,
                                                add.lines = list(c("Day FE", rep("Yes", n_regs)),
                                                                 c("County FE", rep("Yes", n_regs))),#,
                                                                 # c("Mean control", rep(mean_control_state, n_regs))),
                                                covariate.labels = covar_names_cute,
                                                order=paste0("^", covar_names_raw, "$"),
                                                notes = "\\textit{Notes:} Standard errors clustered at the county level. $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01",
                                                notes.append=FALSE, notes.align="l", notes.label="",
                                                omit.stat = c("rsq","ser",  "adj.rsq"), 
                                                float.env=float.env, float=float,...)))
  
  if(echo) print(out)
  print(paste("Done with ", form_cute, " saved as:", path_out))
}
