# This script uses and speeds up the original Goodman Bacon decomposition
# starting from https://github.com/evanjflack/bacondecomp

bacon_orig <- function(formula,
                  data,
                  id_var,
                  time_var, 
                  quietly = F) {
  # Evaluate formula in data environment
  formula <- formula(terms(formula, data = data))
  
  # Unpack variable names and rename variables
  vars <- unpack_variable_names(formula)
  outcome_var <- vars$outcome_var
  treated_var <- vars$treated_var
  control_vars <- vars$control_vars
  data <- rename_vars(data, id_var, time_var, outcome_var, treated_var)
  
  # Check for a balanced panel
  bal <- aggregate(time ~ id,  data = data, FUN = length)
  balanced <- ifelse(all(bal$time == bal$time[1]), 1, 0)
  if (!balanced) {
    stop("Unbalanced Panel")
  }
  
  # Check for NA observations
  nas <- sum(is.na(data[, c("id", "time", "outcome", "treated")]))
  if (length(control_vars > 0)) {
    control_formula <- update(
      formula,
      paste0("treated ~ . - 1 - ", treated_var)
    )
    mm_control <- model.matrix(control_formula, data = data)
    nas_control <- 1 - (nrow(mm_control) == nrow(data))
    nas <- nas + nas_control
  }
  if (nas > 0) {
    stop("NA observations")
  }
  
  # Create 2x2 grid of treatment groups
  treatment_group_calc <- create_treatment_groups(data, control_vars,
                                                  return_merged_df = TRUE)
  two_by_twos <- treatment_group_calc$two_by_twos
  data <- treatment_group_calc$data
  
  # Uncontrolled 
  if (length(control_vars) == 0) {
    # Iterate through treatment group dyads
    for (i in 1:nrow(two_by_twos)) {
      treated_group <- two_by_twos[i, "treated"]
      untreated_group <- two_by_twos[i, "untreated"]
      
      data1 <- subset_data(data, treated_group, untreated_group)
      
      # Calculate estimate and weight
      weight <- calculate_weights(data = data1,
                                  treated_group = treated_group,
                                  untreated_group = untreated_group)
      estimate <- lm(outcome ~ treated + factor(time) + factor(id),
                     data = data1)$coefficients[2]
      
      two_by_twos[i, "estimate"] <- estimate
      two_by_twos[i, "weight"] <- weight
    }
    
    # Rescale weights to sum to 1
    two_by_twos <- scale_weights(two_by_twos)
    
    if (quietly == F) {
      print_summary(two_by_twos)
    }
    
    return(two_by_twos)
    
  } else if (length(control_vars) > 0) {
    # Controled 
    # Predict Treatment and calulate demeaned residuals
    control_formula <- update(
      formula,
      paste0("treated ~ . + factor(time) + factor(id) -", treated_var)
    )
    
    data <- run_fwl(data, control_formula)
    
    # Calculate within treatment group estimate and its weight
    Omega <- calculate_Omega(data)
    beta_hat_w <- calculate_beta_hat_w(data)
    
    # Collapse controls and predicted treatment to treatment group/year level
    r_collapse_x_p <- collapse_x_p(data, control_formula)
    data <- r_collapse_x_p$data
    g_control_formula <- r_collapse_x_p$g_control_formula
    
    # Iterate through treatment group dyads
    for (i in 1:nrow(two_by_twos)) {
      treated_group <- two_by_twos[i, "treated"]
      untreated_group <- two_by_twos[i, "untreated"]
      data1 <- data[data$treat_time %in% c(treated_group, untreated_group), ]
      
      # Calculate between group estimate and weight
      weight_est <- calc_controlled_beta_weights(data1, g_control_formula)
      s_kl <- weight_est$s_kl
      beta_hat_d_bkl <- weight_est$beta_hat_d_bkl
      
      two_by_twos[i, "weight"] <- s_kl
      two_by_twos[i, "estimate"] <- beta_hat_d_bkl
    }
    
    # Rescale weights to sum to 1
    two_by_twos <- scale_weights(two_by_twos)
    
    if (quietly == F) {
      print_summary(two_by_twos)
    }
    
    r_list <- list("beta_hat_w" = beta_hat_w,
                   "Omega" = Omega,
                   "two_by_twos" = two_by_twos)
    return(r_list)
  }
}

unpack_variable_names <- function(formula) {
  outcome_var <- base::as.character.default(formula)[2]
  right_side_vars <- base::as.character.default(formula)[3]
  right_side_vars <- strsplit(right_side_vars, " \\+ ")[[1]]
  treated_var <- right_side_vars[1]
  control_vars <- right_side_vars[-1]
  r_list <- list(outcome_var = outcome_var, treated_var = treated_var,
                 control_vars = control_vars)
  return(r_list)
}

bacon_decomp_custom <- function(formula,
                                data,
                                id_var,
                                time_var, 
                                quietly = FALSE) {
  # Evaluate formula in data environment
  formula <- formula(terms(formula, data = data))
  
  # Unpack variable names and rename variables
  vars <- unpack_variable_names(formula)
  if(is.na(vars$outcome_var)) warning("Problem with formula!")
  outcome_var <- vars$outcome_var
  treated_var <- vars$treated_var
  control_vars <- vars$control_vars
  data <- bacondecomp:::rename_vars(data, id_var, time_var, outcome_var, treated_var)
  
  # Check for a balanced panel
  bal <- aggregate(time ~ id,  data = data, FUN = length)
  balanced <- ifelse(all(bal$time == bal$time[1]), 1, 0)
  if (!balanced) {
    stop("Unbalanced Panel")
  }
  
  # Check for NA observations
  nas <- sum(is.na(data[, c("id", "time", "outcome", "treated")]))
  if (length(control_vars > 0)) {
    control_formula <- update(
      formula,
      paste0("treated ~ . - 1 - ", treated_var)
    )
    mm_control <- model.matrix(control_formula, data = data)
    nas_control <- 1 - (nrow(mm_control) == nrow(data))
    nas <- nas + nas_control
  }
  if (nas > 0) {
    stop("NA observations")
  }
  
  # Create 2x2 grid of treatment groups
  treatment_group_calc <- bacondecomp:::create_treatment_groups(data, 
                                                                control_vars,
                                                                return_merged_df = TRUE)
  two_by_twos <- as_tibble(treatment_group_calc$two_by_twos) %>% 
    mutate(across(c(weight, estimate), as.numeric),
           means=list(tibble()))
  data <- treatment_group_calc$data
  
  # Uncontrolled
  if (length(control_vars) == 0) {
    # Iterate through treatment group dyads
    for (i in 1:nrow(two_by_twos)) {
      treated_group <- two_by_twos[i, "treated", drop=TRUE]
      untreated_group <- two_by_twos[i, "untreated", drop=TRUE]
      
      data1 <- bacondecomp:::subset_data(data, treated_group, untreated_group)
      
      # Calculate estimate and weight
      weight <- bacondecomp:::calculate_weights(data = data1,
                                                treated_group = treated_group,
                                                untreated_group = untreated_group)
      all_means_df <- all_means(data1, timing_treat=treated_group) 
      estimate <- lfe::felm(outcome ~ treated |time+ id,data = data1)
      
      two_by_twos[i, "estimate"] <- coef(estimate)
      two_by_twos[i, "weight"] <- weight
      two_by_twos[i, "means"] <- list(list(all_means_df))
      
    }
    
    # Rescale weights to sum to 1
    two_by_twos <- bacondecomp:::scale_weights(two_by_twos)
    
    if (quietly == FALSE) {
      bacondecomp:::print_summary(two_by_twos)
    }
    res <- two_by_twos
    
  } else if (length(control_vars) > 0) {
    # Controled
    # Predict Treatment and calulate demeaned residuals
    control_formula <- update(
      formula,
      paste0("treated ~ . + factor(time) + factor(id) -", treated_var)
    )
    
    data <- bacondecomp:::run_fwl(data, control_formula)
    
    # Calculate within treatment group estimate and its weight
    Omega <- bacondecomp:::calculate_Omega(data)
    beta_hat_w <- bacondecomp:::calculate_beta_hat_w(data)
    
    # Collapse controls and predicted treatment to treatment group/year level
    r_collapse_x_p <- bacondecomp:::collapse_x_p(data, control_formula)
    data <- r_collapse_x_p$data
    g_control_formula <- r_collapse_x_p$g_control_formula
    
    # Iterate through treatment group dyads
    for (i in 1:nrow(two_by_twos)) {
      treated_group <- two_by_twos[i, "treated", drop=TRUE]
      untreated_group <- two_by_twos[i, "untreated", drop=TRUE]
      data1 <- data[data$treat_time %in% c(treated_group, untreated_group), ]
      
      # Calculate between group estimate and weight
      weight_est <- bacondecomp:::calc_controlled_beta_weights(data1, g_control_formula)
      s_kl <- weight_est$s_kl
      beta_hat_d_bkl <- weight_est$beta_hat_d_bkl
      
      all_means_df <- all_means(data1, timing_treat=treated_group) 
      
      two_by_twos[i, "weight"] <- s_kl
      two_by_twos[i, "estimate"] <- beta_hat_d_bkl
      two_by_twos[i, "means"] <- list(list(all_means_df))
    }
    
    # Rescale weights to sum to 1
    two_by_twos <- bacondecomp:::scale_weights(two_by_twos)
    
    if (quietly == FALSE) {
      bacondecomp:::print_summary(two_by_twos)
    }
    
    res <- list("beta_hat_w" = beta_hat_w,
                "Omega" = Omega,
                "two_by_twos" = two_by_twos)
    
  }
  res
}

all_means <- function(df, timing_treat){
  sort(unique(df$time))
  df %>% 
    mutate(treatment_period=if_else(time<timing_treat, "pre", "post") %>% 
             factor() %>% 
             fct_relevel("pre"),
           treat_order=fct_relevel(factor(treat_time), as.character(timing_treat))) %>% 
    rename(group=treat_time) %>% 
    group_by(group, treat_order, time, treatment_period) %>% 
    summarise(mean=mean(outcome, na.rm=TRUE),
              # treatment_period=unique(treated),
              .groups="drop_last") %>% 
    ungroup()  %>% 
    arrange(treat_order, time)
}


get_any <- function(df, after=0, before=1, instant=TRUE, print=FALSE, warn=TRUE){
  time_avail <- unique(df$time)
  range <- diff(range(time_avail))
  if(is.null(after)) after <- range
  if(is.null(before)) before <- range
  treat_time <- min(filter(df, treatment_period=="post")$time)
  time_keep <- (treat_time-before):(treat_time+after)
  if(!instant) time_keep <- time_keep[time_keep!=treat_time]
  time_keep_keep <- time_keep[time_keep%in%time_avail]
  if(!all(time_keep%in%time_avail) & warn) warning("Missing some periods")
  
  if(print) print(paste("Treatment at:", treat_time, "looking at: ", paste(time_keep_keep, collapse = ",")))
  dat_DID <- df %>%
    filter(time %in% time_keep_keep) %>% 
    arrange(treat_order, time) %>% 
    group_by(treat_order, treatment_period) %>%
    summarise(mean=mean(mean), .groups="drop_last") %>%
    ungroup() %>%
    spread(treat_order, mean) %>%
    mutate(diff_group=.[,2,drop=TRUE]-.[,3,drop=TRUE])
  
  ## check enough
  if(nrow(dat_DID)<2){
    if(warn) warning("Not enough values")
    return(NA_real_)
  }
  diff(dat_DID$diff_group)
}

get_chaise <- function(df, ...){ 
  get_any(df, after=0, before=1, instant=TRUE, ...)
}

get_jojo1 <- function(df, ...){ 
  get_any(df, after=1, before=1, instant=FALSE, ...)
}

get_bacon <- function(df, ...){ 
  get_any(df, after=NULL, before=NULL, instant=TRUE, ...)
}

get_7 <- function(df, ...){ 
  get_any(df, after=7, before=NULL, instant=TRUE, ...)
}
