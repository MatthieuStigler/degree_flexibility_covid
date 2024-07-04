library(Formula)
frm_get_rhs <- function(f) {
  attr(terms(Formula::as.Formula(f), rhs=1), "term.labels")
}
frm_show_rhs <- function(f) {
  formula(Formula::as.Formula(f), rhs=1, lhs=0)
}

frm_get_covars_df <- function(f) {
  attr(terms(Formula::as.Formula(f), rhs=1), "term.labels") %>% 
    enframe(name=NULL, value="covariate")
}

frm_df_covars <- function(df, formu_var=formula, formu_num_var = formu_num) {
  df %>% 
    mutate(covariates = map_chr({{formu_var}}, ~frm_show_rhs(.) %>%  mat_formu_to_char)) %>% 
    distinct({{formu_num_var}}, covariates)  
}

frm_to_char <- function (x, ...) {
  form <- paste(deparse(x), collapse = " ")
  form <- gsub("\\s+", " ", form, perl = FALSE)
  return(form)
}

## Update formula: substitute y with outcome in `y_var`
frm_y_to_outcome <- function(df, y_var =list_var){
  if(!all(c("formu_char", "formula") %in% colnames(df))) stop("Expected variables 'formu_char' and 'formula'")

  
  res <- df %>% 
    mutate(formu_char = map2_chr({{y_var}}, formu_char, ~str_replace(.y, "y ~", paste(.x, " ~")) %>% 
                                   str_squish()),
           formula = map2({{y_var}}, formula, ~update_F(Formula::as.Formula(.y),
                                                           as.formula(paste(.x, " ~.")))))
  
  ## check
  check_df <- res %>% 
    rowwise() %>% 
    mutate(formu_check = frm_to_char(formula)) %>%
    ungroup() %>% 
    select(formula, formu_check, formu_char) %>% 
    filter(formu_check!=formu_char)
  if(nrow(check_df)!=0){
    warning("Not equal formu_check and formu_char!?")
    print(check_df)
  }
  
  ## return
  res
  
}

frm_df_extend_y <- function(reg_df, y_df, y_var =list_var){
  
  # y_var =rlang::quo(list_var)
  ## would be better to simply use expand_grid(list_var_df, formus)!!
  res <- y_df %>% 
    mutate(formula_df = map({{y_var}}, ~reg_df %>% 
                              rowwise() %>% 
                              mutate(formu_char =str_replace(formu_char, "y ~", paste(.x, " ~")) %>% 
                                       str_squish(),
                                     formula=list(update_F(Formula::as.Formula(formula),
                                                              as.formula(paste(.x, " ~."))))))) %>%
    unnest(formula_df)
  
  ## check
  check_df <- res %>% 
    rowwise() %>% 
    mutate(formu_check = frm_to_char(formula)) %>%
    ungroup() %>% 
    select(formula, formu_check, formu_char) %>% 
    filter(formu_check!=formu_char)
  if(nrow(check_df)!=0){
    warning("Not equal formu_check and formu_char!?")
    print(check_df)
  }
  res
}


frm_felm_to_fxt <- function(f){
  part_iv <- terms(Formula::as.Formula(f), rhs=3, lhs=0) %>% 
    attr("term.labels")
  if(length(part_iv)!=0) warning("Has IV!? Don't know how to convert to fixest")
  part_se <- terms(Formula::as.Formula(f), rhs=4, lhs=0) %>% 
    attr("term.labels")
  if(length(part_se)>1) warning("Has many clusters? Use se='twoway'")
  formula(Formula::as.Formula(f), rhs=1:2)
}

if(FALSE){
  frm_felm_to_fxt(log(Euros) ~ log(dist_km)|Origin+Destination|0|Origin+Mama)
  frm_felm_to_fxt(log(Euros) ~ log(dist_km)|Origin+Destination|0|Origin)
}

################################
#'## Low level utilities
################################

update_mine <- function(old, new, ...) 
{
  tmp <- .Call(stats:::C_updateform, as.formula(old), as.formula(new))
  out <- formula(terms.formula(tmp, keep.order = TRUE, simplify = TRUE))
  return(out)
}


update_F <- function (object, new, ...) 
{
  new <- Formula(new)
  o_lhs <- attr(object, "lhs")
  o_rhs <- attr(object, "rhs")
  n_lhs <- attr(new, "lhs")
  n_rhs <- attr(new, "rhs")
  lhs <- rep(list(NULL), length.out = max(length(o_lhs), length(n_lhs)))
  rhs <- rep(list(NULL), length.out = max(length(o_rhs), length(n_rhs)))
  update_components <- function(x, y) {
    xf <- yf <- ~.
    xf[[2L]] <- x
    yf[[2L]] <- y
    update_mine(xf, yf)[[2L]]
  }
  if (length(lhs) > 0L) 
    for (i in 1L:length(lhs)) {
      lhs[[i]] <- if (length(o_lhs) < i) 
        n_lhs[[i]]
      else if (length(n_lhs) < i) 
        o_lhs[[i]]
      else update_components(o_lhs[[i]], n_lhs[[i]])
    }
  if (length(rhs) > 0L) 
    for (i in 1L:length(rhs)) {
      rhs[[i]] <- if (length(o_rhs) < i) 
        n_rhs[[i]]
      else if (length(n_rhs) < i) 
        o_rhs[[i]]
      else update_components(o_rhs[[i]], n_rhs[[i]])
    }
  rval <- Formula:::paste_formula(lhs, rhs)
  rval <- Formula(rval)
  environment(rval) <- environment(object)
  return(rval)
}


