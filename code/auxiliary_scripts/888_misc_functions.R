prj_formu_to_char <- function (x, ...) {
  form <- paste(deparse(x), collapse = " ")
  form <- gsub("\\s+", " ", form, perl = FALSE)
  return(form)
}

prj_head_byVar <- function(df, var,n_head=NULL, n_slice=NULL){
  dst <- distinct(df, {{var}})
  n_dst <-  nrow(dst)
  if(!is.null(n_head) & !is.null(n_slice)) stop("Not both")
  
  if(!is.null(n_head)){
    if(n_head>n_dst) warning("Too big")
    dst <- dst %>% 
      head(n_head)
  }
  if(!is.null(n_slice)){
    if(max(n_slice) > n_dst) warning("Too big")
    dst <- dst %>% 
      slice(n_slice)
  } 
  df %>% 
    semi_join(dst, by = rlang::as_name(rlang::ensym(var)))
  
}

if(FALSE){
  df <- tibble(group = rep(letters[1:5], each=2), value = 1:10)
  prj_head_byVar(df, group, 2)
  prj_head_byVar(df, group, n_slice=2)
  prj_head_byVar(df, group, 6)
}

### toc() with message, instead of message in tic()
my.msg.toc <- function(tic, toc, msg, info){
  outmsg <- paste(info, " Time: ",
                  round(toc - tic, 3), 
                  " seconds elapsed", sep="")
}


prj_toc <- function(info="done!") {
  tictoc::toc(info = info, func.toc=my.msg.toc)
}

if(FALSE){
  tic()
  Sys.sleep(1)
  prj_toc("Horrah!")
}

#' check if 0 rows
prj_check_0row <- function (df, message_ok = "OK", warn_message = NULL, stop_message = NULL, 
                            print_df = TRUE, n_row = 0) {
  if (nrow(df) == n_row) {
    return(message_ok)
  }
  else if (!is.null(warn_message)) {
    warning(warn_message)
  }
  else {
    if (is.null(stop_message)) 
      stop_message <- paste("Does not have", n_row, "rows!")
    if (print_df) 
      print(df)
    stop(stop_message)
  }
}
