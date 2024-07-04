
### table to pdf simplified
## problem with undetrscore, need ot add babel!? see https://tex.stackexchange.com/questions/121416/putting-an-underscore-in-a-label
prj_table_to_pdf <- function(path_tex,
                             plus ="\\usepackage{booktabs}\n\\usepackage{dcolumn}\n\\usepackage{underscore}\n\\usepackage[english]{babel}"){
  path_pdf <- str_replace(path_tex, "\\.tex$", ".pdf")
  
  mat_table_to_pdf(x = path_tex,
                   filename = path_pdf,
                   is_path_x = TRUE, copy.mode = FALSE,
                   plus=plus)
}


