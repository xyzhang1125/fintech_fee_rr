print_as_percent <- function(
    x, 
    .file, 
    round = 0, 
    type = NULL, 
    abs = FALSE, 
    is_negative = FALSE,
    double_negative = TRUE
  ) {
  
  require(magrittr) 
  require(stringr)
  
  x <- as.numeric(x) # to not need to manually as.numeric, eg when pulling from a data.table
  
  if (!is.null(type)) {
    if (type == "log") x <- exp(x) - 1 # convert log to percent change
  } 
  if (abs) {
    x <- abs(x) # for saying things like "fell by" in the text; don't want negative sign
  } else {
    if (x < 0) is_negative <- TRUE
  }
  if (is_negative) {
    if (double_negative) {
      prefix <- "-"
    } else {
      prefix <- ""
    }
    
    round(x*100, digits = round) %>% 
      format(nsmall = round) %>% # to get it to display decimal places even if 0
      str_c(prefix, ., "%") %T>% 
        # add an extra negative sign for latex so now it's --
        # this % is to prevent space after number; not the \% in Latex
      write(.file) %>%
      print() # includes the "%" in printing since it's a percent    
  } else {
    round(x*100, digits = round) %>% 
      format(nsmall = round) %>% # to get it to display decimal places even if 0
      str_c(., "%") %T>% # this % is to prevent space after number; not the \% in Latex
      write(.file) %>%
      print() # includes the "%" in printing since it's a percent
  }
}
