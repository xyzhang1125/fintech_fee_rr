print_as_is <- function(x, .file, round = 1, commas = FALSE, is_negative = FALSE) {
  require(magrittr, quietly = TRUE) 
  require(stringr, quietly = TRUE)
  
  x <- as.numeric(x) # to not need to manually as.numeric, eg when pulling from a data.table
  if (x < 0) is_negative <- TRUE
  if (is_negative == TRUE) {
    char <- "-"
  } else {
    char <- ""
  } 
  if (commas == FALSE) {
    round(x, digits = round) %T>% 
      print() %>% # prior to adding the % for Latex
      str_c(char, ., "%") %>% # this % is to prevent space after number; not the \% in Latex
      write(.file) 
  } else {
    round(x, digits = round) %>%
      prettyNum(big.mark = ",") %T>% 
      print() %>% # prior to adding the % for Latex
      str_c(char, ., "%") %>% # this % is to prevent space after number; not the \% in Latex
      write(.file)     
  }
}
