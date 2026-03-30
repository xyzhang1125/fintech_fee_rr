print_N <- function(x, .file) {
  require(magrittr, quietly = TRUE) 
  require(stringr, quietly = TRUE)
  
  x <- as.numeric(x) # to not need to manually as.numeric, eg when pulling from a data.table
  
  prettyNum(x, big.mark = ",") %T>% 
    print() %>% # prior to adding the % for Latex
    str_c(., "%") %>% # this % is to prevent space after number
    write(.file) 
}
