# FUNCTIONS

# Create lag variable or delta variable in a data.table
create_lag <- function(dt, var, newvar, index, delta = FALSE, lag = 1) {
  temp_p <- dt %>% plm::pdata.frame(index = index)
  if (delta == TRUE) {
    dt[, (newvar) := temp_p[[var]] - plm::lag(temp_p[[var]])]
  } else {
    dt[, (newvar) := plm::lag(temp_p[[var]], lag = lag)]
  }
}


time_stamp <- function() {
  datestamp <- Sys.time() %>% 
      str_sub(1, 10) %>% 
      str_replace_all("-", "")
  timestamp <- Sys.time() %>% 
    str_sub(12, 19) %>% 
    str_replace_all(":", "")
  str_c("_", datestamp, "_", timestamp)
}

no_special <- function(x) {
  x %>% 
    str_replace_all(" ", "") %>% 
    str_replace_all("/", "") %>% 
    str_replace_all("\\.", "") %>% # escape it
    str_replace_all("-", "") %>% 
    str_replace_all("á", "a") %>% 
    str_replace_all("é", "e") %>% 
    str_replace_all("í", "i") %>%
    str_replace_all("ó", "o") %>% 
    str_replace_all("ú", "u") %>% 
    str_replace_all("ñ", "n")  
    # can add more here as needed
}

winsorize <- function(dt, outcome, newvar, by = NULL, w = 5, highonly = FALSE, na.rm = FALSE) {
  if (is.null(by)) {
    dt[, dummy := 1]
    by = "dummy"
  }
  dt[, winlevel_hi := quantile(eval(parse(text = outcome)), probs = 1 - w/100, na.rm = na.rm), by = by]
  dt[, winlevel_lo := quantile(eval(parse(text = outcome)), probs = w/100, na.rm = na.rm), by = by]
  if (highonly) {
    dt[, (newvar) := ifelse(eval(parse(text = outcome)) > winlevel_hi, winlevel_hi, 
      eval(parse(text = outcome))
    )]
  } else {
    dt[, (newvar) := ifelse(eval(parse(text = outcome)) > winlevel_hi, winlevel_hi, 
      ifelse(eval(parse(text = outcome)) < winlevel_lo, winlevel_lo, eval(parse(text = outcome))
    ))]    
  }
  winlevels <- dt %>% select_colnames("winlevel")
  dt[, (winlevels) := NULL]
  dt
}

# For factors that should have been numeric
fac_to_num <- function(x) { # see http://bit.ly/2x26lUU
  as.numeric(levels(x))[x] # faster than as.numeric(as.character(x))
}
fac_to_int <- function(x) { # see http://bit.ly/2x26lUU
  as.integer(levels(x))[x] # faster than as.integer(as.character(x))
}
check_convert <- function(x, FUN=fac_to_int) {
  if (class(x)=="factor") {
    FUN(x)
  }
  else {
    x
  }
}
# NAs to 0 (if NA, no added or dropped POS terminals that day)
na_to_0 <- function(x, reverse = FALSE) {
  if (reverse) {
    ifelse(x == 0, NA, x)
  } else {
    ifelse(is.na(x), 0, x)
  }
}
# Lowercase names
lowercase_names <- function(df) {
  require(stringr)
  require(purrr)
  names(df) <- map_chr(names(df), str_to_lower)
  df # return
}
select_colnames <- function(df, pattern, ...) {
  if (length(list(...)) > 0) pattern <- str_c(c(pattern, ...), collapse = "|")
  names(df)[names(df) %>% map_lgl(function(x) str_detect(x, pattern) %>% any())]
    # the any() is so that pattern can have multiple elements, 
    #  and it just looks for one of the elements
    # which means you can either specify as select_colnames(df, c("a", "b"))
    #  or because of the ..., as select_colnames(df, "a", "b")
}
select_elements <- function(x, pattern) {
  x[x %>% map_lgl(function(x) str_detect(x, pattern))]
}
select_cols <- function(df, string) {
  df %>% .[, which(purrr::map_lgl(names(.), stringr::str_detect, string))]
} # Added this because it works with both data.table and tibble, 
  #  whereas select(contains()) only works with tibble

# Print all rows of tibble/data.table/etc.
print_all <- function(x) {
  print(x, n = Inf)
}

# Read transposed csv (each row represents a variable)
read.transposed.xlsx <- function(file, sheetIndex = 1) { # http://bit.ly/2yeQkIB
  require(xlsx)
  df <- read.xlsx(file, sheetIndex = sheetIndex , header = FALSE)
  dft <- as.data.frame(t(df[-1]), stringsAsFactors = FALSE) 
  names(dft) <- df[,1] 
  # dft <- as.data.frame(lapply(dft,type.convert))
  return(dft)            
}

# Read dbf as tibble, lowercase var names
read_dbf <- function(x, lower = TRUE, ...) {
  require(foreign)
  require(dplyr)
  require(stringr)
  a <- read.dbf(x, ...) %>% as_tibble()
  if (lower) {
    names(a) <- str_to_lower(names(a))
  }
  a
}

# Format numbers
formatted <- function(x) format(x, nsmall = 2, big.mark = ",")
  # http://r4ds.had.co.nz/r-markdown.html

