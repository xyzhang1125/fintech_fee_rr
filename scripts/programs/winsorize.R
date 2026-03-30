winsorize <- function(dt, outcome, newvar, by = NULL, w = 5, highonly = FALSE, na.rm = FALSE) {
  require(data.table)
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
