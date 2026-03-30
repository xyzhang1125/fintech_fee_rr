# Strip the normal fixed effects regressions to bare minimum
strip_feols_vars <- c("nobs", "nobs_origin", "fml", "method", "method_type",
                      "fml_all", "nparams", "fixef_vars", "residuals", "sigma2",
                      "coeftable", "ssr_null", "ssr", "ssr_fe_only", "summary")
strip_feols <- function(x) {
  x <- x[strip_feols_vars]
  class(x) <- "fixest"
  return(x)
}

# Strip instrumental variables  regressions with fixed effects
strip_feols_iv_vars = strip_feols_vars %>% append(c("fixef_id", "scores", "cov.unscaled",
                                                    "coefficients", "cov.scaled")  )
strip_feols_iv <- function(x) {
  x <- x[strip_feols_iv_vars]
  class(x) <- "fixest"
  return(x)
}


  