format_unit_of_analysis_name <- function (unit_of_analysis) {
  stringr::str_replace_all(unit_of_analysis, c(` ` = "_", `%` = "Pct", `-` = "_"))
}


get_unit_of_analysis <- function (requested_unit, names = FALSE) {
  ll = list(`% Change Power` = "percentage", `% Change Amplitude` = "sqrt_percentage", 
            `z-score Power` = "zscore", `z-score Amplitude` = "sqrt_zscore", 
            decibel = "decibel")
  if (missing(requested_unit)) {
    if (names) 
      return(names(ll))
    return(ll)
  }
  if (!any(requested_unit == names(ll))) {
    warning("requested unit of analysis not available: ", 
            requested_unit, ". Returning % Change Power")
    return(ll[["% Change Power"]])
  }
  return(ll[[requested_unit]])
}

