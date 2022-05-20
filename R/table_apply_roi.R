#' @export
build_roi_levels <- function(data_table, model_roi_variable, 
                             ignore_hemisphere = FALSE, ignore_gyrus_sulcus = FALSE) {
  # source_data <- local_data$source_data
  if(!is.data.frame(data_table)) { return() }
  
  mrv <- model_roi_variable
  if(length(mrv) != 1 || !nchar(mrv)) { return() }
  
  col_name <- sprintf("%s%s", RAVE_ROI_KEY, mrv)
  if(!col_name %in% names(data_table)) { return() }
  
  lvls <- unique(data_table[[col_name]])
  if(any(is.na(lvls))) {
    lvls[is.na(lvls)] <- "N/A"
  }
  
  if(ignore_hemisphere) {
    lvls <- remove_hemisphere_labels(lvls)
  }
  
  if(ignore_gyrus_sulcus) {
    lvls <- remove_hemisphere_labels(lvls)
  }
  
  return(lvls)
}

#' @export
table_apply_roi <- function(
  table, roi_column, roi, use_regex,
  na_rm = FALSE
){
  var <- table[[roi_column]]
  
  if(use_regex){
    
    pattern <- paste0('(',roi,')', collapse = '|')
    idx <- str_detect(var, pattern)
    
  } else {
    idx <- var %in% roi
  }
  
  if( na_rm ) {
    return(table[idx & !is.na(var),])
  } else {
    return(table[is.na(var) | idx,])
  }
  
}



