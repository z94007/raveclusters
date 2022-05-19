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



