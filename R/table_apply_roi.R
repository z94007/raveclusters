#' @export
table_apply_roi <- function(table, roi_column, roi, use_regex){
  var <- table[[roi_column]]
  
  if(use_regex){
    
    pattern <- paste0('(',roi,')', collapse = '|')
    idx <- str_detect(var, pattern)
    
  } else {
    idx <- var %in% roi
  }
  
  return(table[idx,])
}



