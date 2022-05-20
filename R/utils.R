
`%OF%` <- function(lhs, rhs){
  if(length(rhs)){ de <- rhs[[1]] } else { de <- rhs }
  lhs <- lhs[!is.na(lhs)]
  if(!length(lhs)){ return(de) }
  sel <- lhs %in% rhs
  if(any(sel)){ return(lhs[sel][[1]]) }
  return(de)
}

remove_event_prefix <- function(x){
  # event prefix
  event_prefix <- format_unit_of_analysis_name(names(get_unit_of_analysis()))
  
  gsub(sprintf("^(%s)[_]{0,1}", paste(event_prefix, collapse = "|")),
       replacement = "", x)
  
}

