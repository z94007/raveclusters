
`%OF%` <- function(lhs, rhs){
  if(length(rhs)){ de <- rhs[[1]] } else { de <- rhs }
  lhs <- lhs[!is.na(lhs)]
  if(!length(lhs)){ return(de) }
  sel <- lhs %in% rhs
  if(any(sel)){ return(lhs[sel][[1]]) }
  return(de)
}
