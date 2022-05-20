#' @title Convert 'RAVE' cluster results into a readable table
#' @param x a list of cluster results
#' @param split_by variable name to be split by; for example, \code{'Subject'}
#' @param var variable name indicating the cluster results; for example, 
#' \code{'Cluster'}
#' @param value the name of variable to concatenate into a single string or cell
#' value; for example, \code{'Electrode'}
#' @return A \code{\link{data.frame}} that is readable
#' 
#' @export
convert_cluster_table <- function(x, split_by = 'A', var = 'C', value = 'B'){
  clusters = sort(unique(x[[var]]))
  
  if(length(split_by) > 1) {
    x$.group <- do.call("paste", c(list(sep = "_"), lapply(split_by, function(nm) {
      x[[nm]]
    })))
    split_by <- ".group"
  }
  res <- lapply(split(x, x[[split_by]]), function(tbl){
    
    res = lapply(clusters, function(cl){
      dipsaus::deparse_svec(as.numeric(tbl[[value]][tbl[[var]] == cl]))
    })
    names(res) = paste0(var, clusters)
    res = as.data.frame(res)
    res = data.frame(Subject = tbl[[split_by]][[1]], res, Total = dim(tbl)[1])
    res
  })
  
  res <- do.call('rbind', res)
  rownames(res) <- NULL
  return(res)
}
