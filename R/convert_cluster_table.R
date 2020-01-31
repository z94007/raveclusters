convert_cluster_table <- function(x, split_by = 'A', var = 'C', value = 'B'){
  clusters = sort(unique(x[[var]]))
  res = lapply(split(x, x[[split_by]]), function(tbl){

  res = lapply(clusters, function(cl){
    dipsaus::deparse_svec(as.numeric(tbl[[value]][tbl[[var]] == cl]))
  })
  names(res) = paste0(var, clusters)
  res = as.data.frame(res)
  res = data.frame(Subject = tbl[[split_by]][[1]], res)
  res
})
  res <- do.call('rbind', res)
  rownames(res) <- NULL
  return(res)
}