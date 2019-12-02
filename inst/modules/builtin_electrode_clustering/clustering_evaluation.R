eva <- function(){
  res <- local_data$my_results
  
  si <- cluster::silhouette(res$clusters_res,cluster::daisy(res$indata))
  
  return(si)

}