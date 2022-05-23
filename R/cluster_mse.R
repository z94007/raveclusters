#' Calculate cluster centroid and mean standard error
#' @export
cluster_mse <- function(indata_plot, cluster){
  cluster_idx <- sort(unique(cluster))
  
  # get time range
  time <- sort(unique(unlist(lapply(indata_plot, "[[", "time"))))
  
  # time x (mean, se) x condition group x cluster
  mse <- array(NA_real_, c(length(time), 2, length(indata_plot), length(cluster_idx)))
  
  for(ii in seq_along(cluster_idx)) {
    ci <- cluster_idx[[ii]]
    
    for(jj in seq_along(indata_plot)) {
      item <- indata_plot[[jj]]
      
      sel <- time %in% item$time
      
      group_mse <- apply(item$data[cluster == ci,,drop = FALSE], 2, 
                         dipsaus::mean_se, na.rm = TRUE, se_na_as_zero = FALSE)
      
      mse[sel, , jj, ii] <- t(group_mse)
    }
  }
  
  dimnames(mse) <- list(
    Time = time,
    Stat = c("mean", "stderr"),
    Group = seq_along(indata_plot),
    Cluster = cluster_idx
  )
  mse
}
