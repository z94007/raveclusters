#' @export
cluster_membership <- function(cluster_table){
  
  if(!is.data.frame(cluster_table)) { return() }
  
  cluster_table$Event <- remove_event_prefix(cluster_table$Event)
  
  nevents <- length(unique(cluster_table$Event))
  if(length(nevents) == 1) {
    split_by <- c("Subject")
  } else {
    split_by <- c("Subject", "Event")
  }
  
  tbl <- convert_cluster_table(
    x = cluster_table,
    split_by = split_by,
    var = "Cluster",
    value = "Electrode"
  )
  tbl
  
}