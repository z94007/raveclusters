#' @export
cluster_mds_plot <- function(results) {
  
  par(mfrow = c(1, 1), mar = c(0.5, 0.5, 3.6, 0.5))
  
  mds_result <- results$mds_result
  cluster_table <- results$cluster_table
  
  if(is.data.frame(cluster_table)) {
    cluster_idx <- attr(cluster_table, "cluster_idx")
    cluster_col <- attr(cluster_table, "color_space")
  } else {
    cluster_table <- attr(results$collapsed_array, "item_table")
    cluster_table$Cluster <- 1
    cluster_idx <- 1
    cluster_col <- "black"
    cluster_table$Color <- "black"
  }
  cluster_table$Event <- factor(remove_event_prefix(cluster_table$Event))
  event_levels <- levels(cluster_table$Event)
  labels <- sprintf("%s%s", cluster_table$Subject, cluster_table$Electrode)
  
  
  if(length(event_levels) == 1) {
    plot(mds_result$x, mds_result$y, type = 'n', xlab = '',
         xaxt = "n", yaxt = "n", ylab = "")
    text(mds_result$x, mds_result$y,
         labels = labels,
         col = cluster_table$Color)
    if(length(cluster_idx) > 1) {
      legend('topright', sprintf('Cluster %d', cluster_idx),
             bty='n', text.font = 2, text.col = cluster_col)
    }
    
  } else {
    plot(mds_result$x, mds_result$y, type = 'n', xlab = '', 
         xaxt = "n", yaxt = "n", ylab = "")
    points(mds_result$x, mds_result$y, 
           pch = as.integer(cluster_table$Event), 
           col = cluster_table$Color)
    text(mds_result$x, mds_result$y, 
         labels = sprintf(" %s", labels),
         col = cluster_table$Color, adj = 0)
    
    if(length(cluster_idx) > 1) {
      legend(
        'topright',
        c(sprintf('Cluster %d', cluster_idx),
          event_levels),
        bty = 'n',
        text.font = 2,
        text.col = c(cluster_col, rep("black", length(event_levels))),
        pch = c(rep(NA, length(cluster_col)), seq_along(event_levels))
      )
    } else {
      legend(
        'topright',
        event_levels,
        bty = 'n',
        text.font = 2,
        text.col = "black",
        pch = seq_along(event_levels)
      )
    }
    
  }
  
  rave_title(
    sprintf(
      '%d subj, %d elec, %d evt, %d obs',
      length(unique(cluster_table$Subject)),
      length(unique(cluster_table$Electrode)), 
      length(event_levels),
      nrow(cluster_table)
    )
  )
}
