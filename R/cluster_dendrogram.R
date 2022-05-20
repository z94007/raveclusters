#' @export
cluster_dendrogram <- function(results, cex = 0.7) {
  
  if(!identical(results$cluster_method, 'H-Clust')) {
    stop("Cluster dendrogram is only available for hierarchical clustering")
  }
  cluster_table <- results$cluster_table
  analysis_time_window <- results$analysis_time_window
  cluster_table$Event <- remove_event_prefix(cluster_table$Event)
  
  hclust <- attr(cluster_table, "hclust")
  cluster_idx <- attr(cluster_table, "cluster_idx")
  cluster_col <- attr(cluster_table, "color_space")
  
  labels <- sprintf("%s%s", cluster_table$Subject, cluster_table$Electrode)
  
  n = length(labels)
  k = length(cluster_idx)
  
  leafCol <- function(x, col){
    if(stats::is.leaf(x)){
      attr(x,'label') <- labels[x]
      attr(x, 'nodePar') <- list(lab.col = col,pch = 46,cex=0 )
      attr(x, "edgePar") <- list(col = col)
    }else{
      if (is.null(attr(x, "edgePar"))) {
        attr(x, "edgePar") <- list(col = col)
      }
    }
    unclass(x)
  } 
  
  #set lay out
  layout(matrix(1:2, ncol = 2), widths = c(3/4, 1/4))
  par(cex = cex, mar = c(0,1,0,1))
  
  # define the dendrogram
  dend <- as.dendrogram(hclust)
  
  #color the nodes(leaves) and branches of the dendrogram (from dendextend package color_branches)
  g <- dendextend::cutree(hclust, k = k, h = NULL)
  descendTree <- function(sd) {
    groupsinsubtree <- unique(g[labels(sd)])
    if (length(groupsinsubtree) > 1) {
      for (i in seq(sd)) {
        sd[[i]] <- descendTree(sd[[i]])
      }
    }
    else {
      sd <- dendrapply(sd, leafCol,cluster_col[groupsinsubtree])
      # if (!is.null(groupLabels)) {
      #   attr(sd, "edgetext") <- groupLabels[groupsinsubtree]
      #   attr(sd, "edgePar") <- c(attr(sd, "edgePar"), 
      #                            list(p.border = cluster_col[groupsinsubtree]))
      # }
    }
    unclass(sd)
  }
  
  dend <- descendTree(dend) 
  class(dend) <- 'dendrogram'
  
  #plot the horizontal dendrogram
  plot(dend,las = 1,horiz = TRUE, yaxt='n',#remove the y axis and labels
       ylim = c(0, n + 1))
  
  
  #add clustering cutting line
  MidPoint = (hclust$height[n - k] + hclust$height[n - k + 1]) / 2
  abline(v = MidPoint, lty = 2, col = "grey60")
  text(x = MidPoint, y = n, adj = 1, labels = sprintf("Cut-tree: k=%d  ", k), col = "grey60")
  
  legend('topleft', sprintf('Cluster %d', cluster_idx), 
         bty = 'n', text.font = 2, cex = cex *1.5, 
         text.col = cluster_col)
  
  plot_clean(xlim = c(0, 1), ylim = c(0, n + 1))
  
  # combine mse
  indata_analysis <- results$indata_analysis
  
  image( t(indata_analysis[order.dendrogram(dend), , drop=FALSE]),  
         y = seq_len(n),
         col= get_palette('BlueYellow'),
         yaxt = 'n',bty = 'n', xaxt= 'n', add = TRUE)
  
}