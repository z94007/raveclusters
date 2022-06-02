
cluster_visualization2 <- function(results, combines, main = NULL, ...) {
  
  cluster_mat <- attr(results$cluster_index, "clusters")
  item_table <- attr(results$indata_plot, "item_table")
  
  # combines <- c(2,3,4)
  cluster <- cluster_mat[,max(combines)]
  sel <- cluster %in% combines
  cluster[sel] <- min(combines)
  
  item_table$Cluster <- cluster
  mse <- raveclusters::cluster_mse(indata_plot = results$indata_plot, 
                                   cluster = cluster)
  
  plot_args <- results
  plot_args$cluster_table <- item_table
  plot_args$mse <- mse
  attr(plot_args$cluster_table, "nclusters") <- max(cluster)
  if(is.null(main)) {
    main <- sprintf("n=%s", sum(sel))
  }
  cluster_visualization(plot_args, which = min(combines), one_plot = FALSE, 
                        main = main, ...)
}



#' @export
cluster_dendrogram <- function(results, cex = 0.7, main = "Dendrogram") {
  
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
  
  
  # define the dendrogram
  dend <- stats::as.dendrogram(hclust)
  
  #color the nodes(leaves) and branches of the dendrogram (from dendextend package color_branches)
  g <- stats::cutree(hclust, k = k)
  heights <- dipsaus::fastqueue2()
  descendTree <- function(sd) {
    groupsinsubtree <- unique(g[labels(sd)])
    if (length(groupsinsubtree) > 1) {
      for (i in seq(sd)) {
        sd[[i]] <- descendTree(sd[[i]])
      }
    }
    else {
      sd <- dendrapply(sd, leafCol,cluster_col[groupsinsubtree])
    }
    heights$add(list(
      groupsinsubtree = groupsinsubtree,
      midpoint = attr(sd, "midpoint"),
      height = attr(sd, "height"),
      members = attr(sd, "members")
    ))
    unclass(sd)
  }
  
  dend <- descendTree(dend) 
  class(dend) <- 'dendrogram'
  
  #set lay out
  layout(matrix(c(2, 1, 4, 3), ncol = 2), widths = c(3/4, 1/4), heights = c(graphics::lcm(1.5), 1))
  par(cex = cex, mar = c(0,1,0, 2.4))
  
  # plot the horizontal dendrogram
  # remove the y axis and labels
  plot(dend,las = 1,horiz = TRUE, yaxt='n', 
       ylim = c(0, n + 1))
  # tmpfile <- tempfile()
  # png(filename = tmpfile, width = 480, height = 480)
  # item <- heights[[2]]
  # par(mar = c(0.1, 0.1, 2.1, 0.1))
  # cluster_visualization2(
  #   results, item$groupsinsubtree, style_title = "simplified+default", 
  #   style_analysis = "none", style_baseline = "none", 
  #   style_axis = "box", style_legend = "none"
  # )
  # dev.off()
  # node <- png::readPNG(tmpfile)
  # points(item$height, item$midpoint, pch = 16)
  # points(item$height, item$midpoint, pch = 16)
  # rasterImage(node, xleft = )
  # 
  # 
  # legend
  # cluster_visualization(style_title = "simplified+default", style_analysis = "none", style_baseline = "none", style_axis = "box", style_legend = "none")
  
  
  #add clustering cutting line
  MidPoint = (hclust$height[n - k] + hclust$height[n - k + 1]) / 2
  abline(v = MidPoint, lty = 2, col = "grey60")
  text(x = MidPoint, y = n, adj = 1, labels = sprintf("Cut tree: k = %d  ", k), col = "grey60")
  
  par(mar = c(1.0, 1, 1.1, 1))
  plot_clean(c(0,1), c(0,1), main = main)
  
  legend('topleft', sprintf('Cluster %d', cluster_idx), 
         bty = 'n', text.font = 2, cex = cex *1.5, 
         text.col = cluster_col, ncol = 5)
  
  par(mar = c(0, 1, 0, 1))
  plot_clean(xlim = c(0, 1), ylim = c(0, n + 1))
  
  # combine mse
  indata_analysis <- results$indata_analysis
  z <- t(indata_analysis[order.dendrogram(dend), , drop=FALSE])
  zlim <- quantile(abs(z), 0.99, na.rm = TRUE)
  z[z > zlim] <- zlim
  z[z < -zlim] <- -zlim
  pal <- get_palette('BlueYellow')
  image( z, zlim = c(-zlim, zlim),
         y = seq_len(n),
         col= pal,
         yaxt = 'n',bty = 'n', xaxt= 'n', add = TRUE)
  
  zlen <- seq(-zlim, zlim, length.out = length(pal))
  par(mar = c(0.0, 1, 2.6, 1))
  plot_clean(xlim = c(0, 1), ylim = c(0, 1))
  image(z = matrix(zlen, ncol = 1), col = pal,
        yaxt = 'n', bty = 'n', xaxt = 'n', add = TRUE)
  zlen_txt <- sprintf("%.1f", c(-zlim, zlim))
  axis(3, c(0, 0.5, 1), labels = c(zlen_txt[1], "0", zlen_txt[2]), 
       tcl = -0.2, mgp = c(3, 0.25, 0))
  
  
}