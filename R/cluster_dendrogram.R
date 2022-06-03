
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
cluster_dendrogram <- function(results, cex = 1, main = "Dendrogram", 
                               margin_right = 2.1, 
                               cex_leaf = 0.7, pal = "BlueGrayRed") {
  
  # list2env(list(cex = 1, main = "Dendrogram", 
  #               cex_leaf = 0.7), envir=.GlobalEnv)
  
  if(!identical(results$cluster_method, 'H-Clust')) {
    stop("Cluster dendrogram is only available for hierarchical clustering")
  }
  
  if(length(pal) == 1) {
    pal <- get_heatmap_palette(pal)
  }
  if(length(pal) <= 100) {
    pal <- colorRampPalette(pal)(101)
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
  
  tmpfile <- tempfile()
  png(filename = tmpfile, width = 240 * k, height = 100)
  par(mar = c(0.1,0,0.1,0), mfrow = c(k, 1))
  cluster_visualization(results, style_title = "none", 
                        style_analysis = "none", style_baseline = "none", 
                        style_axis = "box", style_legend = "none", one_plot = FALSE,
                        lwd = 3)
  dev.off()
  graph <- png::readPNG(tmpfile)
  unlink(tmpfile)
  
  leafCol <- function(x, col){
    if(stats::is.leaf(x)){
      attr(x,'label') <- labels[x]
      attr(x, 'nodePar') <- list(lab.col = col, pch = 46, cex=0, lab.cex = cex_leaf)
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
  
  xlim <- attr(dend, "height")
  ylim <- n + 0.8
  
  #set lay out
  layout(matrix(c(2, 1), ncol = 2), widths = c(3/4, 1/4))
  
  
  par(mar = c(0, 1, 1, 1))
  plot_clean(xlim = c(0, 1), ylim = c(0, ylim))
  
  # combine mse
  indata_analysis <- results$indata_analysis
  z <- t(indata_analysis[order.dendrogram(dend), , drop=FALSE])
  zlim <- quantile(abs(z), 0.99, na.rm = TRUE)
  z[z > zlim] <- zlim
  z[z < -zlim] <- -zlim
  image( z, zlim = c(-zlim, zlim),
         y = seq_len(n),
         col= pal,
         yaxt = 'n',bty = 'n', xaxt= 'n', add = TRUE)
  
  zlen <- seq(-zlim, zlim, length.out = length(pal))
  z <- matrix(zlen, ncol = 1)
  image(z = cbind(z, z), y = c(ylim + 0.2, ylim + 0.5), col = pal,
        yaxt = 'n', bty = 'n', xaxt = 'n', add = TRUE)
  zlen_txt <- sprintf("%.1f", c(-zlim, zlim))
  axis(3, c(0, 0.5, 1), labels = c(zlen_txt[1], "0", zlen_txt[2]), 
       tcl = -0.2, mgp = c(3, 0.25, 0), cex.axis = 0.6)
  
  
  
  par(cex = cex, mar = c(0,1, 1, margin_right), xpd = TRUE)
  
  # plot the horizontal dendrogram
  # remove the y axis and labels
  plot(dend,las = 1,horiz = TRUE, yaxt='n', 
       ylim = c(0, ylim), main = main)
  
  #add clustering cutting line
  MidPoint = (hclust$height[n - k] + hclust$height[n - k + 1]) / 2
  abline(v = MidPoint, lty = 2, col = "grey60")
  text(x = MidPoint, y = 0, adj = 0, labels = sprintf("  Cut tree: k = %d", k), col = "grey60")
  
  legend_params <- legend('topleft', sprintf('Cluster %d', cluster_idx), 
                          text.font = 2, cex = cex, bty = 'n', 
                          text.col = cluster_col)
  
  graph_left <- legend_params$rect$left + legend_params$rect$w
  if(k > 1) {
    graph_height <- abs(legend_params$text$y[2] - legend_params$text$y[1])
  } else {
    graph_height <- legend_params$rect$h
  }
  graph_width <- graph_height * 2.4 / ylim * xlim
  graph_bottom <- min(legend_params$text$y) - graph_height / 2
  rasterImage(graph, xleft = graph_left, ybottom = graph_bottom, 
              xright = graph_left - graph_width, ytop = graph_bottom + graph_height * k)
  
  # text(y = ylim, x = 0, sprintf("[%.1f~%.1f]", -zlim, zlim), adj = c(0.5,1))
}