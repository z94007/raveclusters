
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
cluster_dendrogram <- function(
    results, cluster_excluded = NULL,
    type = c("tree", "fan"), 
    adjust_heights = FALSE,
    main = "Dendrogram", 
    style_legend = c("all", "simplified", "none"),
    margin_right = 2.1, 
    cex = 1, cex_legend = 1, cex_leaf = 0.7, pal = "BlueWhiteRed") {
  
  # list2env(list(main = "Dendrogram", margin_right = 2.1, 
  # style_legend = c("all", "simplified", "none"),cex = 1, cex_legend = 1, 
  # cex_leaf = 0.7, pal = "BlueWhiteRed"), envir=.GlobalEnv)
  
  if(!identical(results$cluster_method, 'H-Clust')) {
    stop("Cluster dendrogram is only available for hierarchical clustering")
  }
  style_legend <- match.arg(style_legend)
  type <- match.arg(type)
  
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
  hclust$labels <- labels
  
  n = length(labels)
  k = length(cluster_idx)
  
  if(adjust_heights) {
    kh = (hclust$height[n - k] + hclust$height[n - k + 1]) / 2
    adj_height <- hclust$height / kh
    adj_height[adj_height > 1] <- log(adj_height[adj_height > 1]) + 1
    hclust$height <- adj_height * kh
  }
  
  if(is.matrix(results$baseline_array)) {
    #legend_yrange <- quantile(baseline_array, c(0.001, 0.999), na.rm = TRUE)
    legend_yrange <- quantile(results$baseline_array, c(0.001, 0.999), na.rm = TRUE)
  } else {
    legend_yrange <- NULL
  }
  
  
  leafCol <- function(x, col){
    if(stats::is.leaf(x)){
      attr(x,'label') <- labels[x]
      attr(x, 'nodePar') <- list(lab.col = col, pch = 46, cex=0, lab.cex = cex_leaf * cex)
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
    unclass(sd)
  }
  
  dend <- descendTree(dend) 
  class(dend) <- 'dendrogram'
  
  dend <- dendextend::prune(dend, 
                            labels[cluster_table$Cluster %in% as.numeric(cluster_excluded)])
  
  dend_order <- order.dendrogram(dend)
  
  xlim <- attr(dend, "height")
  ylim <- length(dend_order) + 1
  
  #set layout
  switch (
    style_legend,
    "all" = {
      layout(rbind(c(4, 4, 4, 1),
                   cbind(6, 5, 3, 2)),
             widths = c(4, 8, lcm(0.3), 4),
             heights = c(lcm(1), 1))
    }, {
      layout(rbind(c(4, 4, 1),
                   cbind(5, 3, 2)),
             widths = c(3, lcm(0.3), 1),
             heights = c(lcm(1), 1))
    }
  )
  
  
  par(mar = c(0, 0, 1.5, 1))
  # plot_clean(xlim = c(0, 1), ylim = c(0, ylim))
  
  # combine mse
  indata_analysis <- results$indata_analysis
  z <- t(indata_analysis[!(cluster_table$Cluster %in% as.numeric(cluster_excluded)), ,
                         drop=FALSE][dend_order,])
  zlim <- quantile(abs(z), 0.99, na.rm = TRUE)
  z[z > zlim] <- zlim
  z[z < -zlim] <- -zlim
  
  zlen <- seq(-zlim, zlim, length.out = length(pal))
  zlegend <- matrix(zlen, ncol = 1)
  plot_clean(c(0, 1), c(0, ylim), xaxs = "i")
  image(z = zlegend, col = pal,
        yaxt = 'n', bty = 'n', xaxt = 'n', xlim = c(0, 1), add = TRUE)
  zlen_txt <- sprintf("%.1f", c(-zlim, zlim))
  axis(3, c(0, 0.5, 1), labels = c(zlen_txt[1], "0", zlen_txt[2]), 
       tcl = -0.2, mgp = c(3, 0.25, 0), cex.axis = 0.6)
  
  par(mar = c(0, 0, 0.1, 1))
  plot_clean(c(0, 1), c(0, ylim), xaxs = "i")
  image( z, zlim = c(-zlim, zlim),
         y = seq(1.5, length(dend_order)-0.5, length.out = length(dend_order)),
         col= pal, ylim = c(0, ylim), add = TRUE,
         yaxt = 'n',bty = 'n', xaxt= 'n')
  
  par(mar = c(0, 0, 0.1, 0))
  plot_clean(c(0, 1), c(0, ylim), xaxs = "i")
  
  image(matrix(
    cluster_table$Cluster[!(cluster_table$Cluster %in% as.numeric(cluster_excluded)),
                          drop=FALSE][dend_order],
    nrow = 1
  ), y = seq(1.5, length(dend_order)-0.5, length.out = length(dend_order)), 
  add = TRUE, yaxt = 'n',bty = 'n', xaxt= 'n',
  col = cluster_col, zlim = c(1,max(cluster_table$Cluster)),useRaster = TRUE)
  
  par(cex = cex, mar = c(0, 1, 1.5, 1))
  
  plot_clean(c(0,1), c(0,1), main = main)
  
  if(type == "fan") {
    par(mar = c(0, 0.1, 0, 0.1))
    phylo <- ape::as.phylo(hclust)
    plot(phylo, type = "fan", cex = cex_leaf * cex, 
         tip.color = cluster_col[cluster_table$Cluster], 
         no.margin = FALSE, rotate.tree = 90, show.tip.label = TRUE)
  } else {
    par(mar = c(0, 1, 0.1, margin_right))
    # plot the horizontal dendrogram
    # remove the y axis and labels
    plot(dend, las = 1, horiz = TRUE, yaxt='n', 
         ylim = c(0, ylim), xaxs = "i")
    
    #add clustering cutting line
    MidPoint = (hclust$height[n - k] + hclust$height[n - k + 1]) / 2
    abline(v = MidPoint, lty = 2, col = "grey60")
    if(k <= 4) {
      text(x = MidPoint, y = n + 1, adj = 0, labels = sprintf("  Cut tree: k = %d", k), col = "grey60")
    } else {
      text(x = MidPoint, y = n + 1, adj = 1, labels = sprintf("Cut tree: k = %d  ", k), col = "grey60")
    }
  }
  
  par(mar = c(0, 0.1, 0.1, 0))
  if(style_legend == "all") {
    plot_clean(c(0, 1), c(0, 1))
  }
  
  
  if(style_legend != "none") {
    
    legend_text <- sprintf('Cluster %d', cluster_idx)
    
    if(style_legend == "all") {
      legend_params <- legend(
        'topleft', legend_text, text.font = 2, 
        cex = cex_legend * cex, bty = 'n', 
        text.col = cluster_col, y.intersp = 1.5, plot = FALSE)
      legend_bottom <- legend_params$rect$top - legend_params$rect$h
      if(legend_bottom < 0) {
        cex_legend <- cex_legend / legend_params$rect$h * legend_params$rect$top
      }
    }
    
    
    legend_params <- legend(
      'topleft', legend_text, text.font = 2, 
      cex = cex_legend * cex, bty = 'n', 
      text.col = cluster_col, y.intersp = 1.5)
    
    if(style_legend == "all") {
      graph_left <- legend_params$rect$left + legend_params$rect$w
      if(k > 1) {
        graph_height <- abs(legend_params$text$y[2] - legend_params$text$y[1])
      } else {
        graph_height <- legend_params$rect$h
      }
      graph_width <- 1 - graph_left#graph_height * 2.4 / ylim * xlim
      graph_bottom <- min(legend_params$text$y) - graph_height / 2
      
      ndc_x <- grconvertX(c(graph_left, graph_left + graph_width), "user", "ndc")
      ndc_y <- grconvertY(c(graph_bottom, graph_bottom + graph_height * seq_len(k)), "user", "ndc")
      ndc_y[ndc_y < 0] <- 0
      ndc_y[ndc_y > 1] <- 1
      
      for(jj in 1:k) {
        par(
          fig = c(
            ndc_x,
            ndc_y[c(jj, jj+1)]
          ),
          new = TRUE,
          mar = c(0, 0, 0, 0)
        )
        
        cluster_visualization(results, style_title = "none", 
                              style_analysis = "none", style_baseline = "none", 
                              style_axis = "none", style_legend = "none", 
                              one_plot = FALSE, which = (k-jj) + 1, lwd = 0.8,
                              yrange = legend_yrange)
      }
      
      
    }
  }
  
  invisible(dend)
  # text(y = ylim, x = 0, sprintf("[%.1f~%.1f]", -zlim, zlim), adj = c(0.5,1))
}