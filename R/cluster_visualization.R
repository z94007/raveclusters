
#' @export
cluster_visualization <- function(
  results, color_scheme = "Beautiful Field",
  cex = 1, 
  plot_range = NULL){
  
  # # DEBUG: start
  # results <- raveclusters::ravecluster(
  #   names = c("condition_groups", "collapsed_array", "mse", "dis", "use_baseline", "baseline", "indata_analysis", "indata_plot", "cluster_table", "input_nclusters", "analysis_time_window", "plot_time_window", "power_unit"))
  # color_scheme <- "Beautiful Field"
  # cex <- 1
  # # DEBUG: end
  
  cols <- get_palette("Beautiful Field")
  
  use_baseline <- results$use_baseline
  power_unit <- results$power_unit
  power_unit_text <- switch (
    power_unit,
    `decibel` = "Decibel",
    `Pct_Change_Power` = "% Change Power",
    `Pct_Change_Amplitude` = "% Change Amplitude",
    `z_score_Power` = "z-score Power",
    `z_score_Amplitude` = "z-score Amplitude", 
    {
      gsub("_", " ", power_unit)
    }
  )
  nclust <- attr(results$cluster_table, "nclusters")
  cluster_color <- attr(results$cluster_table, "color_space")
  cluster_idx <- attr(results$cluster_table, "cluster_idx")
  plot_time_window <- results$plot_time_window
  if(length(plot_range) != 2) {
    plot_range <- plot_time_window
  }
  analysis_time_window <- results$analysis_time_window
  mse <- results$mse
  group <- results$condition_groups
  group_names <- sapply(group, "[[", "group_name")
  
  dnames <- dimnames(mse)
  dnames$Time <- as.numeric(dnames$Time)
  
  bg_analysis_rect <- rgb(red = 1, green = 0, blue = 0, alpha = 0.1)
  
  # Prepare canvas
  # if( !separate ){
  old_par <- par("mfrow", "mar")
  if( nclust <= 4 ){
    par(mfrow = c(1, nclust))
  }else{
    nrow <- ceiling((nclust) / 4)
    par(mfrow = c(nrow, 4))
  }
  par(mar = c(4.1,4.1, 4.1, 2))
  on.exit({ do.call("par", old_par) })
  # }
  
  # Calculate ylim
  yrange <- c(
    min(mse[,1,,] - mse[,2,,], mse[,1,,], na.rm = TRUE),
    max(mse[,1,,] + mse[,2,,], mse[,1,,], na.rm = TRUE)
  )
  xrange <- range(plot_range)
  xaxi <- pretty(xrange)
  yaxi <- pretty(yrange)
  
  # get size of texts
  cex_base <- cex * get_cex_for_multifigure()
  cex_main <- cex_base * 1.2
  cex_lab <- cex_base
  cex_legend <- cex_base * 0.8
  cex_axis <- cex_base * 0.8
  
  # for each cluster
  lapply(seq_along(cluster_color), function(ii){
    ccol <- cluster_color[ii]
    
    # create a plot
    rutabaga::plot_clean(xlim = xrange, ylim = yrange)
    rutabaga::ruta_axis(2, yaxi, cex.axis = cex_axis)
    rutabaga::ruta_axis(1, labels = xaxi, at = xaxi, cex.axis = cex_axis)
    
    # plot the rectangle of analysis window
    x_rect <- analysis_time_window
    y_rect <- yrange
    rect(x_rect[1], y_rect[1], x_rect[2], y_rect[2],
         col = bg_analysis_rect, border = NA)
    # FIXME: the location of this should not overlap with the legend of group condition
    legend(0.7 * x_rect[2] + 0.3 * x_rect[1], y_rect[2],
           'Analysis', text.col = 'red', bty='n', 
           text.font = 1, adj = 1, cex = cex_legend)
    
    # Add legends for each conditions
    legend(x = xrange[[1]],y = max(yaxi), group_names, bty='n', 
           text.font = 1, text.col = cols, cex = cex_legend, adj = c(0, 1))
    
    # generate ylab
    ylab <- sprintf( "%s%s", power_unit_text, 
                     ifelse(use_baseline, " (z-scored)", ""))[[1]]
    mtext(ylab, side = 2, line = 2, cex = cex_lab)
    
    # generate xlab
    mtext('Time(s)', side = 1, line = 2, cex = cex_lab)
    
    # Add title
    ravebuiltins:::rave_title(
      sprintf('Cluster%s (n=%s)', cluster_idx[ii],
              sum(results$cluster_table$Cluster == cluster_idx[ii])),
      col = ccol,
      cex = cex_main
    )
    
    # Plot!
    lapply(seq_along(group), function(jj){
      
      m <- mse[, 1, jj, ii]
      se <- mse[, 2, jj, ii]
      
      rutabaga::ebar_polygon(dnames$Time, m, sem = se, col = cols[jj])
      
    })
    
  })
}