
#' @export
cluster_visualization <- function(
  results, color_scheme = "Beautiful Field",
  cex = 1, 
  style_title = c("color", "default", 
                  "simplified+color", "simplified+default", 
                  "none"),
  style_analysis = c("default", "none"),
  style_baseline = c("default", "none"),
  style_axis = c("default", "box", "text+box", "none"),
  style_legend = c("default", "none"),
  box_colors = "auto",
  one_plot = TRUE,
  which = 0,
  plot_range = NULL, yrange = NULL,
  before_plot = NULL, after_plot = NULL){
  
  
  style_title <- match.arg(style_title)
  style_analysis <- match.arg(style_analysis)
  style_baseline <- match.arg(style_baseline)
  style_axis <- match.arg(style_axis)
  style_legend <- match.arg(style_legend)
  
  # # DEBUG: start
  # results <- raveclusters::ravecluster(
  #   names = c("condition_groups", "collapsed_array", "mse", "dis", "use_baseline", "baseline", "indata_analysis", "indata_plot", "cluster_table", "input_nclusters", "analysis_time_window", "power_unit"))
  # color_scheme <- "Beautiful Field"
  # cex <- 1
  # style_title <- "color"
  # style_analysis <- "default"
  # style_baseline <- "default"
  # style_axis <- "default"
  # plot_range <- yrange <- before_plot <- after_plot <- NULL
  # # DEBUG: end
  
  
  cols <- get_palette(color_scheme)
  
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
  if(!length(nclust)) {
    nclust <- length(unique(results$cluster_table$Cluster))
  }
  cluster_idx <- attr(results$cluster_table, "cluster_idx")
  if(!length(cluster_idx)) {
    cluster_idx <- seq_len(nclust)
  }
  cluster_color <- attr(results$cluster_table, "color_space")
  if(length(cluster_color) != nclust) {
    ccol <- c(par("fg"), "black")[[1]]
    cluster_color <- rep(ccol, nclust)
  }
  if(identical(box_colors, "auto")) {
    box_colors <- cluster_color
  } else {
    box_colors <- rep(box_colors, ceiling(nclust / length(box_colors)))
  }
  

  analysis_time_window <- results$analysis_time_window
  mse <- results$mse
  group <- results$condition_groups
  group_names <- sapply(group, "[[", "group_name")
  
  dnames <- dimnames(mse)
  dnames$Time <- as.numeric(dnames$Time)
  
  if(length(plot_range) != 2) {
    plot_range <- range(dnames$Time)
  }
  
  bg_analysis_rect <- rgb(red = 1, green = 0, blue = 0, alpha = 0.1)
  bg_baseline_rect <- rgb(red = 0.3, green = 0.3, blue = 0.3, alpha = 0.1)
  
  # Calculate ylim
  if(length(yrange) != 2) {
    yrange <- c(
      min(mse[,1,,] - mse[,2,,], mse[,1,,], na.rm = TRUE),
      max(mse[,1,,] + mse[,2,,], mse[,1,,], na.rm = TRUE)
    )
  }
  
  xrange <- range(plot_range)
  xaxi <- pretty(xrange)
  yaxi <- pretty(yrange)
  
  baseline_time <- results$settings$baseline_time
  if(style_baseline == "default" && use_baseline && length(baseline_time)) {
    baseline_time <- raveio::validate_time_window(baseline_time)[[1]]
  } else {
    baseline_time <- NULL
  }
  
  # get size of texts
  cex_base <- cex * get_cex_for_multifigure()
  cex_main <- cex_base
  cex_lab <- cex_base
  cex_legend <- cex_base * 0.8
  cex_axis <- cex_base * 0.8
  
  if(identical(which, 0)) {
    which <- seq_len(nclust)
  }
  if(one_plot && length(which) > 1) {
    par(mfrow = n2mfrow(length(which), asp = 2.5))
  }
  
  # for each cluster
  
  lapply(which, function(ii){
    
    if(is.function(before_plot)) {
      before_plot()
    }
    on.exit({
      if(is.function(after_plot)) {
        after_plot()
      }
    }, add = FALSE, after = FALSE)
    
    # create a plot
    rutabaga::plot_clean(xlim = xrange, ylim = yrange)
    
    if(style_axis %in% c("text+box", 'default')) {
      rutabaga::ruta_axis(2, yaxi, cex.axis = cex_axis)
      rutabaga::ruta_axis(1, labels = xaxi, at = xaxi, cex.axis = cex_axis)
      
      # generate ylab
      ylab <- sprintf( "%s%s", power_unit_text, 
                       ifelse(use_baseline, " (z-scored)", ""))[[1]]
      mtext(ylab, side = 2, line = 2, cex = cex_lab)
      
      # generate xlab
      mtext('Time(s)', side = 1, line = 2, cex = cex_lab)
      
    }
    
    if(style_axis %in% c("text+box", 'box')) {
      graphics::box(which = "plot", lty = 1, lwd = 2, col = box_colors[[ii]])
    }
    
    y_rect <- yrange
    
    # plot the rectangle of baseline window
    x_rect <- baseline_time
    
    if(length(x_rect)) {
      rect(x_rect[1], y_rect[1], x_rect[2], y_rect[2],
           col = bg_baseline_rect, border = NA)
    }
    
    
    # plot the rectangle of analysis window
    x_rect <- analysis_time_window
    if( style_analysis == 'default' ) {
      rect(x_rect[1], y_rect[1], x_rect[2], y_rect[2],
           col = bg_analysis_rect, border = NA)
      # FIXME: the location of this should not overlap with the legend of group condition
      legend(0.7 * x_rect[2] + 0.3 * x_rect[1], y_rect[2],
             'Analysis', text.col = 'red', bty='n', 
             text.font = 1, adj = 1, cex = cex_legend)
    }
    
    if(style_legend == "default") {
      # Add legends for each conditions
      legend(x = xrange[[1]],y = max(yaxi), group_names, bty='n', 
             text.font = 1, text.col = cols, cex = cex_legend, adj = c(0, 1))
    }
    
    
    # Add title
    if(style_title != 'none') {
      if(style_title %in% c("color", "simplified+color")) {
        ccol <- cluster_color[[ii]]
      } else {
        ccol <- c(par("fg"), "black")[[1]]
      }
      # rave_title(
      #   sprintf('Cluster%s (n=%s)', cluster_idx[ii],
      #           sum(results$cluster_table$Cluster == cluster_idx[ii])),
      #   col = ccol,
      #   cex = cex_main
      # )
      if(style_title %in% c("simplified+color", "simplified+default")) {
        main <- sprintf('n=%s', cluster_idx[ii],
                        sum(results$cluster_table$Cluster == cluster_idx[ii]))
      } else {
        main <- sprintf('Cluster%s (n=%s)', cluster_idx[ii],
                         sum(results$cluster_table$Cluster == cluster_idx[ii]))
      }
      graphics::title(main = list(main, col = ccol, font = 1,
                                  cex = cex_main))
    }
    
    # Plot!
    lapply(seq_along(group), function(jj){
      
      m <- mse[, 1, jj, ii]
      se <- mse[, 2, jj, ii]
      
      rutabaga::ebar_polygon(dnames$Time, m, sem = se, col = cols[jj])
      
    })
    
    on.exit({}, add = FALSE, after = FALSE)
    if(is.function(after_plot)) {
      after_plot()
    }
    
  })
  invisible()
}
