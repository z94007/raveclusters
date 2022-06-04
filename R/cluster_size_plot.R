cluster_idx_plot <- function(
  x, dis, clustfun, method = c("silhouette", "wss", "gap_stat"),
  distance_method, col = "dodgerblue3", cv_fold = 5,
  n_highlights = 3, max_clusters = 8
) {
  method <- match.arg(method)
  indata_analysis <- x
  rownames(indata_analysis) <- NULL
  if(missing(dis)) {
    dis <- calculate_distance(indata_analysis, method = distance_method)
  }
  dis <- as.matrix(dis)
  sa <- sample(seq_len(nrow(indata_analysis)))
  cv_size <- ceiling(length(sa) / cv_fold)
  cv_idx <- matrix(NA_integer_, cv_fold, cv_size)
  cv_idx[seq_along(sa)] <- sa
  
  if(method != "gap_stat" && cv_fold > 1){
    cv_y <- lapply(seq_len(cv_fold), function(ii) {
      idx <- cv_idx[ii, ]
      idx <- idx[!is.na(idx)]
      idx <- sa[!sa %in% idx]
      
      cxi <- factoextra::fviz_nbclust(
        indata_analysis[idx, , drop = FALSE],
        FUNcluster = clustfun, 
        method = method, 
        diss = dis[idx, idx, drop = FALSE],
        k.max = max_clusters )
      cxi$data$y
    })
    xlen <- seq_len(min(sapply(cv_y, length)))
    cv_y <- sapply(cv_y, function(x){ x[xlen] })
    cv_mse_y <- apply(cv_y, 1, dipsaus::mean_se)
    sd <- cv_mse_y[2,] * sqrt(cv_fold - 1) * 2
    sd[is.na(sd) | sd < 0.01] <- 0.01
    cv_mse_y[2,] <- sd
  } else {
    cxi <- factoextra::fviz_nbclust(
      indata_analysis,
      FUNcluster = clustfun, 
      method = method, 
      diss = dis,
      k.max = max_clusters )
    if(method == "gap_stat") {
      xlen <- seq_len(nrow(cxi$data))
      cv_mse_y <- rbind(
        (cxi$data$ymax + cxi$data$ymin) / 2,
        (cxi$data$ymax - cxi$data$ymin) / 2
      )
    } else {
      xlen <- seq_along(cxi$data$y)
      cv_mse_y <- rbind(cxi$data$y, 0)
    }
    
  }
  
  
  
  yrange <- range(
    cv_mse_y[1, ] + cv_mse_y[2,],
    cv_mse_y[1, ] - cv_mse_y[2,]
  )
  
  xlab <- "Number of clusters k"
  switch (
    method,
    silhouette = {
      ylab <- "Average Silhouette width"
    },
    wss = {
      ylab <- "Total within sum of square"
    }, {
      ylab <- "Gap statistics (k)"
    }
  )
  
  rutabaga::plot_clean(
    xlen, yrange, xlab = xlab,
    ylab = ylab,
    main = "Optimal number of clusters"
  )
  rutabaga::ruta_axis(side = 1, xlen)
  if(method == "wss") {
    pyl <- pretty(yrange)
    fyl <- floor(log10(pyl))
    labels <- sprintf("%.0fE%.0f", pyl / 10^fyl, fyl)
    if(pyl[[1]] <= 0) {
      labels[1] <- "0"
    }
    rutabaga::ruta_axis(side = 2, at = pyl, 
                        labels = labels)
  } else {
    rutabaga::ruta_axis(side = 2, pretty(yrange))
  }
  
  if(cv_fold > 1) {
    rutabaga::ebars(
      x = xlen,
      y = cv_mse_y[1, ],
      sem = cv_mse_y[2, ], 
      type = "p",
      pch = 1, col = col
    )
  } else {
    points(
      x = xlen,
      y = cv_mse_y[1, ],
      type = "p",
      pch = 1, col = col
    )
  }
  
  if(n_highlights > 0) {
    n_highlights <- min(n_highlights, length(xlen))
    idx <- order(cv_mse_y[1, ], decreasing = TRUE)[seq_len(n_highlights)]
    
    points(idx, cv_mse_y[1, idx], col = "red", pch = 19)
    legend('bottomright', 
           "Suggested # clusters",
           bty='n', text.font = 2, col = 'red', pch = 19)
    
    
  }
  
}
