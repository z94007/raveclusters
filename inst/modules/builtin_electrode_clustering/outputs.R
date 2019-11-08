tsne_plot <- function(){
  # shiny::validate(
  #   shiny::need(exists('tsne'), message = 'Please press "Run Analysis" button.'),
  #   shiny::need(exists('tsne') && !is.null('tsne'), message = 'Lack of number of electrodes')
  # )
  # plot(tsne$Y, t = 'n')
  # text(tsne$Y, labels = baselined$dimnames$Electrode, col = colors[clusters])
  shiny::validate(
    shiny::need(exists('mds_res'), message = 'Please press "Run Analysis" button.'),
    shiny::need(exists('mds_res') && !is.null('mds_res'), message = 'Number of clusters must be greater than 1')
  )
  # plot(pca_res$x, col = clusters, pch = 16)
  pcs = 1:2
  plot(mds_res[,pcs], type = 'n',xlab = 'X', ylab = 'Y')
  text(mds_res[,pcs], labels = collapsed, col = colors[clusters])
}

cluster_plot <- function(){
  shiny::validate(
    shiny::need(exists('cluster_means'), message = 'Please press "Run Analysis" button.')
  )
  
  if( input_nclusters <= 3 ){
    par(mfrow = c(1, input_nclusters))
  }else{
    nrow = ceiling(input_nclusters / 3)
    par(mfrow = c(nrow, 3))
  }
  
  time_points = as.numeric(names(indata))#preload_info$time_points
  
  cluster_vis <- mapply(function(cm, ii) {
    rutabaga::plot_clean(time_points, ylim=c(-100, 300))
    gnames = list()
    cols = NULL
    
    for( j in seq_along(groups) ){
      g = groups[[j]]
      conditions = g$conditions
      gnames[[j]] = g$condition_name
      lines(time_points, colMeans(indata[which(collapsed$ConditionGroup %in% g$condition_name),]),
            col=j)
      
      cols = c(cols, j)
    }
    
    # lines(baselined$dimnames$Time, colMeans(cm[which(baselined$dimnames$Trial %in% v_trials),]),
    #       col="#377EB8")
    
    legend('topright', unlist(gnames), bty='n', text.font = 2,
           text.col = cols)
    text(quantile(time_points, 0.05),200, paste0('n_elec=', sum(clusters == ii)))# didn't work
    title(main=rutabaga::deparse_svec(baselined$dimnames$Electrode[which(ii == clusters)]))
    
    rutabaga::ruta_axis(1, pretty(time_points))
    rutabaga::ruta_axis(2, pretty(-100:1000))
    
  }, cluster_means, seq_along(cluster_means))
}

viewer_3d_fun <- function(...){
  # brain = rave::rave_brain2('congruency/YAB')
  
  
  
  
  # Cluster
  tbl = data.frame(
    Cluster = paste(clusters),
    Project = subject$project_name,
    Subject = subject$subject_code,
    Electrode = preload_info$electrodes
  )
  brain$set_electrode_values(tbl)
  
  brain$plot(side_width = 160, side_shift = c(0,0), palettes = list(
    'Cluster' = colors
  ))
}