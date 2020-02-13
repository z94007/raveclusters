
# local_data = ...local_data
# input = ...input


mds_plot <- function(){
  # rave::rave_context()
  
  res <- local_data$my_results
  
  shiny::validate(
    shiny::need(!is.null(res$indata) && ncol(res$indata) > 2, message = 'Please press "Run Analysis" button.'),
    shiny::need(length(res$input_nclusters) && 
                  !is.na(res$input_nclusters) &&
                  res$input_nclusters > 1, message = 'Number of clusters must be greater than 1')
  )
  
  rave::set_rave_theme()
  
  mds_res = cmdscale(dist(res$indata, method = input$mds_distance_method), k=2)
  # ravebuiltins:::set_palette_helper
  assign('res', res, envir = globalenv())
  
  #colors
  collapsed_data <- res$collapsed

  pcs = 1:2
  plot(mds_res[,pcs], type = 'n',xlab = 'X', ylab = 'Y')
  text(mds_res[,pcs], labels = paste0(collapsed_data$Subject, 
                                          collapsed_data$Electrode), col = res$colors[res$clusters_res])
  # legend('topright', sprintf('Cluster %d', seq_along(unique(res$clusters_res)),
  #                            bty='n', text.font = 2, text.col = res$colors[seq_along(unique(res$clusters_res))]))
         
}


cluster_membership <- function(){
  
  if(is.list(local_data$my_results)){
    tbl = convert_cluster_table(local_data$my_results$cluster_table,split_by = 'Subject', var = "Cluster", value = "Electrode")
    div(
      style = 'overflow-x:scroll; height: 380px',
      HTML(knitr::kable(tbl, format = 'html', row.names = FALSE, table.attr = 'class="table shiny-table table-striped"')) # spacing-xs
    )
  }else{
    div(
      style = 'height: 380px',
    )
  }
  
}


dendrogram_output <- function() {
  res <- local_data$my_results
  
  shiny::validate(shiny::need(!is.null(res$cluster_mse), message = 'Please press "Run Analysis" '))
  
  shiny::validate(shiny::need(input$input_method == 'H-Clust', 'Only available for method = H-clust'))
  
  shiny::validate(shiny::need('hclust' %in% class(local_data$cluster_method_output), message = 'Please press "Run Analysis" '))
  
  labels = res$collapsed %$% paste0(Subject, Electrode)
  
  leafCol <- function(x){
    if(stats::is.leaf(x)){
      attr(x,'label') <- labels[x]
      attr(x, 'nodePar') <- list(lab.col = res$colors[res$clusters_res[x]],pch = 46) #FIXME
    }
    return(x)
  } 

  rave::set_rave_theme()
  plot(stats::dendrapply(as.dendrogram(local_data$cluster_method_output), leafCol),las = 1) 
  ravebuiltins:::rave_title(sprintf('%s %d %s %d %s','Hierarchical clustering of',length(res$collapsed$Electrode),
                                    'electrodes across',length(unique(res$collapsed$Subject)),'patients'))
}

optimal_output <- function(){
  res <- local_data$my_results
  shiny::validate(
    shiny::need(isTRUE(input$op_run), message = 'Xxx disabled'), 
    shiny::need(!is.null(res)&&!is.null(res$indata), message = 'Please press "Run Analysis" after loading data')
  )
    
  rave::set_rave_theme()
  #, message = 'Please press "Optimal Number of Clusters Analysis" '))
 # observe(input$op_run,{  
  
  methods = c('silhouette','wss')
  if (input$input_method == "H-Clust"){
    clustfun = factoextra::hcut
  } else if (input$input_method == "PAM") {
    clustfun = cluster::pam
  }
  par(mfrow= c(1,2))
  
  op_res <- lapply(methods, function(x){
    factoextra::fviz_nbclust(res$indata,FUNcluster = clustfun, method =x, 
                             k.max = ceiling(dim(res$indata)[1]/2))
  })
  junk <- lapply(op_res, function(x){
    plot(x$data$y, pch = 20, type = 'o', xlab = x$labels$x, ylab =x$labels$y, lwd=2,las = 1)
    lst <- sort(x$data$y, index.return=TRUE, decreasing=TRUE)
    if(!is.null(x$labels$xintercept)){points(lst$ix[1:3],lst$x[1:3],col = 'red',pch =19)}
    }
  )
  #})

}


cluster_plot <-  function(){
  
 palette(ravebuiltins:::rave_colors$GROUP)
  
 res <- local_data$my_results
 
 nclust = length(unique(res$clusters_res))
 
 shiny::validate(shiny::need(!is.null(res$cluster_mse), message = 'Please press "Run Analysis" '))
 
 rave::set_rave_theme()
 if( nclust <= 4 ){
   par(mfrow = c(1, nclust))
 }else{
   nrow = ceiling((nclust) / 4)
   par(mfrow = c(nrow, 4))
 }
 par(mar = c(2,4.1, 4.1, 2))
 
 #local_data = ...local_data
 time_points =  unique(local_data$analysis_data_raw$data$Time[local_data$analysis_data_raw$data$Time 
                                                              %within% res$time_range])
 n_timepoints = length(time_points)
 group_names = res$group_names
 n_cond_groups = length(group_names)
 #res
 yrange = c(min(sapply(res$cluster_mse, function(x){
   x[2,is.na(x[2,])] = 0
   min(x[1,]-x[2,], na.rm = TRUE)
 }))-1
   ,max(sapply(res$cluster_mse, function(x){
   x[2,is.na(x[2,])] = 0
   max(colSums(x), na.rm = TRUE)
 }))+1)
 xaxi = pretty(time_points)
 # yaxi = pretty(yrange)
 
 junk <- dipsaus::iapply(res$cluster_mse,function(x, cl_idx){
   # x = res$cluster_mse[[1]]
   # cl_idx = 1
   # time_points = preload_info$time_points
   cl_mean = x[1,]
   cl_sd = x[2,]
   
   #time_columns = names(res$indata)
   time_columns = res$time_columns
   
   # case 1 variable y-lim
   yrange = range(cl_mean, cl_mean+cl_sd, cl_mean-cl_sd, na.rm = TRUE)
   # case 2 fixed yrange for all plots
   rutabaga::plot_clean(time_points, ylim=yrange) ##FIXME
   
   
   #gnames = NULL
   #j=1
   cols = seq_len(n_cond_groups)
   lapply(seq_len(n_cond_groups), function(j){
     
     sel = stringr::str_ends(time_columns, paste0('_', j))
     time = as.numeric(stringr::str_extract(time_columns[sel], '^[^_]+'))
     
     rutabaga::ebar_polygon(time, cl_mean[sel], sem = cl_sd[sel], col = cols[[j]])
   })
   
   # gc <- mapply(function(sub_x,ii){
   #   lines(time_points, sub_x, col =ii)
   #   cols <- c(cols,ii)
   #   gnames <- c(gnames,input$input_groups[[ii]]$group_name)
   #   print(input$input_groups[[ii]]$group_name)
   #   return(list(gnames = gnames, cols = cols))
   # }, x, seq_along(input$input_groups))
   yaxi = pretty(yrange)
   rutabaga::ruta_axis(1, xaxi)
   rutabaga::ruta_axis(2, yaxi)
   legend('topright', group_names, bty='n', text.font = 2, text.col = cols)
   ravebuiltins:::rave_title(sprintf('%s%d (n=%d)','Cluster', cl_idx, sum(res$clusters_res == cl_idx)), col =res$colors[cl_idx]) 
 }
 )

}

# cluster_plot1 <- function(){
#   res <- local_data$my_results
#   shiny::validate(shiny::need(!is.null(res$cluster_means), message = 'Please press "Run Analysis" '))
#   
#   time_points =  res$time_points
#   ymax = max(unlist(res$cluster_means))
#   xaxi = pretty(time_points)
#   yaxi = pretty(c(-100, ymax))
#   rutabaga::plot_clean(time_points, ylim=c(-100, ymax)) ##FIXME
#   rutabaga::ruta_axis(1, xaxi)
#   rutabaga::ruta_axis(2, yaxi)
#   
#   invisible(lapply(unique(res$clusters_res),function(x){
#     lines(time_points, res$cluster_means[[x]], col = res$colors[as.character(x)])
#   }))#FIXME
# 
# }
  

viewer_3d_fun <- function(...){
  # brain = rave::rave_brain2('congruency/YAB')
  res <- local_data$my_results
  
  subjects = local_data$analysis_data_raw$subjects
  project_name = subject$project_name
  
  tbl = res$cluster_table
  tbl$Cluster = paste('Cluster', tbl$Cluster)
  tbl$Project = project_name
  
  if(!is.data.frame(tbl)){
    return(NULL)
  }
  
  bs = lapply(subjects, function(sub){
    rave::rave_brain2(sprintf('%s/%s', project_name, sub))
  })
  brain = threeBrain::merge_brain(.list = bs)
  
  brain$set_electrode_values(tbl)
  
  brain$plot(side_width = 160, side_shift = c(0,0), 
             palettes = list('Cluster' = res$colors))
}

# cluster_plot2 <- function(){
#   res <- local_data$my_results
#   
#   shiny::validate(
#     shiny::need(!is.null(res$cluster_means), message = 'Please press "Run Analysis" button.')
#   )
#   
#   if( res$input_nclusters <= 3 ){
#     par(mfrow = c(1, res$input_nclusters))
#   }else{
#     nrow = ceiling(res$input_nclusters / 3)
#     par(mfrow = c(nrow, 3))
#   }
#   
#   time_points = as.numeric(names(res$indata))#preload_info$time_points
#   
# cluster_vis <- mapply(function(cm, ii) {
#   
#     rutabaga::plot_clean(time_points, ylim=c(-100, 500)) ##FIXME
#     gnames = list()
#     cols = NULL
#     
#     for( j in seq_along(input_groups) ){
#       g = input_groups[[j]]
#       conditions = g$group_conditions
#       gnames[[j]] = g$group_name
#       lines(time_points, colMeans(res$indata[which(res$collapsed$ConditionGroup %in% g$group_name & res$clusters_res ==ii),]),
#             col=j)
#       
#       cols = c(cols, j)
#     }
#     
#     # lines(baselined$dimnames$Time, colMeans(cm[which(baselined$dimnames$Trial %in% v_trials),]),
#     #       col="#377EB8")
#     
#     legend('topright', unlist(gnames), bty='n', text.font = 2,
#            text.col = cols)
#     text(quantile(time_points, 0.15),200, paste0('n_elec=', sum(res$clusters_res == ii)))# didn't work
#     title(main=rutabaga::deparse_svec(res$collapsed$Electrode[which(ii == res$clusters_res)]))
#     
#     rutabaga::ruta_axis(1, pretty(time_points))
#     rutabaga::ruta_axis(2, pretty(-100:1000))
#     
#   }, res$cluster_means, seq_along(res$cluster_means))
# }

