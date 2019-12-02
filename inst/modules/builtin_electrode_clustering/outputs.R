
# local_data = ...local_data
# input = ...input


tsne_plot <- function(){
  res <- local_data$my_results
  
  
  shiny::validate(
    shiny::need(!is.null(res$indata) && ncol(res$indata) > 2, message = 'Please press "Run Analysis" button.'),
    shiny::need(length(res$input_nclusters) && 
                  !is.na(res$input_nclusters) &&
                  res$input_nclusters > 1, message = 'Number of clusters must be greater than 1')
  )
  
  mds_res = cmdscale(dist(res$indata, method = input$mds_distance_method), k=2)
  
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


cluster_plot <-  function(){
  
 palette(ravebuiltins:::rave_colors$GROUP)
  
 res <- local_data$my_results
 
 shiny::validate(shiny::need(!is.null(res$cluster_mse), message = 'Please press "Run Analysis" '))
 
 if( res$input_nclusters <= 3 ){
   par(mfrow = c(1, res$input_nclusters))
 }else{
   nrow = ceiling((res$input_nclusters) / 3)
   par(mfrow = c(nrow, 3))
 }
 #local_data = ...local_data
 time_points =  unique(local_data$analysis_data_raw$data$Time[local_data$analysis_data_raw$data$Time %within% res$time_range])
 n_timepoints = length(time_points)
 group_names = res$group_names
 n_cond_groups = length(group_names)
 res
 yrange = c(min(sapply(res$cluster_mse, function(x){
   x[2,is.na(x[2,])] = 0
   min(x[1,]-x[2,], na.rm = TRUE)
 }))-1
   ,max(sapply(res$cluster_mse, function(x){
   x[2,is.na(x[2,])] = 0
   max(colSums(x), na.rm = TRUE)
 }))+1)
 xaxi = pretty(time_points)
 yaxi = pretty(yrange)
 
 a<- dipsaus::iapply(res$cluster_mse,function(x, cl_idx){
   #x = res$cluster_mse[[1]]
   cl_mean = x[1,]
   cl_sd = x[2,]
   
   #time_columns = names(res$indata)
   time_columns = res$time_columns
   
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
   
   
   rutabaga::ruta_axis(1, xaxi)
   rutabaga::ruta_axis(2, yaxi)
   legend('topright', group_names, bty='n', text.font = 2, text.col = cols)
   title(sprintf('Cluster %d', cl_idx), col.main =res$colors[cl_idx])
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

