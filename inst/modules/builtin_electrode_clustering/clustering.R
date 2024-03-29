# if(is_invalid(input_nclusters)){
#   input_nclusters = 1
# }
# input_nclusters = min(input_nclusters, length(preload_info$electrodes))


#----------------------------------------------


observeEvent(input$do_run, {
  res = clustering_analysis()
  local_data$my_results = res
  
  
  # update the number of clusters to the actual number of clusters obtained, rather than the requested number of clusters
  updateNumericInput(getDefaultReactiveDomain(), 'input_nclusters', value = length(unique(res$clusters_res)))
  
})
# 
# observeEvent(input$analysis_data_btn, {
#   # print('trying to count # els')
#   
#   # uniq_electrodes = unique(local_data$analysis_data_raw$data %$% paste0(Subject, Electrode))
#   # print(uniq_electrodes)
#   # 
#   # updateNumericInput(getDefaultReactiveDomain(),inputId = 'input_nclusters', max = length(uniq_electrodes))
# })



#-----------------------------------------

# group_data = lapply(input_groups, function(g){
#   g$trial_number = trial_data$Trial[ trial_data$Condition %in% unlist(g$group_conditions) ]
#   g
# })
# print(group_data)
#all_trials = unique(unlist(lapply(group_data, '[[', 'trial_number')))




#----------------------------------------------
clustering_analysis <- function(){
  # Get data
  #local_data = ...local_data
  #input = ...input
  
  progress = progress("Run clustering", max = 5)
  on.exit({
    progress$close()
  })
  
  progress$inc("Apply ROI filter")
  raw_table <- local_data$analysis_data_raw$data
  
  
  #var_name = names(raw_table)[3]
  var_name = input$trial_selected[[1]]
  
  # subset with only the selected ROI variable
  roi_list <- c("VAR_IS_ROI_Hemisphere", "VAR_IS_ROI_freesurferlabel", 
                "VAR_IS_ROI_Group", "VAR_IS_ROI_Block")
  roi_var<- paste0('VAR_IS_ROI_',input$model_roi_variable)
  
  raw_table = raw_table[, !names(raw_table) %in% roi_list[!roi_list %in% roi_var]]

  #select based on the ROI selector
  use_regex <- ( input$roi_ignore_gyrus_sulcus || input$roi_ignore_hemisphere )
  
  raw_table <- table_apply_roi(table = raw_table, roi_column = roi_var,
                               roi = input$filter_by_roi, use_regex = use_regex)
  
  progress$inc("Collapsing group data")
  
  collapsed = lapply(seq_along(input$input_groups), function( ii ){
    
    group = input$input_groups[[ ii ]]
    
    group_name = group$group_name
    
    if(is.null(group_name) && group_name == ''){
      group_name = sprintf('Group %d', ii)
    }
    
    group_condition = group$group_conditions

    print(paste0('start data deformation... Group', ii))
    
    #plot data
    sub_plot =raw_table[raw_table$Condition %in% group_condition & 
                          raw_table$Time %within% input$plot_time_window, ]
    
    sub = sub_plot[sub_plot$Time %within% input$time_window,]
    
    sub_plot$Time = paste0(sub_plot$Time, '_', ii)
    
    sub_time = paste0(sub$Time, '_',ii)
    
    fml <- Subject + Electrode + VAR_IS_ROI_freesurferlabel ~ Time
    fml[[2]][[3]] <- parse(text = roi_var)[[1]]
    
    collapsed_mean <- lapply(var_name, function(var){
      reshape2::dcast(
          sub_plot,
          fml,
          fun.aggregate = mean, value.var = var
        )
    }
    )
    
    merged <- Reduce(function(a, b){
      # b <- collapsed[[1]]
      merge(a, b, all = FALSE, 
            by = c("Subject", 'Electrode',roi_var))
    }, collapsed_mean)

    return(list(
      collapsed_mean = merged,
      group_name = group_name,
      group_index = ii,
      sub_time = sub_time
    ))
  })
  
  
  # get the baseline mean
  baseline = lapply(seq_along(input$input_groups), function( ii ){
    group = input$input_groups[[ ii ]]
    
    group_name = group$group_name
    
    if(is.null(group_name) && group_name == ''){
      group_name = sprintf('Group %d', ii)
    }
    
    group_condition = group$group_conditions
    
    baseline_raw = raw_table[raw_table$Condition %in% group_condition &
                raw_table$Time %within% input$baseline_time, ]
    
    fml <- Subject + Electrode + VAR_IS_ROI_freesurferlabel ~ Time
    fml[[2]][[3]] <- parse(text = roi_var)[[1]]
    
    # baseline_mean <- lapply(var_name, function(var){
    #   reshape2::dcast(
    #     baseline_raw,
    #     fml,
    #     fun.aggregate = mean, value.var = var
    #   )
    # }
    # )
    
    baseline_mean <- reshape2::dcast(
      baseline_raw,
      fml,
      fun.aggregate = mean, value.var = var_name
    )
    
    return(baseline_mean)

  })

  baseline_merged = Reduce(function(a, b){
    # b <- collapsed[[1]]
    
    baseline_mean = merge(a, b, all = FALSE, 
                           by = c("Subject", 'Electrode',roi_var))
    baseline_mean
    
  }, baseline, right = FALSE)

  baseline_mean_indata <- baseline_merged[,!names(baseline_merged) %in% c("Subject", 'Electrode',roi_var)]
  baseline_mean <- rowMeans(baseline_mean_indata)
  baseline_sd <- apply(baseline_mean_indata,1,sd)
  
  

  group_names = sapply(collapsed, '[[', 'group_name')#FIXME
  # merge(..., all=TRUE) will keep all the IDs, FALSE will be inner join
  
  #showing progress message
  progress$inc("Merging collapsed data")
  
  merged = Reduce(function(a, b){
    # b <- collapsed[[1]]
    list(
      collapsed_mean = merge(a$collapsed_mean, b$collapsed_mean, all = FALSE, 
                             by = c("Subject", 'Electrode',roi_var)),
      sub_time = c(a$sub_time, b$sub_time)
    )
  }, collapsed, right = FALSE)
  
  
  collapsed = merged$collapsed_mean
  
  ## For merge(..., all=TRUE), there might be some cases where 
  ## one subject miss some of the condition groups
  ## We need to show warnings to the user
  #
  # if(any(is.na(collapsed))){
  #   showNotification(p(''))
  # }
  
  #' asad
  #' dd
  #' @noRd
  
  indata = collapsed[, names(collapsed) %in% merged$sub_time]
  # baselined = baseline(
  #   power$subset(
  #     Trial=Trial %in% all_trials, Frequency = Frequency %within% input_frequencies,
  #     Electrode = Electrode %in% parse_selections(text_electrode)
  #   ),
  #   from = input_baseline_range[1], to = input_baseline_range[2])
  # 
  # collapsed = baselined$collapse(keep = c(1, 3, 4), method = 'mean')
  
  #
  # print(dim(power))
  #
  # print( input_frequencies )
  # print( input_groups )
  # requested_electrodes = parse_selections(text_electrode)
  #
  # time_points = preload_info$time_points
  # frequencies = preload_info$frequencies
  #
  # trial = module_tools$get_meta('trials')
  
  # list2env(as.list(..param_env), envir = globalenv())
  # list2env(as.list(environment()), envir = globalenv()

  progress$inc("Running MDS on merged data")
  
  #z-score
  if (input$check_scale) {#with or without 'input', what is the difference? 
    
    indata = t(scale(t(indata),center = baseline_mean, baseline_sd))
    collapsed[, !names(collapsed) %in% c('Subject', 'Electrode', roi_var)] <- 
      t(scale(t(collapsed[, !names(collapsed) %in% c('Subject', 'Electrode', roi_var)]),
              center = baseline_mean, baseline_sd))
  }
  
  #MDS
  if(ncol(indata) <= 2 ){
    mds_res = NULL
    }else if(input$mds_distance_method == '1 - correlation') {
      dis = as.dist(1-cor(t(indata)))
      mds_res = cmdscale(dis, k=2)
    }else{
      dis = dist(indata, method = input$mds_distance_method)
      mds_res = cmdscale(dis, k=2)
    }
  



  
  #clustering
  progress$inc('start clustering analysis')
  
  n_clust = min(input$input_nclusters, nrow(collapsed))
  
  #get the distance metric for clustering

  if (isTRUE(input$distance_method == '1 - correlation')) {
    dis = as.dist(1-cor(t(indata)))
  }else if(isTRUE(input$distance_method == 'DTW')){
    dis =as.dist(dtwDist(indata))
  }else{
    dis = dist(indata, method = input$distance_method)
  }
  
  
  if (input$input_method == "H-Clust"){
    hcl = stats::hclust(dis, method = input$hclust_method)
    local_data$cluster_method_output = hcl
    clusters <- stats::cutree(hcl, k = n_clust)
  } else if (input$input_method == "K-Medois") {
    #km <- kmeans(indata, centers = n_clust,iter.max = 100, )
    #clusters <- km$cluster
    km <- cluster::pam(dis, k = n_clust,  diss = TRUE,
                       cluster.only = TRUE, keep.data = FALSE, 
                       keep.diss = FALSE)
    local_data$cluster_method_output = km
    clusters <- km
  }
  
  #clusters = res$clusters_res 
  #indata = res$indata
  mse <- lapply(sort(unique(clusters)), function(ci){
    # rutabaga::collapse(indata[clusters == ci, , drop = FALSE], average = TRUE, keep = 2)
     apply(collapsed[clusters == ci,
                     !names(collapsed) %in% c('Subject', 'Electrode', roi_var),
                     drop=FALSE], 
           2, dipsaus::mean_se)
    
    #cluster_mean = colMeans(indata[clusters == c0i, , drop = FALSE])
    
    # Calculate total variances
    # total_var = colSums(merged$collapsed_var[clusters == ci, -c(1,2), drop = FALSE])
    # total_df = colSums(merged$collapsed_df[clusters == ci, -c(1,2), drop = FALSE])
    
     
    #rbind(cluster_mean, sqrt(total_var / total_df))
  })
  
  #clusters color code
  colors = ravebuiltins::get_palette("Dark2")
   
   
  names(colors)=unique(clusters)
  

  # print('cluster finished')
  # plot(tsne$Y, t = 'n')
  # text(tsne$Y, labels = baselined$dimnames$Electrode, col = colors[clusters])
  
  # visualize the clustering results
  #collapsed
  
  cluster_table = collapsed[,1:2]
  cluster_table$Cluster = clusters
  
  roi = collapsed[,3]
  
  return(list(
    collapsed = collapsed,
    indata = indata,#FIXME
    mds_res = mds_res,
    clusters_res = clusters,
    cluster_table = cluster_table,
    roi = roi,
    mse =  mse,
    dis = dis,
    input_nclusters = input$input_nclusters,
    #time_points = as.numeric(names(indata)),
    colors = colors,
    group_names = group_names,
    time_range = input$time_window,
    time_range_plot = input$plot_time_window
  ))
}



