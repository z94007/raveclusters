# if(is_invalid(input_nclusters)){
#   input_nclusters = 1
# }
# input_nclusters = min(input_nclusters, length(preload_info$electrodes))


#----------------------------------------------

observeEvent(input$do_run, {
  print('lalalala')#FIXME
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
  #input_groups = ...input$input_groups
  #input = ...input
  raw_table = local_data$analysis_data_raw$data
  
  #var_name = names(raw_table)[3]
  var_name = input$trial_selected
  
  collapsed = lapply(seq_along(input$input_groups), function( ii ){
    group = input$input_groups[[ ii ]]
    group_name = group$group_name
    if(is.null(group_name) && group_name == ''){
      group_name = sprintf('Group %d', ii)
    }
    group_condition = group$group_conditions
    # collapsed_data = rutabaga::do_aggregate(data = dat[dat$Condition %in% group$conditions, ], 
    #                                         Power ~ Subject + Electrode + Time, FUN = mean)
    # collapsed_data$ConditionGroup = group$condition_name
    # collapsed_data
    print('afaljfl')
    
    sub = raw_table[raw_table$Condition %in% group_condition 
                                            & raw_table$Time %within% input$time_window, ]
    sub$Time = paste0(sub$Time, '_', ii)
    collapsed_mean = reshape2::dcast(
      sub,
      Subject + Electrode ~ Time, 
      fun.aggregate = mean, value.var = var_name
    )
    
    # collapsed_var = reshape2::dcast(
    #   sub,
    #   Subject + Electrode ~ Time, 
    #   fun.aggregate = function(x){
    #     var(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))
    #   }, value.var = 'Power'
    # )
    # collapsed_df = reshape2::dcast(
    #   sub,
    #   Subject + Electrode ~ Time, 
    #   fun.aggregate = function(x){
    #     sum(!is.na(x))
    #   }, value.var = 'Power'
    # )
    # collapsed_mean$ConditionGroup = group_name
    
    
    return(list(
      collapsed_mean = collapsed_mean,
      # collapsed_var = collapsed_var,
      # collapsed_df = collapsed_df,
      group_name = group_name,
      group_index = ii
    ))
  })
  
  group_names = sapply(collapsed, '[[', 'group_name')
  # merge(..., all=TRUE) will keep all the IDs, FALSE will be inner join
  merged = Reduce(function(a, b){
    list(
      # collapsed_var = merge(a$collapsed_var, b$collapsed_var, all = FALSE, 
      #                       by = c("Subject", 'Electrode')),
      # collapsed_df = merge(a$collapsed_df, b$collapsed_df, all = FALSE, 
      #                       by = c("Subject", 'Electrode')),
      collapsed_mean = merge(a$collapsed_mean, b$collapsed_mean, all = FALSE, 
                             by = c("Subject", 'Electrode'))
    )
  }, collapsed)
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
  
  #collapsed = do.call('cbind', collapsed)
  # numerical data without subject and electrode names
  indata = collapsed[, !names(collapsed) %in% c('Subject', 'Electrode')]
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
  # list2env(as.list(environment()), envir = globalenv())
  
  
  #set.seed(123)
  
  #z-scoring
  time_columns = names(indata)
  #indata = t(scale(t(indata)))#FIXME
  
  #MDS
  if( ncol(indata) <= 2 ){
    mds_res = NULL
  }else{
    # pca_res <- Rtsne::Rtsne(t(coll),dims = 2,perplexity = length(preload_info$electrodes) / 5, verbose = T, max_iter =20)
    # pca_res <- prcomp(t(coll))
    
    mds_res = cmdscale(dist(indata, method = input$mds_distance_method), k=2)
    
  }
  

  if (check_scale) {
    indata = t(scale(t(indata)))
  }
  
  
  #clustering
  print('start clustering analysis')
  
  n_clust = min(input$input_nclusters, nrow(collapsed))
  
  #input = ...input
  dis = stats::dist(indata, method = input$distance_method)
  if (input$input_method == "H-Clust"){
    hcl = stats::hclust(dis, method = 'ward.D')
    local_data$cluster_method_output = hcl
    clusters <- cutree(hcl, k = n_clust)
  } else if (input$input_method == "PAM") {
    #km <- kmeans(indata, centers = n_clust,iter.max = 100, )
    #clusters <- km$cluster
    km <- cluster::pam(dis, k = n_clust,  cluster.only = TRUE, keep.data = FALSE, keep.diss = FALSE)
    local_data$cluster_method_output = km
    clusters <- km
  }
  
  #clusters = res$clusters_res 
  #indata = res$indata
  cluster_mse <- lapply(sort(unique(clusters)), function(ci){
    # rutabaga::collapse(indata[clusters == ci, , drop = FALSE], average = TRUE, keep = 2)
     apply(indata[clusters == ci,,drop=FALSE], 2, rutabaga::m_se)
    
    #cluster_mean = colMeans(indata[clusters == c0i, , drop = FALSE])
    
    # Calculate total variances
    # total_var = colSums(merged$collapsed_var[clusters == ci, -c(1,2), drop = FALSE])
    # total_df = colSums(merged$collapsed_df[clusters == ci, -c(1,2), drop = FALSE])
    
     
    #rbind(cluster_mean, sqrt(total_var / total_df))
  })
  
  #color pattern
  colors = ravebuiltins::get_palette("Dark2")
   
  names(colors)=unique(clusters)
  

  print('cluster finished')
  # plot(tsne$Y, t = 'n')
  # text(tsne$Y, labels = baselined$dimnames$Electrode, col = colors[clusters])
  
  # visualize the clustering results
  #collapsed
  
  cluster_table = collapsed[,1:2]
  cluster_table$Cluster = clusters
  
  return(list(
    collapsed = collapsed,
    indata = indata,#FIXME
    mds_res = mds_res,
    clusters_res = clusters,
    cluster_table = cluster_table,
    cluster_mse =  cluster_mse,
    input_nclusters = input$input_nclusters,
    #time_points = as.numeric(names(indata)),
    colors = colors,
    group_names = group_names,
    time_range = input$time_window,
    time_columns = time_columns
    
  ))
}



