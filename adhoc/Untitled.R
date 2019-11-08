

if( !exists('do_run') || !isTRUE(do_run != 0) ){
  print('initializing')
  return()
}

if(is_invalid(input_nclusters)){
  input_nclusters = 1
}
input_nclusters = min(input_nclusters, length(preload_info$electrodes))

#

group_data = lapply(input_groups, function(g){
  g$trial_number = trial_data$Trial[ trial_data$Condition %in% unlist(g$group_conditions) ]
  g
})

# print(group_data)

all_trials = unique(unlist(lapply(group_data, '[[', 'trial_number')))




# Get data





distance_method <- 'manhattan'

collapsed = lapply(input_groups, function(group){
  # collapsed_data = rutabaga::do_aggregate(data = dat[dat$Condition %in% group$conditions, ], 
  #                                         Power ~ Subject + Electrode + Time, FUN = mean)
  # collapsed_data$ConditionGroup = group$condition_name
  # collapsed_data
  
  collapsed_data = reshape2::dcast(
    analysis_data_raw$data[analysis_data_raw$data$Condition %in% group$conditions, ],
    Subject + Electrode ~ Time, 
    fun.aggregate = mean, value.var = 'Power'
  )
  collapsed_data$ConditionGroup = group$condition_name
  return(collapsed_data)
})
collapsed = do.call('rbind', collapsed)


# numerical data without subject and electrode names
indata = collapsed[, !names(collapsed) %in% c('Subject', 'Electrode', 'ConditionGroup')]



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

#--------


set.seed(123)
#---------------
# require(rave)
# require(rutabaga)
#---------------

# problem line

if (input_method == "H-Clust"){
  hcl = hclust(stat::dist(indata, method = distance_method), method = 'ward.D')
  clusters <- cutree(hcl, k = input_nclusters)
} else if (input_method == "K-Means") {
  km <- kmeans(indata, centers = input_nclusters, iter.max = 100)
  clusters <- km$cluster
}

cluster_means <- lapply(unique(clusters), function(ci){
  ii <- clusters == ci
  rutabaga::collapse(indata[, ii], average = TRUE, keep = c(1,2))
})

#tsne to visualize
colors = rainbow(length(unique(clusters)))
# # colors = 
names(colors)=unique(clusters)

if( ncol(coll) <= 2 ){
  pca_res = NULL
}else{
  # pca_res <- Rtsne::Rtsne(t(coll),dims = 2,perplexity = length(preload_info$electrodes) / 5, verbose = T, max_iter =20)
  # pca_res <- prcomp(t(coll))
  
  mds_res = cmdscale(dist(indata, method = 'canberra'), k=2)
  
}

# plot(tsne$Y, t = 'n')
# text(tsne$Y, labels = baselined$dimnames$Electrode, col = colors[clusters])

# visualize the clustering results

