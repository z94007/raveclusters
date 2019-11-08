dat <- analysis_data_raw$data

groups  <- input_groups

distance_method <- 'manhattan'

collapsed = lapply(groups, function(group){
  # collapsed_data = rutabaga::do_aggregate(data = dat[dat$Condition %in% group$conditions, ], 
  #                                         Power ~ Subject + Electrode + Time, FUN = mean)
  # collapsed_data$ConditionGroup = group$condition_name
  # collapsed_data
  
  collapsed_data = reshape2::dcast(
    dat[dat$Condition %in% group$conditions, ],
    Subject + Electrode ~ Time, 
    fun.aggregate = mean, value.var = 'Power'
  )
  collapsed_data$ConditionGroup = group$condition_name
  return(collapsed_data)
})
collapsed = do.call('rbind', collapsed)


# numerical data without subject and electrode names
indata = collapsed[, !names(collapsed) %in% c('Subject', 'Electrode', 'ConditionGroup')]

#clustering
k = 5; set.seed(123)
hcl = stats::hclust(dist(indata, method = distance_method),method = 'ward.D')
hcl = stats::hclust(stats::as.dist(1-cor(t(indata))),method = 'ward.D')

plot(hcl)
clusters_hc <- cutree(hcl, k)

km <- kmeans(indata, k, iter.max = 100)
clusters_km <- km$cluster

si3 <- silhouette(clusters_hc,daisy(indata))
si3
plot(si3)
sortSilhouette(si3)
summary(si3, FUN = mean)


mds_res = cmdscale(dist(indata, method = 'canberra'), k=2)

plot(mds_res)
