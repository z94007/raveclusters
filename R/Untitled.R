if(FALSE){
f = c(
  '/Volumes/data/rave_data/ent_data/demo/_project_data/power_explorer/exports/KC_pow_by_cond-20191018-023810.csv',
  '/Volumes/data/rave_data/ent_data/demo/_project_data/power_explorer/exports/YAB_pow_by_cond-20191018-024047.csv',
  '/Volumes/data/rave_data/ent_data/demo/_project_data/power_explorer/exports/YAI_pow_by_cond-20191018-024856.csv'
)


dat = do.call('rbind', dat)


groups = list(
  list(
    condition_name = 'Auditory_Voice_Lead',
    conditions = c('drive_a', 'last_a')
  ),
  list(
    condition_name = 'Auditory_Mouth_Lead',
    conditions = c('meant_a', 'last_a')
  ),
  list(
    condition_name = 'Visual-only',
    conditions = c('meant_v', 'last_v', 'drive_v', 'last_v')
  ),
  list(
    condition_name = 'Audotiry-only',
    conditions = c('drive_a', 'drive_a', 'meant_a', 'meant_a')
  )
)

# dat$elec_name = paste0(dat$Subject, '-', dat$Electrode)

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
hcl = stats::hclust(dist(indata, method = 'manhattan'),method = 'ward.D')
hcl = stats::hclust(stats::as.dist(1-cor(t(indata))),method = 'ward.D')

plot(hcl)
clusters_hc <- cutree(hcl, k)

km <- kmeans(indata, k, iter.max = 100)
clusters_km <- km$cluster


cluster_means <- lapply(unique(clusters_km), function(ci){
  ii <- clusters_km == ci
  colMeans(indata[ii,])
})



si3 <- cluster::silhouette(clusters_hc,cluster::daisy(indata))
si3
plot(si3)
sortSilhouette(si3)
summary(si3, FUN = mean)


mds_res = cmdscale(dist(indata, method = 'canberra'), k=2)

plot(mds_res)




}