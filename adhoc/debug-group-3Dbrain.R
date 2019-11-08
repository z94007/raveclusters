# One-time code. Once ran, don't have to run again
require(raveclusters)
env = dev_raveclusters(T)

# Help for debug
mount_demo_subject()

subject$info()


project_name = subject$project_name
subjects = shiny::isolate(local_data$analysis_data_raw$subjects)

brains = lapply(subjects, function(sub){
  rave::rave_brain2(paste0(project_name, '/', sub))
})
template = threeBrain::merge_brain(.list = brains)


clusters = factor(1:10)

template$set_electrode_values(
  data.frame(
    Project = 'congruency',
    Subject = 'YAB',
    Electrode = 14,
    Time = 0,
    CondGroup1 = clusters[3],
    CondGroup2 = clusters[1]
  )
)

template$plot()
# Project  Subject  Electrode  Time  Cluster of CondGroup1  Cluster of CondGroup2 ...
# congruency  YAB   14         0     
