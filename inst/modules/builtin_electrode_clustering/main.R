# Main algorithm - rave_executes

require(raveclusters)

# Initialize inputs
dev_raveclusters(expose_functions = TRUE)

mount_demo_subject()

init_module('builtin_electrode_clustering', debug = TRUE)

# input_groups = list(
#   list(
#     group_name = 'v-only',
#     group_conditions = unique(trial_data$Condition[endsWith(trial_data$Condition,'_v')])
#   ),
#   list(
#     group_name = 'a-only',
#     group_conditions = unique(trial_data$Condition[endsWith(trial_data$Condition,'_a')])
#   )
# )

# >>>>>>>>>>>> Start ------------- [DO NOT EDIT THIS LINE] ---------------------
######' @auto=FALSE


# <<<<<<<<<<<< End ----------------- [DO NOT EDIT THIS LINE] -------------------

# Debug

raveclusters::dev_raveclusters(expose_functions = TRUE)

# Debug - offline:
main = raveclusters:::debug_module('builtin_electrode_clustering')
ret = main()
result = ret$results

result$get_value('preload_info')


# Debug - online:
rave::rave_options(
  raw_data_dir ='/Volumes/data/rave_data/raw',
  data_dir ='/Volumes/data/rave_data/ent_data',
  rave_data_dir ='/Volumes/data/rave_data/ent_data'
)

raveclusters::dev_raveclusters(expose_functions = TRUE)
mount_demo_subject()
view_layout('builtin_electrode_clustering', sidebar_width = 3, launch.browser = T)
