# Main algorithm - rave_executes

# Initialize inputs
devtools::document()
raveclusters:::dev_raveclusters(T)
mount_demo_subject()

init_module(module_id = 'group_analysis_lme', debug = TRUE)

# >>>>>>>>>>>> Start ------------- [DO NOT EDIT THIS LINE] ---------------------
######' @auto=TRUE
# 
# assign('...local_data', shiny::isolate(shiny::reactiveValuesToList(local_data)), envir = globalenv())
# assign('...input', shiny::isolate(shiny::reactiveValuesToList(input)), envir = globalenv())



# not really used
# <<<<<<<<<<<< End ----------------- [DO NOT EDIT THIS LINE] -------------------

# Debug
# require(raveclusters)
# devtools::document()
raveclusters::dev_raveclusters(expose_functions = T)
# reload_this_package(expose = T, clear_env = F)
mount_demo_subject(force_reload_subject = T)

# module_id = 'group_analysis_lme'
module = raveclusters:::debug_module('group_analysis_lme')
# 
# result = module(ANALYSIS_WINDOW = 0)
# result$phase_histogram()
# result$itpc_plot()
# result$itpc_time_plot()
# result$phase_plot()

results = result$results

view_layout('group_analysis_lme', sidebar_width = 3, launch.browser = T)


