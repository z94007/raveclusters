# File defining module inputs, outputs

# ----------------------------------- Debug ------------------------------------
require(raveclusters)

env = dev_raveclusters(T)

#' Load subject for debugging
#' Make sure this is executed before developing the module to make sure
#' at least one subject is loaded
rave::mount_demo_subject()

# rave::init_module('builtin_electrode_clustering')


# >>>>>>>>>>>> Start ------------- [DO NOT EDIT THIS LINE] ---------------------


#  ----------------------  Initializing Global variables -----------------------

load_scripts(
  get_path('inst/modules/builtin_electrode_clustering/reactives.R'),
  get_path('inst/modules/builtin_electrode_clustering/outputs.R'),
  get_path('inst/modules/builtin_electrode_clustering/clustering.R'),
  #get_path('inst/modules/builtin_electrode_clustering/clustering_evaluation.R'),
  asis = TRUE)

#' define_initialization is executed every time when:
#'   *** a subject is loaded
#'
#' You can use this function to do:
#'   1. Check if data is complete
#'   2. Define some "global" variables
#'
#' Check package vignette to see the explanation
# 
# define_initialization({
#   # Enter code to handle data when a new subject is loaded
#   trial_data <- module_tools$get_meta('trials')
# })




#  ---------------------------------  Inputs -----------------------------------

define_input_analysis_data_csv(
  inputId= 'analysis_data', label = "Data files located in this project's RAVE directory", 
  paths = c('_project_data/group_analysis_lme/source', '_project_data/power_explorer/exports'),
  reactive_target = 'local_data$analysis_data_raw', try_load_yaml = TRUE
)

define_input(
  definition = checkboxInput(inputId = 'check_scale', label = 'Z-score data',
                             value = FALSE
  )
)


# define_input(
#   definition = textInput(inputId = 'text_electrode', label = 'Electrode Not loaded'),
# 
#   init_args = c('label', 'value'),
#   init_expr = {
# 
#     # check ?rave_prepare for what kind of data are loaded
#     loaded_electrodes = preload_info$electrodes
# 
#     # Generate text for loaded electrodes
#     text = deparse_selections(loaded_electrodes)
#     
#     previous_val = parse_selections(cache_input('text_electrode', ''))
#     if( any(previous_val %in% loaded_electrodes) ){
#       previous_val = previous_val[previous_val %in% loaded_electrodes]
#       value = deparse_selections(previous_val)
#     }else{
#       value = text
#     }
# 
#     # Update
#     label = paste0('Electrode (', text, ')')
#   }
# )
#define_input(definition = numericInput(inputId = 'nclusters', label = 'the number of clusters'))


define_input_condition_groups('input_groups', label = 'Condition Group')

define_input(
  definition = numericInput(inputId = 'input_nclusters', label = 'Number of Clusters', 
                            value = 2, min = 1, max = 1e3, step = 1)
)

define_input(
  definition = selectInput(inputId = 'input_method', label = 'Clustering Method',
                           choices = c('H-Clust', 'PAM'), selected = NULL),
  init_args = c('selected'),
  init_expr = {
    selected = cache_input('input_method', 'H-Clust')
  }
)

define_input(
  definition = sliderInput(inputId = 'time_window', label = 'Time Window', 
                min = -1, 
                max= 2,
                value = c(0,1.0),
                step = 0.01)
  # }else{
  #   sliderInput(inputId = 'time_window', label = 'Time Window', 
  #               min = min(local_data$analysis_data_raw$data$Time), 
  #               max= max(local_data$analysis_data_raw$data$Time),
  #               value = c(0,1.0),
  #               step = 0.01)
  # }


)

define_input(
  definition = selectInput(inputId = 'distance_method', label = 'Clustering Distance Measurement',
                           choices = c('euclidean', 'maximum',"manhattan", "canberra", "minkowski"), selected = NULL),
  init_args = c('selected'),
  init_expr = {
    selected = cache_input('distance_method', 'manhattan')
  }
)

define_input(
  definition = selectInput(inputId = 'mds_distance_method',label = 'MDS Distance Measurement',
                           choices = c('euclidean', 'maximum',"manhattan","canberra"), selected = NULL),
  init_args = c('selected'),
  init_expr = {
    selected = cache_input('mds_distance_method', 'manhattan')
  }
)

define_input(
  definition = selectInput(inputId = 'trial_selected', label = 'Select trial',
                           choices = '', selected = NULL)
)
define_input(
  definition = checkboxInput(inputId = 'op_run', label = 'Optimal Number of Clusters Analysis',
                             value = FALSE)
  )


define_input(
  definition = dipsaus::actionButtonStyled('do_run', 'Run Analysis',
                                           type = 'primary', width = '100%')
)

define_input(
  definition = customizedUI('graph_export')
)


input_layout = list(
  'Data Import' = list(
    'analysis_data'
  ),
  #[#99ccff][-]
  'Trial Selector' = list(
    # 'text_electrode',
    'input_groups'
  ),
  'Analysis Settings' = list(
    c( 'input_method', 'input_nclusters' ),
    'trial_selected',
    'time_window',
    'distance_method',
    'mds_distance_method',
    c('check_scale',
    'op_run'),
    'do_run'
  ),
  'Export Settings' = list(
    'graph_export'
  )
)

manual_inputs = c('graph_export','analysis_data', 'input_baseline_range', 
                  # 'text_electrode', 
                  'input_frequencies', 'input_groups', 'input_nclusters',
                  'input_method', unlist(sapply(1:20, function(ii){ paste0('input_groups_', c('group_name', 'group_conditions'), '_', ii) })))

# End of input
# ----------------------------------  Outputs ----------------------------------
#' Define Outputs
#'
#' Use function `define_output` to define outputs.
#' Make sure rave toolbox (dev_[your_package_name]) is loaded.
#'
#' @Usage: define_output(definition, title, width, order)
#'
#' @param definition defines output types, for example:
#'     verbatimTextOutput('text_result')
#'   defines output type that dumps whatever is printed by function `text_result`
#'
#' Here are some possible types
#'   1. textOutput is an output for characters
#'   2. verbatimTextOutput is for console print
#'   3. plotOutput is for figures
#'   4. tableOutput is to tables
#'   5. customizedUI is for advanced UI controls
#'
#'   There are lots of output types and R packages such as DT, threejsr can provide
#'   very useful output types. Please check vignettes.
#'
#'   The easiest way to look for usage is using `help` function:
#'   help('verbatimTextOutput'), or ??verbatimTextOutput
#'
#'
#' @param title is the title for output
#'
#' @param width an integer from 1 to 12, defines the percentage of output width
#'   12 means 100% width, 6 means 50% and 4 means 33% width.
#'
#' @param order numeric order of outputs. Outputs will be re-ordered by this argument
#'
define_output(
  definition = plotOutput('mds_plot'),
  title = 'MDS Diagnosis',
  width = 4,
  order = 2
)


define_output(
  definition = plotOutput('cluster_plot'),
  title = 'Cluster Visualization',
  width = 8,
  order = 1
)

define_output(
  definition = customizedUI('cluster_membership_table'),
  title = 'Clustering Membership',
  width = 5L,
  order = 2.1
)

define_output(
  definition = plotOutput('dendrogram_plot'),
  title = 'Dendrogram',
  width = 7L,
  order = 2.15
)

define_output(
  definition = plotOutput('optimal_cluster_number_plot'),
  title = 'Optimal number of clusters',
  width = 12L,
  order = 2.16
)

# if (input_method == 'H-Clust' && !is.null(local_data$my_resultsmds_res)) {
#   plot()
# }
# define_output(
#   definition = plotOutput('cluster_plot1'),
#   title = 'Cluster Visualization',
#   width = 9,
#   order = 3)

define_output_3d_viewer(outputId = 'viewer_3d', title = '3D Cluster', order = -999, height = '500px')


# <<<<<<<<<<<< End ----------------- [DO NOT EDIT THIS LINE] -------------------

# -------------------------------- View layout ---------------------------------

# Preview

view_layout('builtin_electrode_clustering', theme = 'purple')


