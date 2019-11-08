# File defining module inputs, outputs

# ----------------------------------- Debug ------------------------------------
require(raveclusters)

env = dev_raveclusters(T)

#' Load subject for debugging
#' Make sure this is executed before developing the module to make sure
#' at least one subject is loaded
mount_demo_subject()


# >>>>>>>>>>>> Start ------------- [DO NOT EDIT THIS LINE] ---------------------


#  ----------------------  Initializing Global variables -----------------------

load_scripts(
  get_path('inst/modules/builtin_electrode_clustering/reactives.R'),
  get_path('inst/modules/builtin_electrode_clustering/outputs.R'),
  get_path('inst/modules/builtin_electrode_clustering/Untitled.R'),
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
  definition = textInput(inputId = 'text_electrode', label = 'Electrode Not loaded'),

  init_args = c('label', 'value'),
  init_expr = {

    # check ?rave_prepare for what kind of data are loaded
    loaded_electrodes = preload_info$electrodes

    # Generate text for loaded electrodes
    text = deparse_selections(loaded_electrodes)
    
    previous_val = parse_selections(cache_input('text_electrode', ''))
    if( any(previous_val %in% loaded_electrodes) ){
      previous_val = previous_val[previous_val %in% loaded_electrodes]
      value = deparse_selections(previous_val)
    }else{
      value = text
    }

    # Update
    label = paste0('Electrode (', text, ')')
  }
)
#define_input(definition = numericInput(inputId = 'nclusters', label = 'the number of clusters'))


define_input_condition_groups('input_groups', label = 'Condition Group')

define_input(
  definition = numericInput(inputId = 'input_nclusters', label = 'Number of Clusters', 
                            value = 1, min = 1, max = 10, step = 1),
  init_args = c("value", 'max'),
  init_expr = {
    max = length(preload_info$electrodes)
    value = cache_input('input_nclusters', max(min(ceiling(max / 3), 6), 2))
  }
)

define_input(
  definition = selectInput(inputId = 'input_method', label = 'Clustering Method',
                           choices = c('H-Clust', 'K-Means'), selected = NULL),
  init_args = c('selected'),
  init_expr = {
    selected = cache_input('input_method', 'H-Clust')
  }
)

define_input(
  definition = selectInput(inputId = 'distance_method', label = 'Distance Measurement',
                           choices = c('Euclidean', 'Maximum',"Manhattan", "Canberra", "Binary" , "Minkowski"), selected = NULL),
  init_args = c('selected'),
  init_expr = {
    selected = cache_input('dsitance_method', 'Manhattan')
  }
)
define_input(
  definition = shiny::actionButton('do_run', 'Run Analysis')
)

input_layout = list(
  'Data Import' = list(
    'analysis_data'
  ),
  #[#99ccff][-]
  'Trial Selector' = list(
    'text_electrode',
    'input_groups'
  ),
  '[#ccff99]Analysis Settings' = list(
    'input_frequencies',
    'input_baseline_range',
    c( 'input_method', 'input_nclusters' ),'distance_method'
  ),
  'Model Running' = list(
    'do_run'
  )
)

manual_inputs = c('analysis_data', 'input_baseline_range', 'text_electrode', 'input_frequencies', 'input_groups', 'input_nclusters',
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
  definition = plotOutput('tsne_plot'),
  title = 'MDS Diagnosis',
  width = 3,
  order = 2
)

define_output(
  definition = plotOutput('cluster_plot'),
  title = 'Cluster Visualization',
  width = 9,
  order = 1
)

define_output_3d_viewer(outputId = 'viewer_3d', title = '3D Cluster', order = 999, height = '500px')


# <<<<<<<<<<<< End ----------------- [DO NOT EDIT THIS LINE] -------------------

# -------------------------------- View layout ---------------------------------

# Preview

view_layout('builtin_electrode_clustering', sidebar_width = 3, launch.browser = T)

