# This file contains reactive events for inputs


input %?<-% getDefaultReactiveInput()
output %?<-% getDefaultReactiveOutput()
session %?<-% getDefaultReactiveDomain()

local_data %?<-% reactiveValues(
  source_data = NULL,
  analysis_data_raw = NULL,
  analysis_window = NULL,
  number_of_subjects = NULL
)

base::print(session$ns(NULL))

# Load data when `Load selected data` button is pressed
shiny::bindEvent(
  observe({
    project_name <- subject$project_name
    source_files <- input$data_import_selector
    local_data$source_data <- NULL
    
    dipsaus::shiny_alert2(title = "Loading in progress...",
                          text = "Please wait",
                          icon = "info", auto_close = FALSE,
                          buttons = FALSE)
    on.exit({
      dipsaus::close_alert2()
    })
    
    # run pipeline
    source_data <- raveclusters::ravecluster(
      names = "source_data",
      
      # set variables
      project_name = project_name,
      source_files = source_files
    )
    local_data$source_data <- source_data
  }),
  input$data_import_btn,
  ignoreNULL = TRUE, ignoreInit = TRUE
)

# Update inputs when local_data$source_data is changed (after data_import_btn being pressed and data loaded)
shiny::bindEvent(
  observe({
    # # DEBUG: start
    # source_data <- ravecluster(names = "source_data")
    # # DEBUG: end
    
    source_data <- local_data$source_data
    
    # Not sufficient but should be ok: if the source data is invalid, do not change UI
    if(!is.list(source_data) || !length(source_data)){ return() }
    
    # Find all possible trial conditions
    conditions <- unique(source_data$data$Condition)
    if(!length(conditions)) { conditions <- "" }
    conditions <- sort(conditions)
    
    # Obtain the widest time range
    time_range <- range(source_data$data$Time, na.rm = TRUE)
    
    # set default analysis time range to be identical to time_range
    # in case `confs` are all invalid
    analysis_window <- time_range
    
    # Find table column names that represent power
    nms = names(source_data$data)
    
    usual_dvs = c(format_unit_of_analysis_name(get_unit_of_analysis(names=TRUE)), 'Power')
    # usual_ivs = c('Electrode', 'Trial', 'Condition', 'Subject')
    # usual_ivs = usual_ivs[usual_ivs %in% nms]
    
    dvs = nms[
      sapply(nms, function(x) any(startsWith(x, usual_dvs)))
    ]
    power_unit <- sapply(usual_dvs, function(nm) {
      any(startsWith(dvs, nm))
    })
    power_unit <- usual_dvs[power_unit]
    epoch_event <- gsub(sprintf("^(%s)[_]{0,1}", paste(usual_dvs, collapse = "|")), "", dvs)
    
    # grab ROI variables
    dnu = c('Time', 'uuid', 'Project', 'TrialIsOutlier')
    var_roi_avail <- nms[startsWith(nms, RAVE_ROI_KEY)]
    var_fe_avail <- nms[! nms %in% c(dvs, dnu, var_roi_avail)]
    var_roi_avail <- stringr::str_remove_all(var_roi_avail, RAVE_ROI_KEY)
    
    # Load yaml configurations 
    confs <- dipsaus::drop_nulls(source_data$confs)
    groups <- list()
    if(length(confs)) {
      groups <- confs[[1]]$GROUPS
      
      # try to get analysis window from power explorer
      analysis_window <- confs[[1]]$analysis_window
      if(length(analysis_window) != 2) {
        analysis_window <- time_range
      } else {
        analysis_window <- sort(analysis_window)
        if(analysis_window[[1]] < time_range[[1]]){
          analysis_window[[1]] <- time_range[[1]]
        }
        if(analysis_window[[2]] > time_range[[2]]){
          analysis_window[[2]] <- time_range[[2]]
        }
      }
    }
    
    # Set to `local_data`
    # store the following variables in local_data so that we have everything in one place
    local_data$analysis_window <- analysis_window
    
    ## Update UI
    # `input_groups` (condition group)
    dipsaus::updateCompoundInput2(
      session = session, inputId = 'input_groups', ncomp = max(length(groups), 1), 
      initialization = list(group_conditions = list( choices = conditions )), 
      value = groups)
    
    # `time_window` (analysis time window)
    shiny::updateSliderInput(
      session = session, inputId = 'time_window', min = time_range[[1]], 
      max = time_range[[2]], value = analysis_window)
    
    # `plot_time_window` (plot range)
    shiny::updateSliderInput(
      session = session, inputId = 'plot_time_window', min = time_range[[1]], 
      max = time_range[[2]], value = time_range)
    
    # update response variable -- choices = dvs, with first option selected 
    # updateSelectInput(session, 'trial_selected', choices = dvs, selected = dvs)
    updateSelectInput(session, 'power_unit', choices = power_unit, 
                      selected = shiny::isolate(input$power_unit) %OF% power_unit)
    updateSelectInput(session, 'epoch_event', choices = epoch_event, 
                      selected = epoch_event)
    
    # Update ROI variable
    updateSelectInput(session, 'model_roi_variable', choices = var_roi_avail,
                      selected = input$model_roi_variable %OF% var_roi_avail)
    
  }),
  local_data$source_data,
  ignoreNULL = TRUE, ignoreInit = TRUE
)

# Reactives for ROI variables
shiny::bindEvent(
  observe({
    mrv = input$model_roi_variable
    if(!length(mrv)) { return() }
    
    if(!nchar(mrv)) { return() }
    
    source_data <- local_data$source_data
    if(!is.list(source_data) || !is.data.frame(source_data$data)) { return() }
    
    lvls <- raveclusters::build_roi_levels(
      data_table = source_data$data,
      model_roi_variable = mrv,
      ignore_hemisphere = isTRUE(input$roi_ignore_hemisphere),
      ignore_gyrus_sulcus = isTRUE(input$roi_ignore_gyrus_sulcus)
    )
    updateSelectInput(session, 'filter_by_roi', choices = lvls, selected = lvls)
  }),
  input$model_roi_variable,
  local_data$source_data,
  input$roi_ignore_hemisphere,
  input$roi_ignore_gyrus_sulcus,
  ignoreNULL = TRUE, ignoreInit = TRUE
)


