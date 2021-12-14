input = getDefaultReactiveInput()
output = getDefaultReactiveOutput()
session = getDefaultReactiveDomain()


local_data %?<-% reactiveValues(
  analysis_data_raw = NULL
)

RAVE_ROI_KEY = 'VAR_IS_ROI_'
model_params = dipsaus::fastmap2()

minkowski_p_ui <- function(){
  if(isTRUE(input$distance_method == "minkowski")){
    p("asdasdada")
  } else {
    ""
  }
}

observe({
  dipsaus::cat2('main observe', level='INFO')
  
  raw = local_data$analysis_data_raw
  
  assign('aaa', raw, globalenv())
  
  if( !is.list(raw) ){
    local_data$analysis_data_filtered = NULL
    return()
  }
  
  # raw = list(data = local_data$analysis_data_raw)
  local_data$analysis_data_filtered = raw$data
  
  conditions = unique(raw$data$Condition); if(!length(conditions)){ conditions = '' }
  time_range = range(raw$data$Time, na.rm = TRUE)
  analysis_window = time_range
  confs = dipsaus::drop_nulls(raw$confs)
  groups = list()
  if(length(confs)){
    confs = confs[[1]]
    groups = confs$GROUPS
    analysis_window = sort(c(confs$ANALYSIS_WINDOW, time_range)[1:2])
  }
  
  # store this in local_data so that we have everything in one place
  local_data$analysis_window = analysis_window
  
  dipsaus::updateCompoundInput2(session, 'input_groups', ncomp = max(length(groups), 1), 
                                initialization = list(
                                  group_conditions = list( choices = conditions )
                                ), value = groups)
  
  updateSliderInput(session, 'time_window', min = time_range[[1]], 
                    max=time_range[[2]], value=c(0,1.5))
  
  updateSliderInput(session, 'plot_time_window', min = time_range[[1]], 
                    max=time_range[[2]], value=analysis_window)
  
  nms = names(raw$data)
  
  usual_dvs = c(format_unit_of_analysis_name(get_unit_of_analysis(names=TRUE)), 'Power')
  dnu = c('Time', 'uuid', 'Project', 'TrialIsOutlier')
  # usual_ivs = c('Electrode', 'Trial', 'Condition', 'Subject')
  # usual_ivs = usual_ivs[usual_ivs %in% nms]
  
  # DV
  dvs = nms[
    sapply(nms, function(x) any(startsWith(x, usual_dvs)))
    ]
  # IVs
  local_data$number_of_subjects = length(unique(local_data$analysis_data_filtered$Subject))
  re_sel = c('Electrode')
  if(isTRUE(local_data$number_of_subjects > 1)) {
    re_sel <- c(re_sel, 'Subject')
  }
  
  # update response variable -- choices = dvs, with first option selected 
  updateSelectInput(session, 'trial_selected',choices =dvs)
  
  
  
  # Handle ROI
  # grab any ROI variables
  var_roi_avail <- nms[startsWith(nms, RAVE_ROI_KEY)]
  var_fe_avail <- nms[! nms %in% c(dvs, dnu, var_roi_avail)]
  
  var_roi_avail <- stringr::str_remove_all(var_roi_avail, RAVE_ROI_KEY)
  
  current_roi_selected = model_params$roi_variable
  if (!isTRUE(current_roi_selected %in% var_roi_avail)) {
    print(paste('ROIs available: ', paste0(var_roi_avail, collapse='|')))
    current_roi_selected = character(0)
    updateSelectInput(session, 'model_roi_variable', choices = var_roi_avail)
  }
  
  local_data$var_fixed_effects_available = var_fe_avail
  
  
})