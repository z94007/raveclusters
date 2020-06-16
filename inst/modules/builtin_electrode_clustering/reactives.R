input = getDefaultReactiveInput()
output = getDefaultReactiveOutput()
session = getDefaultReactiveDomain()


local_data %?<-% reactiveValues(
  analysis_data_raw = NULL
)

observe({
  dipsaus::cat2('main observe', level='INFO')
  
  raw = local_data$analysis_data_raw
  
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
  
  dipsaus::updateCompoundInput2(session, 'cond_group', ncomp = max(length(groups), 1), 
                                initialization = list(
                                  group_conditions = list( choices = conditions )
                                ), value = groups)
  
  updateSliderInput(session, 'analysis_window', min = time_range[[1]], 
                    max=time_range[[2]], value=analysis_window)
  
  local_data$omnibus_plots_time_range = time_range
  updateSliderInput(session, 'omnibus_plots_time_range', min = time_range[[1]], 
                    max=time_range[[2]], value=time_range)
  
  nms = names(local_data$analysis_data_filtered)
  
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
    re_sel %<>% c('Subject')
  }
  
  # determine all possible effects
  # here we need to check the number of groups that area assigned (> 1 means create a factor) 
  dipsaus::updateCompoundInput2(session, 'multi_window_analysis', 
                                initialization = list(
                                  analysis_window = list(min = time_range[1], max = time_range[2], value = time_range),
                                  window_is_active = list(value=FALSE)
                                ))
  var_fe_avail <- nms[! nms %in% c(dvs, dnu)]
  local_data$var_fixed_effects_available = var_fe_avail
  updateSelectInput(session, 'model_random_effects', choices = var_fe_avail, selected = re_sel)
  updateSelectInput(session, 'model_fixed_effects', choices = var_fe_avail)
  updateSelectInput(session, 'model_dependent', selected = dvs[1], choices = dvs)
  
})