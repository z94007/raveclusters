# 
# 
# observe({
#   dipsaus::cat2('main observe', level='INFO')
#   
#   raw = local_data$analysis_data_raw
#   if( !is.list(raw) ){
#     local_data$analysis_data_filtered = NULL
#     return()
#   }
#   
#   # raw = list(data = local_data$analysis_data_raw)
#   local_data$analysis_data_filtered = raw$data
#   
#   conditions = unique(raw$data$Condition); if(!length(conditions)){ conditions = '' }
#   time_range = range(raw$data$Time, na.rm = TRUE)
#   analysis_window = time_range
#   confs = dipsaus::drop_nulls(raw$confs)
#   groups = list()
#   if(length(confs)){
#     confs = confs[[1]]
#     groups = confs$GROUPS
#     analysis_window = sort(c(confs$ANALYSIS_WINDOW, time_range)[1:2])
#   }
#   
#   # store this in local_data so that we have everything in one place
#   local_data$analysis_window = analysis_window
#   
#   dipsaus::updateCompoundInput2(session, 'cond_group', ncomp = max(length(groups), 1), 
#                                 initialization = list(
#                                   group_conditions = list( choices = conditions )
#                                 ), value = groups)
#   
#   updateSliderInput(session, 'analysis_window', min = time_range[[1]], 
#                     max=time_range[[2]], value=analysis_window)
#   
#   local_data$omnibus_plots_time_range = time_range
#   updateSliderInput(session, 'omnibus_plots_time_range', min = time_range[[1]], 
#                     max=time_range[[2]], value=time_range)
#   
#   nms = names(local_data$analysis_data_filtered)
#   
#   usual_dvs = c(format_unit_of_analysis_name(get_unit_of_analysis(names=TRUE)), 'Power')
#   dnu = c('Time', 'uuid', 'Project', 'TrialIsOutlier')
#   
#   # DV
#   dvs = nms[
#     sapply(nms, function(x) any(startsWith(x, usual_dvs)))
#     ]
#   # IVs
#   local_data$number_of_subjects = length(unique(local_data$analysis_data_filtered$Subject))
#   
#   # Random effects
#   re_sel = c('Electrode')
#   if(isTRUE(local_data$number_of_subjects > 1)) {
#     re_sel %<>% c('Subject')
#   }
#   
#   # determine all possible effects
#   # here we need to check the number of groups that area assigned (> 1 means create a factor) 
#   dipsaus::updateCompoundInput2(session, 'multi_window_analysis', 
#                                 initialization = list(
#                                   analysis_window = list(min = time_range[1], max = time_range[2], value = time_range),
#                                   window_is_active = list(value=FALSE)
#                                 ))
#   
#   # grab any ROI variables
#   var_roi_avail <- nms[startsWith(nms, RAVE_ROI_KEY)]
#   var_fe_avail <- nms[! nms %in% c(dvs, dnu, var_roi_avail)]
#   
#   var_roi_avail <- stringr::str_remove_all(var_roi_avail, RAVE_ROI_KEY)
#   
#   current_roi_selected = model_params$roi_variable
#   if (!isTRUE(current_roi_selected %in% var_roi_avail)) {
#     print(paste('ROIs available: ', paste0(var_roi_avail, collapse='|')))
#     current_roi_selected = character(0)
#     updateSelectInput(session, 'model_roi_variable', choices = var_roi_avail)
#   }
#   
#   local_data$var_fixed_effects_available = var_fe_avail
#   
#   updateSelectInput(session, 'model_random_effects', choices = var_fe_avail, selected = re_sel)
#   updateSelectInput(session, 'model_fixed_effects', choices = var_fe_avail)
#   updateSelectInput(session, 'model_dependent', selected = dvs[1], choices = dvs)
# })

build_roi_levels <- function() {
  if(!isTRUE(length(local_data$analysis_data_filtered)>1)) {
    return(character(0))
  }
  
  mrv = input$model_roi_variable
  adf = local_data$analysis_data_filtered
  
  if(nchar(mrv) > 0) {
    lvls = levels(factor(adf[[RAVE_ROI_KEY %&% mrv]]))
    
    if(isTRUE(input$roi_ignore_hemisphere)) {
      lvls <- ravebuiltins:::remove_hemisphere_labels(lvls)
      # lvls %<>% remove_hemisphere_labels
    }
    
    if(isTRUE(input$roi_ignore_gyrus_sulcus)) {
      lvls <- ravebuiltins:::remove_gyrus_sulcus_labels(lvls)
      # lvls %<>% remove_gyrus_sulcus_labels
    }
  }
  
  return(lvls)
}

observeEvent(input$model_roi_variable, {
  if(length(local_data$analysis_data_filtered)<1) return()
  
  mrv = input$model_roi_variable
  if(nchar(input$model_roi_variable) > 0) {
    lvls = build_roi_levels() #levels(factor(local_data$ analysis_data_filtered[[RAVE_ROI_KEY %&% mrv]]))
    updateSelectInput(session, 'filter_by_roi', choices = lvls, selected=lvls)
  }
  model_params$roi_variable = mrv
  model_params$roi_filter = lvls
})

observeEvent(input$filter_by_roi, {
  model_params$roi_filter <- input$filter_by_roi
})

observeEvent(input$how_to_model_roi, {
  model_params$how_to_model_roi = input$how_to_model_roi
})


observeEvent(input$roi_ignore_hemisphere, {
  sel = input$filter_by_roi
  # sel = ..model_params$roi_filter
  
  if(isTRUE(input$roi_ignore_hemisphere)) {
    # we are currently NOT ignoring HEMI, but we'll start now
    sel <- unique(ravebuiltins:::remove_hemisphere_labels(sel))
    
    updateSelectInput(session, 'filter_by_roi', choices=build_roi_levels(), selected = sel)
  } else {
    # this means we are currently ignoring HEMI, but we shall do so no longer
    new_lvls = build_roi_levels()
    
    sel = new_lvls[unlist(sapply(sel, function(s) which(endsWith(new_lvls, s))))]
    
    updateSelectInput(session, 'filter_by_roi', choices=build_roi_levels(), selected = sel)
  }
})

observeEvent(input$roi_ignore_gyrus_sulcus, {
  sel = input$filter_by_roi
  # sel = ..model_params$roi_filter
  
  if(isTRUE(input$roi_ignore_gyrus_sulcus)) {
    # we are currently NOT ignoring G/S, but we'll start now
    sel <- unique(ravebuiltins:::remove_gyrus_sulcus_labels(sel))
    updateSelectInput(session, 'filter_by_roi',
                      choices=build_roi_levels(), selected = sel)
  } else {
    # this means we are currently ignoring G/S, but we shall do so no longer
    new_lvls = build_roi_levels()
    
    if(isTRUE(input$roi_ignore_hemisphere)) {
      # if we're ignoring HEMI right now, then we don't need to do the additional check for startsWith
      sel = new_lvls[unlist(sapply(sel, function(s) which(endsWith(new_lvls, s))))]
    } else {
      sel <- new_lvls[unlist(sapply(sel, function(s) {
        which(startsWith(new_lvls, substr(s, 1,2)) & 
                endsWith(new_lvls, ravebuiltins:::remove_hemisphere_labels(s)))
      }))]
    }
    
    updateSelectInput(session, 'filter_by_roi', choices=build_roi_levels(), selected = sel)
  }
})