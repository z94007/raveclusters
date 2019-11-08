input = getDefaultReactiveInput()
output = getDefaultReactiveOutput()
session = getDefaultReactiveDomain()


local_data %?<-% reactiveValues(
    # Full data has two parts: local_data$analysis_data_raw, and local_data$additional_data
    # together makes analysis_data
    analysis_data_raw = NULL,
    additional_data = NULL,
    analysis_data = NULL,
    collapsed_data = NULL,
    analysis_window = 0:1,
    
    potential_analysis = list(),
    analysis_name = NULL,
    sample_table = NULL,
    var_dependent = NULL,
    var_fixed_effects = NULL,
    lmer_results = NULL,
    lmer_results_summary = NULL,
    
    
    electrode_cluster = NULL
)

local_filters = reactiveValues(
    filter_count = 0,
    filter_observers = 0
)

cond_group_ui = function(){
    rave::compoundInput(
        inputId = ns('cond_group'), prefix= 'Condition Group', inital_ncomp = 1, components = {
            textInput('group_name', 'Name', value = '', placeholder = 'Condition Name')
            selectInput('group_conditions', ' ', choices = '', multiple = TRUE, selected = character(0))
        }, max_ncomp = 10)
}


# Sync all group_names
# lapply(1:0, function(ii){
#     name_id = paste0('cond_group_group_name_', ii)
#     .env = environment()
#     observeEvent(input[[name_id]], {
#         val = val_raw = input[[name_id]]
#         if(length(val)){
#             if( stringr::str_detect(val, '^CondGroup[0-9]*') || 
#                 val %in% names(local_data$analysis_data_raw$headers) ){
#                 # Invalid group name, reset to default
#                 val = sprintf('CondGroup%d', ii)
#             }
#             if( val != val_raw ){
#                 updateTextInput(session, name_id, value = val)
#             }
#         }
#     }, event.env = .env, handler.env = .env)
# })

observe({
    raw = local_data$analysis_data_raw
    
    if( !is.list(raw) ){
        local_data$analysis_data_filtered = NULL
        return()
    }
    local_data$analysis_data_filtered = raw$data
    
    conditions = unique(raw$data$Condition); if(!length(conditions)){ conditions = '' }
    time_range = range(raw$data$Time, na.rm = TRUE)
    analysis_window = time_range
    confs = rave::dropNulls(raw$confs)
    groups = list()
    if(length(confs)){
        confs = confs[[1]]
        groups = confs$GROUPS
        analysis_window = sort(c(confs$ANALYSIS_WINDOW, time_range)[1:2])
    }
    
    # store this in local_data so that we have everything in one place
    local_data$analysis_window = analysis_window
    
    rave::updateCompoundInput(session, 'cond_group', to = length(groups))
    # Update cond_group 
    lapply(seq_len(20), function(ii){
        g = list(group_name = '', group_conditions = character(0))
        if( length(groups) >= ii ){
            g = groups[[ii]]
        }
        updateSelectInput(session, inputId = sprintf('%s_%d', 'cond_group_group_conditions', ii),
                          choices = conditions, selected = g$group_conditions)
        updateTextInput(session, inputId = sprintf('%s_%d', 'cond_group_group_name', ii),
                        value = g$group_name)
    })
    
    updateSliderInput(session, 'analysis_window', min = time_range[[1]], 
                      max=time_range[[2]], value=analysis_window)
})

    

# Get additional data
observe({
    cond_groups = lapply(1:20, function(jj){input[[paste0('cond_group_group_conditions_', jj)]]})
    cond_groups = rave::dropNulls(cond_groups)
    conditions = NULL
    
    if( is.list(local_data$analysis_data_raw) ){
        conditions = local_data$analysis_data_raw$data$Condition
    }
    
    if(length(cond_groups) && length(conditions)){
        
        cols = lapply(cond_groups, function(conds){
            as.numeric(conditions %in% unlist(conds))
        })
        gnames = lapply(seq_along(cols), function(jj){input[[paste0('cond_group_group_name_', jj)]]})
        names(cols) = gnames
        local_data$additional_data = do.call('data.frame', cols)
    }else{
        local_data$additional_data = NULL
    }
    
})


observeEvent(input$run_analysis, {
    ### put execution code here
    
    
    electrode_cluster = NULL
    
    
    
    assign('..local_data',
           value = shiny::isolate(shiny:::reactiveValuesToList(local_data)), envir = globalenv())
    
})


observeEvent(input$lmer_yaml_load, {
    fdata = input$lmer_yaml_load
    if(!is.list(fdata) || !length(fdata$files)){ return() }
    assign('fdata', fdata, envir = globalenv())
    f_name = unlist(fdata$files); names(f_name) = NULL
    
    read_source = list(
        'Power Explorer Analysis' = 'power_explorer',
        'LME Analysis' = 'group_analysis_lme'
    )
    
    f_name = c(subject$dirs$subject_dir, '..', '_project_data', read_source[[fdata$root]], f_name)
    f_path = do.call(file.path, as.list(f_name))
    print(f_path)
    conf = yaml::read_yaml(f_path)
    print(conf)
    
    # updateCheckboxInput(session, inputId = 'auto_calculate', value = FALSE)
    # lapply(1:10, function(ii){
    #   gc_id = sprintf('GROUPS_group_conditions_%d', ii)
    #   gc = conf[[gc_id]]
    #   if(!length(gc)){ gc = character(0) }
    #   print(paste(ii, c(gc)))
    #   updateSelectInput(session, gc_id, selected = gc)
    # })
})