input %?<-% getDefaultReactiveInput()
output %?<-% getDefaultReactiveOutput()
session %?<-% getDefaultReactiveDomain()


local_data %?<-% reactiveValues(
  analysis_data_raw = NULL
)

RAVE_ROI_KEY %?<-% 'VAR_IS_ROI_'
model_params %?<-% dipsaus::fastmap2()

minkowski_p_ui <- function(){
  if(isTRUE(input$distance_method == "minkowski")){
    p("asdasdada")
  } else {
    ""
  }
}

visnet_wrapper <- function(){
  session <- shiny::getDefaultReactiveDomain()
  visNetwork::visNetworkOutput(session$ns('visnet'), height = "80vh")
}
output$visnet <- visNetwork::renderVisNetwork({
  visnet()
})


shiny::bindEvent(
  shiny::observe({
    
    try({
      fast_results <- raveclusters::ravecluster(
        names = c("settings", "mds_result", "collapsed_array", 
                  "indata_analysis", "cluster_method", "dis",
                  "distance_method"),
        input_groups = input$input_groups,
        roi_options = list(
          variable = input$model_roi_variable,
          values = input$filter_by_roi,
          roi_ignore_gyrus_sulcus = input$roi_ignore_gyrus_sulcus,
          roi_ignore_hemisphere = input$roi_ignore_hemisphere
        ),
        distance_method = input$distance_method,
        cluster_method = input$input_method,
        use_baseline = input$check_scale,
        baseline_time = input$baseline_time,
        analysis_time_window = input$time_window,
        power_unit = input$power_unit,
        analysis_event = input$epoch_event,
        mds_distance = input$mds_distance_method
      )
      
      local_data$fast_results <- fast_results
    }, silent = TRUE)
  }),
  input$baseline_time,
  input$check_scale,
  input$time_window,
  input$power_unit,
  input$epoch_event,
  input$mds_distance_method,
  ignoreNULL = TRUE, ignoreInit = TRUE
)
