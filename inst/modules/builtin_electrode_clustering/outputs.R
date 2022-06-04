get_pipeline_results <- function(fast_ok = FALSE) {
  
  if(!fast_ok) {
    return(local_data$results)
  }
  results <- local_data$results
  fast_results <- local_data$fast_results
  if(!is.list(results) || !length(results)) {
    return(fast_results)
  }
  
  # compare
  # check inputs
  input_keys <- c("use_baseline", "baseline_time", "analysis_time_window", "power_unit", "analysis_event", "mds_distance")
  
  if(identical(
    fast_results$settings[input_keys],
    results$settings[input_keys]
  )) {
    return(results)
  } else {
    return(fast_results)
  }
  
}


fix_font_color_button <- function (outputId, label = "Download", class = NULL, ...)  {
  aTag <- tags$a(id = outputId, class = paste("btn shiny-download-link", 
                                              class), href = "", target = "_blank", download = NA, 
                 icon("download"), label, ...)
}


graph_export = function(){
  tagList(
    # actionLink(ns('btn_graph_export'), 'Export Graphs'),
    fix_font_color_button(ns('export_pdf'), 'Export hi-res PDF', icon=shiny::icon('download'),
                          class = 'btn btn-default text-white', width = '100%')
    # actionLink(ns('btn_graph_export'), 'Export Graphs'),
    
  )
}

#TODO add json
output$export_pdf <- downloadHandler(
  filename = function(){
    paste0(
      paste(unique(local_data$analysis_data_raw$subject), collapse = "_"),
      '_', input$input_nclusters,'_',input$input_method,'_',
      format(Sys.time(), "%b_%d_%Y_%H_%M_%S"), '.pdf')
  },
  content = function(file){
    pdf(file, width = 12, height = 7)
    cluster_plot(separate = TRUE, cex.main = shiny_cex.main)
    mds_plot()
    dendrogram_plot()
    #optimal_cluster_number_plot()
    dev.off()
    },

  contentType = "application/pdf"


)




visnet <- function(){
  
  results <- local_data$results

  #FIXME
  shiny::validate(
    shiny::need(is.list(results) &&
                  inherits(results$cluster_tree_plot, "visNetwork"),
                message = 'Please press "Run Analysis" button.')
  )
  
  results$cluster_tree_plot
  # visNetwork(node_df, edge_df, width = "100%", height = "100vh") %>% 
  #   visNodes(shapeProperties = list(useBorderWithImage = FALSE), size = 64) %>%
  #   visEdges(arrows = "to") %>% 
  #   visHierarchicalLayout(direction = "LR", nodeSpacing = 150, treeSpacing = 400,
  #                         levelSeparation = 200, 
  #                         sortMethod = "directed") %>% 
  #   visInteraction(dragNodes = FALSE, zoomSpeed = 0.1) %>% 
  #   visConfigure(enabled = FALSE)
  # 
}


mds_plot <- function(){
  # rave::rave_context()
  results <- local_data$results
  
  shiny::validate(
    shiny::need(
      is.list(results) && is.data.frame(results$mds_result), 
      message = 'MDS plot does not exist. Try to adjust (narrow) your analysis time window')
  )
  
  raveclusters::cluster_mds_plot(results)
  # # check
  # if(!is.list(results)) {
  #   raveclusters::cluster_mds_plot(fast_results)
  # } else {
  #   # check inputs
  #   input_keys <- c("use_baseline", "baseline_time", "analysis_time_window", "power_unit", "analysis_event", "mds_distance")
  #   
  #   if(identical(
  #     fast_results$settings[input_keys],
  #     results$settings[input_keys]
  #   )) {
  #     raveclusters::cluster_mds_plot(results)
  #   } else {
  #     raveclusters::cluster_mds_plot(fast_results)
  #   }
  # }
  

}



cluster_membership_table <- function(){
  
  tbl <- raveclusters::cluster_membership(local_data$results$cluster_table)
  if(!is.data.frame(tbl)) {
    return(div(
      style = 'height: 380px',
    ))
  }
  div(
    style = 'overflow-x:scroll; height: 380px',
    HTML(knitr::kable(tbl, format = 'html', row.names = FALSE,
                      table.attr = 'class="table shiny-table table-striped"')) # spacing-xs
  )
  
}


dendrogram_plot <- function() {
  results <- local_data$results
  
  shiny::validate(shiny::need(is.list(results) && length(results) > 1,
                              message = 'Please press "Run Analysis"'))
  shiny::validate(shiny::need(
    identical(results$cluster_method, 'H-Clust'),
    message = 'Only available for method = H-clust'
  )) 
  
  raveclusters::cluster_dendrogram(results, style_legend = "all", main = "")
  
}

optimal_cluster_number_plot <- function(){
  results <- local_data$results
  
  shiny::validate(
    shiny::need(is.matrix(results$indata_analysis), 
                message = 'Please adjust (narrow) the analysis time window')
  )
  
  
  clustfun <- switch(
    results$cluster_method,
    `K-Medois` = cluster::pam,
    {
      factoextra::hcut
    }
  )
  
  indata_analysis <- results$indata_analysis
  rownames(indata_analysis) <- NULL
  dis <- as.matrix(results$dis)
  
  nobs <- nrow(indata_analysis)
  if(nobs > 60) {
    cv_fold <- 1
  } else if(nobs > 30) {
    cv_fold <- 5
  } else {
    cv_fold <- 10
  }
  
  par(mfrow = c(1, 2))
  
  
  cluster_idx_plot(
    indata_analysis, dis = dis, clustfun = clustfun,
    method = "silhouette", col = "dodgerblue3", cv_fold = cv_fold, 
    n_highlights = 3, max_clusters = 8
  ) 
  cluster_idx_plot(
    indata_analysis, dis = dis, clustfun = clustfun,
    method = "wss", col = "dodgerblue3", cv_fold = cv_fold, 
    n_highlights = 0, max_clusters = 8
  ) 
  
  
}


cluster_plot <-  function(){
  
  # results <- ravecluster()
  results <- local_data$results
  
  shiny::validate(shiny::need(is.list(results) && !is.null(results$mse), 
                              message = 'Please press "Run Analysis" button'))
  
  
  mse <- results$mse
  mse[is.na(mse)] <- 0
  yrange <- quantile(c(mse[,1,,] - mse[,2,,], mse[,1,,] + mse[,2,,]), c(0.001, 0.999))
  raveclusters::cluster_visualization(
    results, color_scheme = "Beautiful Field",
    cex = 1, plot_range = input$plot_time_window, yrange = yrange)
  
}
  

viewer_3d_fun <- function(...){
  # brain = rave::rave_brain2('congruency/YAB')
  results <- local_data$results
  
  shiny::validate(
    shiny::need(is.data.frame(results$cluster_table), 
                message = 'Please import data and run analysis')
  )
  
  widget <- raveclusters::cluster_on_brain(results)
  
  shiny::validate(
    shiny::need(
      inherits(widget, "threejs_brain"), 
      message = local({
        if(!is.character(widget)) {
          widget <- "Cannot generate 3D viewer"
        }
        widget
      }))
  )
  
  widget
}


