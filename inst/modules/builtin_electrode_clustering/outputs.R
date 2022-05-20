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
  
  res <- local_data$my_results

  #FIXME
  shiny::validate(
    shiny::need(is.list(res) &&
                  is.matrix(res$indata) && 
                  inherits(res$dis, "dist"),
                message = 'Please press "Run Analysis" button.')
  )
  
  raveio::catgl("Rendering [visnet]")
  
  # function(indata, dist, ...){
  #   
  # }
  # get the height of the tree plot
  k.max = 8
  height <- min(k.max,dim(res$indata)[1])
  
  # the clustering results of different cluster number
  v <- array(0, c( height,dim(res$indata)[1] ))
  
  for (i in 1:height) {
    clust <- factoextra::hcut(res$dis, i , isdiss = TRUE)
    v[i,] <- clust$cluster
  }
  
  # initialize the plot
  last_layer_id <- NULL
  node_ids <- NULL
  node_levels <- NULL
  node_pos <- NULL
  edges <- list() # from, to, size
  
  for(layer in 1:height){
    
    
    if(layer == 1){
      layer_ids <- 1
    } else {
      layer_ids <- max(node_ids) + 1:layer
    }
    node_ids <- c(node_ids, layer_ids)
    node_levels <- c(node_levels, rep(layer, length(layer_ids)))
    # node_pos <- c(node_pos, 1:layer + height / 2 - layer / 2 - 0.5)
    node_pos <- c(node_pos, 1:layer)
    
    if(layer > 1){
      
      layer_table <- t(v[c(layer-1, layer),])
      
      for(from_node in 1:(layer - 1)){
        
        for(to_node in 1:layer){
          
          edge_size <- sum(layer_table[,1] == from_node & layer_table[,2] == to_node)
          
          if(edge_size > 0){
            
            from_node_id <- last_layer_id[from_node]
            to_node_id <- layer_ids[to_node]
            
            edges[[length(edges) + 1]] <- c(from_node_id, to_node_id, edge_size)
            
          }
          
        }
        
      }
      
      
    }
    
    
    last_layer_id <- layer_ids
    
  }
  
  codes <- c()
  for(i in 1:8){
    for(ii in 1:i){
      codes <-  c(codes,paste0(i,'_',ii))
    }
  }
  
  image_dir <- tempfile()
  raveio::dir_create2(image_dir)
  
  roi_var<- paste0('VAR_IS_ROI_',input$model_roi_variable)
  time_points_plot =  unique(local_data$analysis_data_raw$data$Time[local_data$analysis_data_raw$data$Time 
                                                                    %within% res$time_range_plot])
  n_timepoints_plot = length(time_points_plot)
  
  
  for( i in 1:height){
    
    clusters = v[i,]
    
    mse <- lapply(sort(unique(clusters)), function(ci){
      # rutabaga::collapse(indata[clusters == ci, , drop = FALSE], average = TRUE, keep = 2)
      apply(res$collapsed[clusters == ci,
                          !names(res$collapsed) %in% c('Subject', 'Electrode', roi_var),
                          drop=FALSE], 
            2, rutabaga::m_se)})
    
    label = lapply(sort(unique(clusters)), function(ci){
      sum(clusters == ci)
    })
    colors = ravebuiltins::get_palette("Dark2")
    
    
    names(colors)=unique(clusters)
    
    
    lapply(seq_len(i), function(ii){
      
      fname <- sprintf("%d_%d.png",i,ii )
      
      png(filename = file.path(image_dir, fname), width = 640, height = 480)
      on.exit(dev.off(), add = TRUE, after = FALSE)
      
      {
        yrange = c(min(sapply(mse, function(x){
          x[2,is.na(x[2,])] = 0
          x[1,is.na(x[1,])] = 0 # replace na with 0
          min(x[1,]-x[2,], na.rm = TRUE)
        }))
        ,max(sapply(res$mse, function(x){
          x[2,is.na(x[2,])] = 0
          x[1,is.na(x[1,])] = 0
          max(colSums(x), na.rm = TRUE)
        })))
        xaxi = pretty(time_points_plot)#change here
        yaxi = pretty(yrange)
        
        cl_mean = mse[[ii]][1,]
        cl_sd = mse[[ii]][2,]
        
        group_names = res$group_names
        n_cond_groups = length(group_names)
        
        cols = seq_len(n_cond_groups)
        
        rutabaga::plot_clean(xlim = res$time_range_plot, ylim=range(yaxi))
        rutabaga::ruta_axis(2, yaxi, cex.axis = 2)
        rutabaga::ruta_axis(1, labels = xaxi, at=xaxi,cex.axis = 2)
        
        lapply(seq_len(n_cond_groups), function(j){
          
          
          
          sel_sorted <- paste0(sort(time_points_plot),'_',j)
          
          x_lim <- seq(res$time_range_plot[1],res$time_range_plot[2], 
                       length.out = n_timepoints_plot)
          
          rutabaga::ebar_polygon(x_lim, cl_mean[sel_sorted], 
                                 sem = cl_sd[sel_sorted], col = cols[[j]])
          
        })
        
        legend('topleft',legend = paste('#elec :', label[[ii]]), bty='n', cex = 4)
        
        
      }
      
      # dev.off()
    })
    
  }
  
  
  img <- file.path(image_dir, sprintf('%s.png', codes))
  
  image <- sapply(img, function(x){
    base64enc::dataURI(file = x)
  })
  unlink(image_dir, recursive = TRUE)
  
  node_df <- data.frame(
    id = node_ids,
    level = node_levels,
    x = node_pos,
    # label = as.character(node_pos),
    shape = 'image',
    image = image
  )
  edge_df <- as.data.frame(do.call('rbind', edges))
  names(edge_df) <- c("from", "to", "label")
  edge_df <- edge_df[,c('from','to')]
  #edge_df$label <- as.character(edge_df$label)
  
  
  visNetwork(node_df, edge_df, width = "100%", height = "100vh") %>% 
    visNodes(shapeProperties = list(useBorderWithImage = FALSE), size = 64) %>%
    visEdges(arrows = "to") %>% 
    visHierarchicalLayout(direction = "LR", nodeSpacing = 150, treeSpacing = 400,
                          levelSeparation = 200, 
                          sortMethod = "directed") %>% 
    visInteraction(dragNodes = FALSE, zoomSpeed = 0.1) %>% 
    visConfigure(enabled = FALSE)
  
}


mds_plot <- function(){
  # rave::rave_context()
  results <- get_pipeline_results(fast_ok = TRUE)
  
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
  
  raveclusters::cluster_dendrogram(results)
  
}

optimal_cluster_number_plot <- function(){
  results <- get_pipeline_results(fast_ok = TRUE)
  
  shiny::validate(
    shiny::need(is.matrix(results$indata_analysis), 
                message = 'Please adjust (narrow) the analysis time window')
  )
  
  methods = c('silhouette', 'wss')
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
  
  for(method in methods) {
    cluster_idx_plot(
      indata_analysis, dis = dis, clustfun = clustfun,
      method = method, col = "dodgerblue3", cv_fold = cv_fold
    ) 
  }
  
}


cluster_plot <-  function(){
  
  # results <- ravecluster()
  results <- local_data$results
  
  shiny::validate(shiny::need(is.list(results) && !is.null(results$mse), 
                              message = 'Please press "Run Analysis" button'))
  
  
  raveclusters::cluster_visualization(
    results, color_scheme = "Beautiful Field",
    cex = 1, plot_range = input$plot_time_window)
  
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


