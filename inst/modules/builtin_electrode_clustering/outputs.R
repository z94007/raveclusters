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
  
  res <- local_data$my_results
  
  shiny::validate(
    shiny::need(!is.null(res$indata) && ncol(res$indata) > 2, 
                message = 'Please press "Run Analysis" button.'),
    shiny::need(length(res$input_nclusters) && !is.na(res$input_nclusters) &&res$input_nclusters > 1, 
                message = 'Number of clusters must be greater than 1')
  )
  
  rave::set_rave_theme()
  
  #mds_res = cmdscale(dist(res$indata, method = input$mds_distance_method), k=2)
  # ravebuiltins:::set_palette_helper
  
  assign('res', res, envir = globalenv())
  
  #colors
  collapsed_data <- res$collapsed
  
  par(mfrow = c(1,1))

  pcs = 1:2#why???
  plot(res$mds_res[,pcs], type = 'n',xlab = '', ylab = '')
  text(res$mds_res[,pcs], labels = paste0(collapsed_data$Subject,collapsed_data$Electrode),
       col = res$colors[res$clusters_res])
  legend('topright', sprintf('Cluster %d', seq_along(unique(res$clusters_res))),
                             bty='n', text.font = 2, text.col = res$colors[seq_along(unique(res$clusters_res))])
  ravebuiltins:::rave_title(sprintf('%d %s %d %s',
                                    length(res$collapsed$Electrode),
                                    'electrodes across',
                                    length(unique(res$collapsed$Subject)),
                                    'patients'))

}


cluster_membership_table <- function(){
  
  if(is.list(local_data$my_results)){
    tbl = convert_cluster_table(local_data$my_results$cluster_table,
                                split_by = 'Subject', var = "Cluster",
                                value = "Electrode")
    div(
      style = 'overflow-x:scroll; height: 380px',
      HTML(knitr::kable(tbl, format = 'html', row.names = FALSE,
                        table.attr = 'class="table shiny-table table-striped"')) # spacing-xs
    )
  }else{
    div(
      style = 'height: 380px',
    )
  }
  
}


dendrogram_plot <- function() {
  res <- local_data$my_results
  
  shiny::validate(shiny::need(!is.null(res$mse), message = 'Please press "Run Analysis" '))
  
  shiny::validate(shiny::need(input$input_method == 'H-Clust', 'Only available for method = H-clust'))
  
  shiny::validate(shiny::need('hclust' %in% class(local_data$cluster_method_output), message = 'Please press "Run Analysis" '))
  
  labels = res$collapsed %$% paste0(Subject, Electrode)
  
  n = length(labels)
  k = res$input_nclusters
  col = res$colors[1:k]
  
  leafCol <- function(x,col){
    if(stats::is.leaf(x)){
      attr(x,'label') <- labels[x]
      attr(x, 'nodePar') <- list(lab.col = res$colors[res$clusters_res[x]],pch = 46,cex=0 )
      attr(x, "edgePar") <- list(col = res$colors[res$clusters_res[x]])
    }else{
      if (is.null(attr(x, "edgePar"))) {
        attr(x, "edgePar") <- list(col = col)
      }
    }
    unclass(x)
  } 

  #set lay out
  layout(matrix(1:2, ncol=2),
         widths = c(3/4, 1/4))
  
  par(cex = .7, mar = c(0,1,0,1))
  
  # define the dendrogram
  dend <- as.dendrogram(local_data$cluster_method_output)
  
  #color the nodes(leaves) and branches of the dendrogram (from dendextend package color_branches)
  g <- dendextend::cutree(local_data$cluster_method_output,k = k,h = NULL)
  descendTree <- function(sd) {
    groupsinsubtree <- unique(g[labels(sd)])
    if (length(groupsinsubtree) > 1) {
      for (i in seq(sd)) {
        sd[[i]] <- descendTree(sd[[i]])
      }
    }
    else {
      sd <- dendrapply(sd, leafCol,col[groupsinsubtree])
      # if (!is.null(groupLabels)) {
      #   attr(sd, "edgetext") <- groupLabels[groupsinsubtree]
      #   attr(sd, "edgePar") <- c(attr(sd, "edgePar"), 
      #                            list(p.border = col[groupsinsubtree]))
      # }
    }
    unclass(sd)
  }
  
  dend <- descendTree(dend) 
  class(dend) <- 'dendrogram'
  
  #plot the horizontal dendrogram
  plot(dend,las = 1,horiz = TRUE, yaxt='n',#remove the y axis and labels
       ylim = c(0, n+1))

  
  #add clustering cutting line
  MidPoint = (local_data$cluster_method_output$height[n-k] + local_data$cluster_method_outpu$height[n-k+1]) / 2
  abline(v = MidPoint, lty=2)
  
  # ravebuiltins:::rave_title(sprintf('%s %d %s %d %s','Hierarchical clustering of',
  #                                   length(res$collapsed$Electrode),
  #                                   'electrodes across',
  #                                   length(unique(res$collapsed$Subject)),'patients'),cex = 1.5)
  legend('topleft', sprintf('Cluster %d', seq_along(unique(res$clusters_res))),
                             bty='n', text.font = 2, cex=1.5, text.col = res$colors[seq_along(unique(res$clusters_res))])
    # legend('topleft', legend=paste0('clust', rev(runle$values)),
    #      cex=1, text.col = 1 + rev(runle$values), bty='n')

  # plot_signals2 <- function(signals, space, ylim1 = c(0, 1), ...){
  #   space <- stats::quantile(signals, space, na.rm = TRUE) * 2
  #   nr <- nrow(signals)
  #   ylim0 <- range(seq_len(nr) * space + signals, na.rm = TRUE)
  #   scale <- (ylim1[2] - ylim1[1]) / (ylim0[2] - ylim0[1])
  #   space <- space * scale
  #   signals <- (signals - ylim0[1]) * scale + ylim1[1]
  #   
  #   plot_clean(xlim = c(1, ncol(signals)), ...)
  #   
  #   plot_signals(signals = signals, space = space, space_mode = "asis", 
  #                new_plot = FALSE)
  # }
  # 
  # plot_signals2(res$indata, space = 0.99, ylim = c(-1, n+2), ylim1 = c(0, n+1))
  
  plot_clean(xlim = c(0,1), ylim = c(0,n+1))
  image( t(res$indata[order.dendrogram(dend),]),  y=1:n,
        col= hcl.colors(100, palette = "BluYl",rev = TRUE),
        yaxt = 'n',bty = 'n', xaxt= 'n', add = TRUE)
  
}

optimal_cluster_number_plot <- function(){
  res <- local_data$my_results
  
  shiny::validate(
    shiny::need(isTRUE(input$op_run), message = 'Click the checkbox to enable'), 
    shiny::need(!is.null(res)&&!is.null(res$indata), message = 'Please press "Run Analysis" after loading data')
  )
    
  rave::set_rave_theme()#why
  #, message = 'Please press "Optimal Number of Clusters Analysis" '))
 # observe(input$op_run,{  
  
  methods = c('silhouette','wss')
  if (input$input_method == "H-Clust"){
    clustfun = factoextra::hcut
  } else if (input$input_method == "K-Medois") {
    clustfun = cluster::pam
  }
  par(mfrow= c(1,2))
  
  op_res <- lapply(methods, function(x){
    factoextra::fviz_nbclust(res$indata,FUNcluster = clustfun, method =x, 
                             diss = res$dis,
                             k.max = 8)
  })
  junk <- lapply(op_res, function(x){
    plot(x$data$y, pch = 12, type = 'o', xlab = x$labels$x, ylab =x$labels$y, lwd=2,las = 1)
    lst <- sort(x$data$y, index.return=TRUE, decreasing=TRUE)
    if(!is.null(x$labels$xintercept)){
      points(lst$ix[1:3],lst$x[1:3],col = 'red',pch =19)
      legend('topright', sprintf('%s %s','suggested number of clusters',
                                 paste(lst$ix[1:3], collapse = ', ')),  
             bty='n', text.font = 2)
      #the return value of paste(1,2,3) is different from paste(c(1:3))
      }
    }
  )
  #})

}


cluster_plot <-  function(){
  
  # results <- ravecluster()
  results <- local_data$results
  
  shiny::validate(shiny::need(is.list(results) && !is.null(results$mse), 
                              message = 'Please press "Run Analysis" button'))
  
  
  raveclusters::cluster_visualization(
    results, color_scheme = "Beautiful Field",
    cex = shiny_cex.main, 
    plot_range = input$plot_time_window)
  
}
  

viewer_3d_fun <- function(...){
  # brain = rave::rave_brain2('congruency/YAB')
  res <- local_data$my_results
  
  subjects = local_data$analysis_data_raw$subjects
  project_name = subject$project_name
  
  tbl = res$cluster_table
  tbl$Cluster = paste('Cluster', tbl$Cluster)
  tbl$Project = project_name

  shiny::validate(
    shiny::need(is.data.frame(tbl), message = 'Please import data and run analysis')
  )
  
  roi_varname <- isolate(input$model_roi_variable)
  if(length(roi_varname) == 1){
    tbl[[roi_varname]] = res$roi
  }
  
  brain = lapply(subjects, function(sub){
    rave::rave_brain2(sprintf('%s/%s', project_name, sub))
  })
  
  brain <- dipsaus::drop_nulls(brain)
  if(length(subjects) > 1){
    brain = threeBrain::merge_brain(.list = brain)
    
    all_electrodes <- do.call('rbind', lapply(brain$objects, function(b){
      elecs <- b$electrodes$raw_table$Electrode
      etbl <- data.frame(
        Subject = b$subject_code,
        Electrode = elecs,
        Selected = elecs %in% tbl$Electrode[tbl$Subject == b$subject_code]
      )
    }))
    
    
  } else if(!length(brain)) {
    # show message like "no brain exists"
    message('there is no brain data exists')
  } else {
    brain <- brain[[1]]
    elecs <- brain$electrodes$raw_table$Electrode
    all_electrodes <- data.frame(
      Subject = brain$subject_code,
      Electrode = elecs,
      Selected = elecs %in% tbl$Electrode
    )
  }

  
  brain$set_electrode_values(tbl)
  brain$set_electrode_values(all_electrodes)
  
  brain$plot(
    side_width = 160, side_shift = c(0,0), 
    palettes = list(
      'Cluster' = res$colors,
      'Selected' = c("black", '#1B9E77'),
      '[Subject]' = 'black'
    )
  )
}


# cluster_plot2 <- function(){
#   res <- local_data$my_results
#   
#   shiny::validate(
#     shiny::need(!is.null(res$cluster_means), message = 'Please press "Run Analysis" button.')
#   )
#   
#   if( res$input_nclusters <= 3 ){
#     par(mfrow = c(1, res$input_nclusters))
#   }else{
#     nrow = ceiling(res$input_nclusters / 3)
#     par(mfrow = c(nrow, 3))
#   }
#   
#   time_points = as.numeric(names(res$indata))#preload_info$time_points
#   
# cluster_vis <- mapply(function(cm, ii) {
#   
#     rutabaga::plot_clean(time_points, ylim=c(-100, 500)) ##FIXME
#     gnames = list()
#     cols = NULL
#     
#     for( j in seq_along(input_groups) ){
#       g = input_groups[[j]]
#       conditions = g$group_conditions
#       gnames[[j]] = g$group_name
#       lines(time_points, colMeans(res$indata[which(res$collapsed$ConditionGroup %in% g$group_name & res$clusters_res ==ii),]),
#             col=j)
#       
#       cols = c(cols, j)
#     }
#     
#     # lines(baselined$dimnames$Time, colMeans(cm[which(baselined$dimnames$Trial %in% v_trials),]),
#     #       col="#377EB8")
#     
#     legend('topright', unlist(gnames), bty='n', text.font = 2,
#            text.col = cols)
#     text(quantile(time_points, 0.15),200, paste0('n_elec=', sum(res$clusters_res == ii)))# didn't work
#     title(main=rutabaga::deparse_svec(res$collapsed$Electrode[which(ii == res$clusters_res)]))
#     
#     rutabaga::ruta_axis(1, pretty(time_points))
#     rutabaga::ruta_axis(2, pretty(-100:1000))
#     
#   }, res$cluster_means, seq_along(res$cluster_means))
# }

