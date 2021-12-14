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




# local_data = ...local_data
# input = ...input

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
  
  mds_res = cmdscale(dist(res$indata, method = input$mds_distance_method), k=2)
  # ravebuiltins:::set_palette_helper
  assign('res', res, envir = globalenv())
  
  #colors
  collapsed_data <- res$collapsed
  
  par(mfrow = c(1,1))

  pcs = 1:2#why???
  plot(mds_res[,pcs], type = 'n',xlab = '', ylab = '')
  text(mds_res[,pcs], labels = paste0(collapsed_data$Subject,collapsed_data$Electrode),
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
  plot(dend, las = 1,horiz = TRUE, yaxt='n',#remove the y axis and labels
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

  plot_signals2 <- function(signals, space, ylim1 = c(0, 1), ...){
    space <- stats::quantile(signals, space, na.rm = TRUE) * 2
    nr <- nrow(signals)
    ylim0 <- range(seq_len(nr) * space + signals, na.rm = TRUE)
    scale <- (ylim1[2] - ylim1[1]) / (ylim0[2] - ylim0[1])
    space <- space * scale
    signals <- (signals - ylim0[1]) * scale + ylim1[1]
    
    plot_clean(xlim = c(1, ncol(signals)), ...)
    
    plot_signals(signals = signals, space = space, space_mode = "asis", 
                 new_plot = FALSE)
  }
  
  plot_signals2(res$indata, space = 0.99, ylim = c(-1, n+2), ylim1 = c(0, n+1))
  
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
  } else if (input$input_method == "PAM") {
    clustfun = cluster::pam
  }
  par(mfrow= c(1,2))
  
  op_res <- lapply(methods, function(x){
    factoextra::fviz_nbclust(res$indata,FUNcluster = clustfun, method =x, 
                             k.max = ceiling(dim(res$indata)[1]/2))
  })
  junk <- lapply(op_res, function(x){
    plot(x$data$y, pch = 20, type = 'o', xlab = x$labels$x, ylab =x$labels$y, lwd=2,las = 1)
    lst <- sort(x$data$y, index.return=TRUE, decreasing=TRUE)
    if(!is.null(x$labels$xintercept)){
      points(lst$ix[1:3],lst$x[1:3],col = 'red',pch =19)
      legend('topright', 'suggested number of clusters', col = 'red',pch =19, bty='n', text.font = 2)
      }
    }
  )
  #})

}


cluster_plot <-  function(separate = FALSE, cex.main = shiny_cex.main){
  
  
  palette(ravebuiltins::get_palette("Beautiful Field")) #condition group color code
  
  res <- local_data$my_results
  
  nclust = max(res$clusters_res)
  
  shiny::validate(shiny::need(!is.null(res$mse), message = 'Please press "Run Analysis" '))
  
  #rave::set_rave_theme()
  if( separate ){
    
  } else {
    if( nclust <= 4 ){
      par(mfrow = c(1, nclust))
    }else{
      nrow = ceiling((nclust) / 4)
      par(mfrow = c(nrow, 4))
    }
    par(mar = c(4.1,4.1, 4.1, 2))
  }
  
  
  

  time_points_plot =  unique(local_data$analysis_data_raw$data$Time[local_data$analysis_data_raw$data$Time 
                                                               %within% res$time_range_plot])
  
  time_points =  time_points_plot[time_points_plot %within% res$time_range]
  
  n_timepoints_plot = length(time_points_plot)
  
  n_timepoints = length(time_points)
  
  group_names = res$group_names
  n_cond_groups = length(group_names)
  
  var_name = input$trial_selected
  n_var = length(var_name)
  
  yrange = c(min(sapply(res$mse, function(x){
    x[2,is.na(x[2,])] = 0 # replace na with 0
    min(x[1,]-x[2,], na.rm = TRUE)
  }))
  ,max(sapply(res$mse, function(x){
    x[2,is.na(x[2,])] = 0
    max(colSums(x), na.rm = TRUE)
  })))
  xaxi = pretty(time_points_plot)
  yaxi = pretty(yrange)
  
  
  cache <- dipsaus::iapply(res$mse,function(x, cl_idx){
    
    # debug settings
    # x = res$mse[[1]]
    # cl_idx = 1
    # time_points = preload_info$time_points
    
    cl_mean = x[1,]
    cl_sd = x[2,]
    
    
    # case 1 variable y-lim
    #FIXME#yrange = range(cl_mean, cl_mean+cl_sd, cl_mean-cl_sd, na.rm = TRUE)#keep it?
    # case 2 fixed yrange for all plots
    #rutabaga::plot_clean(time_points, ylim=yrange) ##FIXME
    
    # set colors and layout canvase
    cols = seq_len(n_cond_groups)

    rutabaga::plot_clean(xlim = res$time_range_plot, ylim=range(yaxi))
    rutabaga::ruta_axis(2, yaxi)
    rutabaga::ruta_axis(1, labels = xaxi, at=xaxi)#n_timepoints_plot*xaxi/res$time_range_plot[2])
    
    #plot the rectangle of analysis window
    x_rect <- res$time_range
    y_rect <- range(yaxi)
    
    rect(x_rect[1], y_rect[1],x_rect[2],y_rect[2],
         col = rgb(red = 1, green = 0, blue = 0, alpha = 0.05), border = NA)
    legend(x_rect[1],y_rect[2],'Analysis', text.col = 'red', bty='n', 
           text.font = 1,)#FIXME#the location of this should not overlap with the legend of group conditon
    
    # plot the lines 
    lapply(seq_len(n_cond_groups), function(j){


      
      sel_sorted <- paste0(sort(time_points_plot),'_',j)
      
      x_lim <- seq(res$time_range_plot[1],res$time_range_plot[2], 
                   length.out = n_timepoints_plot)
        
      rutabaga::ebar_polygon(x_lim, cl_mean[sel_sorted], 
                             sem = cl_sd[sel_sorted], col = cols[[j]])
      
    })
    
    
    
    # add the line to seperate different events
    #abline(v = n_timepoints, lty = 2,col = "gray")
    
    # add the legend of condition groups
    lapply(seq_len(n_var), function(i){
      #legend(x = (i-1)*n_timepoints,y = yrange[2], var_name[i], bty='n', text.font = 2,cex = 1)
      legend(x = res$time_range_plot[1],y = range(yaxi)[2], group_names, bty='n', 
             text.font = 1, text.col = cols, cex = 1)}
    
    )
    
    #label of y-axis
    if(input$check_scale){
      y_label = 'z-scored'
    } else {
      y_label = NULL
    }
    
    mtext(paste0(y_label,'% change Amplitude'), side = 2, line = 2,cex = 1.5)
    mtext('Time(s)', side = 1, line = 2,cex = 1.5)
    # mtext('z-score % change Amplitude', side = 3, line = 0, at= 0)
    
    
    # gc <- mapply(function(sub_x,ii){
    #   lines(time_points, sub_x, col =ii)
    #   cols <- c(cols,ii)
    #   gnames <- c(gnames,input$input_groups[[ii]]$group_name)
    #   print(input$input_groups[[ii]]$group_name)
    #   return(list(gnames = gnames, cols = cols))
    # }, x, seq_along(input$input_groups))
      
      #rutabaga::ruta_axis(1, xaxi)
      #,labels = if(separate){}else{local_data$analysis_data_raw$headers[3]})
     
    ravebuiltins:::rave_title(
      sprintf(
        '%s%d (n=%d)',
        'Cluster',
        cl_idx,
        sum(res$clusters_res == cl_idx)
        ),
      col = res$colors[cl_idx],
      cex = cex.main
      )

  })
  
  
}

# cluster_plot1 <- function(){
#   res <- local_data$my_results
#   shiny::validate(shiny::need(!is.null(res$cluster_means), message = 'Please press "Run Analysis" '))
#   
#   time_points =  res$time_points
#   ymax = max(unlist(res$cluster_means))
#   xaxi = pretty(time_points)
#   yaxi = pretty(c(-100, ymax))
#   rutabaga::plot_clean(time_points, ylim=c(-100, ymax)) ##FIXME
#   rutabaga::ruta_axis(1, xaxi)
#   rutabaga::ruta_axis(2, yaxi)
#   
#   invisible(lapply(unique(res$clusters_res),function(x){
#     lines(time_points, res$cluster_means[[x]], col = res$colors[as.character(x)])
#   }))#FIXME
# 
# }
  

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

