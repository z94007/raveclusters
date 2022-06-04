#' @export
cluster_on_brain <- function(
  results, force_template = FALSE, ..., 
  controllers = list(
    `Show Time` = FALSE,
    `Show Panels` = FALSE
  ),
  surface_types = c("pial", "smoothwm")) {
  # results <- ravecluster(
  #   names = c(
  #     "project", "collapsed",
  #     "condition_groups", "collapsed_array", "mse", "dis", "use_baseline",
  #     "baseline", "indata_analysis", "indata_plot", "cluster_table",
  #     "input_nclusters", "analysis_time_window", "plot_time_window",
  #     "power_unit"
  #   ))
  # force_template <- TRUE
  
  project_name <- results$project$name
  cluster_table <- results$cluster_table
  cluster_idx <- as.character(attr(cluster_table, "cluster_idx"))
  cluster_col <- as.character(attr(cluster_table, "color_space"))
  
  # event prefix
  event_prefix <- format_unit_of_analysis_name(names(get_unit_of_analysis()))
  
  cluster_table$Event <- 
    gsub(sprintf("^(%s)[_]{0,1}", paste(event_prefix, collapse = "|")),
         replacement = "", cluster_table$Event)
  cluster_table$Cluster <- as.integer(cluster_table$Cluster)
  
  cluster_table <- reshape2::dcast(cluster_table, Subject + Electrode + ROI ~ Event, fun.aggregate = function(x){ if(length(x)){x[[1]]} else { NA_integer_ } }, value.var = "Cluster")
  cluster_table$Project = project_name
  
  nms <- names(cluster_table)
  nms_sel <- !nms %in% c("Subject", "Electrode", "ROI", "Project")
  nms[nms_sel] <- sprintf("Cluster-%s", nms[nms_sel])
  cluster_names <- nms[nms_sel]
  names(cluster_table) <- nms
  for(nm in cluster_names) {
    cluster_table[[nm]] <- factor(
      as.character(cluster_table[[nm]]), levels = cluster_idx)
  }
  
  # get subjects & brain
  subjects <- unique(cluster_table$Subject)
  brain <- dipsaus::drop_nulls(lapply(subjects, function(sub){
    tryCatch({
      raveio::rave_brain(sprintf('%s/%s', project_name, sub), surfaces = surface_types)
    }, error = function(e){
      NULL
    })
  }))
  
  # check if the brain length is 0 or 1
  if(!length(subjects) || !length(brain)) {
    return(invisible("No RAVE-brain is found for any of these subjects"))
  }
  if(force_template || length(subjects) > 1) {
    brain <- threeBrain::merge_brain(.list = brain, 
                                     template_surface_types = surface_types)
  } else {
    brain <- brain[[1]]
  }
  
  # build electrode table
  electrode_table <- lapply(subjects, function(sub){
    re <- raveio::load_meta2(meta_type = "electrodes", project_name = project_name, subject_code = sub)
    if(is.data.frame(re)) {
      re$Subject <- sub
      return(re)
    }
    return(NULL)
  })
  electrode_table <- data.table::rbindlist(electrode_table, use.names = TRUE, fill = TRUE)
  
  bycols <- c("Project", "Subject", "Electrode")
  bycols <- bycols[
    bycols %in% names(cluster_table) &
      bycols %in% names(electrode_table)
  ]
  value_table <- merge(electrode_table, cluster_table, by = bycols, all.x = TRUE)
  value_table$Project <- project_name
  
  brain$set_electrode_values(value_table)
  
  controllers <- as.list(controllers)
  controllers[["Display Data"]] <- controllers[["Display Data"]] %OF% cluster_names
  
  
  brain$plot(
    side_width = 160, side_shift = c(0,0), 
    palettes = c(
      structure(
        lapply(cluster_names, function(nm){ cluster_col }),
        names = cluster_names
      ),
      list(
        'Selected' = c("black", '#1B9E77'),
        '[Subject]' = 'black'
      )
    ),
    ...,
    controllers = controllers
  )
}
