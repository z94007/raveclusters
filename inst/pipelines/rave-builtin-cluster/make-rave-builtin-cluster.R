library(targets)
library(raveio)
source("common.R", local = TRUE, chdir = TRUE)
...targets <- list(`__Check_settings_file` = targets::tar_target_raw("settings_path", 
    "settings.yaml", format = "file"), `__Load_settings` = targets::tar_target_raw("settings", 
    quote({
        load_yaml(settings_path)
    }), deps = "settings_path", cue = targets::tar_cue("always")), 
    input_new_input = targets::tar_target_raw("new_input", quote({
        settings[["new_input"]]
    }), deps = "settings"), input_input_groups = targets::tar_target_raw("input_groups", 
        quote({
            settings[["input_groups"]]
        }), deps = "settings"), input_source_files = targets::tar_target_raw("source_files", 
        quote({
            settings[["source_files"]]
        }), deps = "settings"), input_baseline_time = targets::tar_target_raw("baseline_time", 
        quote({
            settings[["baseline_time"]]
        }), deps = "settings"), input_project_name = targets::tar_target_raw("project_name", 
        quote({
            settings[["project_name"]]
        }), deps = "settings"), input_distance_method = targets::tar_target_raw("distance_method", 
        quote({
            settings[["distance_method"]]
        }), deps = "settings"), input_hclust_method = targets::tar_target_raw("hclust_method", 
        quote({
            settings[["hclust_method"]]
        }), deps = "settings"), input_input_nclusters = targets::tar_target_raw("input_nclusters", 
        quote({
            settings[["input_nclusters"]]
        }), deps = "settings"), input_roi_options = targets::tar_target_raw("roi_options", 
        quote({
            settings[["roi_options"]]
        }), deps = "settings"), input_analysis_event = targets::tar_target_raw("analysis_event", 
        quote({
            settings[["analysis_event"]]
        }), deps = "settings"), input_input_method = targets::tar_target_raw("input_method", 
        quote({
            settings[["input_method"]]
        }), deps = "settings"), input_plot_time_window = targets::tar_target_raw("plot_time_window", 
        quote({
            settings[["plot_time_window"]]
        }), deps = "settings"), input_check_scale = targets::tar_target_raw("check_scale", 
        quote({
            settings[["check_scale"]]
        }), deps = "settings"), input_filter_by_roi = targets::tar_target_raw("filter_by_roi", 
        quote({
            settings[["filter_by_roi"]]
        }), deps = "settings"), input_baseline_method = targets::tar_target_raw("baseline_method", 
        quote({
            settings[["baseline_method"]]
        }), deps = "settings"), input_time_window = targets::tar_target_raw("time_window", 
        quote({
            settings[["time_window"]]
        }), deps = "settings"), load_project = targets::tar_target_raw(name = "project", 
        command = quote({
            {
                library(raveio)
                project <- raveio::as_rave_project(project = project_name)
                project
            }
            return(project)
        }), deps = "project_name", cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), load_data = targets::tar_target_raw(name = "source_data", 
        command = quote({
            {
                search_paths <- file.path(project$group_path("power_explorer"), 
                  "exports")
                source_data <- raveclusters::import_configurations(search_paths = search_paths, 
                  file_names = source_files)
                print(names(source_data))
                source_data$data
            }
            return(source_data)
        }), deps = c("project", "source_files"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), get_ROI_var = targets::tar_target_raw(name = "roi_var", 
        command = quote({
            {
                roi_list <- c("VAR_IS_ROI_Hemisphere", "VAR_IS_ROI_freesurferlabel", 
                  "VAR_IS_ROI_Group", "VAR_IS_ROI_Block")
                roi_var <- paste0("VAR_IS_ROI_", roi_options$variable)
            }
            return(roi_var)
        }), deps = "roi_options", cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), apply_RIO_filters = targets::tar_target_raw(name = "raw_table", 
        command = quote({
            {
                library(raveclusters)
                raw_table <- source_data$data
                roi_list <- c("VAR_IS_ROI_Hemisphere", "VAR_IS_ROI_freesurferlabel", 
                  "VAR_IS_ROI_Group", "VAR_IS_ROI_Block")
                excluded_roi <- roi_list[!roi_list %in% roi_var]
                selected_names <- names(raw_table)
                selected_names <- selected_names[!selected_names %in% 
                  excluded_roi]
                raw_table = raw_table[, !names(raw_table) %in% 
                  excluded_roi, with = FALSE]
                use_regex <- (roi_options$roi_ignore_gyrus_sulcus || 
                  roi_options$roi_ignore_hemisphere)
                raw_table <- raveclusters::table_apply_roi(table = raw_table, 
                  roi_column = roi_var, roi = filter_by_roi, 
                  use_regex = use_regex, na_rm = FALSE)
            }
            return(raw_table)
        }), deps = c("source_data", "roi_var", "roi_options", 
        "filter_by_roi"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), find_data_column_name = targets::tar_target_raw(name = "var_name", 
        command = quote({
            {
                var_name <- sprintf("%s_%s", baseline_method, 
                  analysis_event)
            }
            return(var_name)
        }), deps = c("baseline_method", "analysis_event"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), collapse_data_by_condition_group = targets::tar_target_raw(name = "collapsed", 
        command = quote({
            {
                library(rutabaga)
                library(data.table)
                collapsed = lapply(seq_along(input_groups), function(ii) {
                  group = input_groups[[ii]]
                  group_condition = group$group_conditions
                  group_columns <- c("Subject", "Electrode", 
                    roi_var, "Time")
                  collapsed_table <- raw_table[Condition %in% 
                    group_condition, lapply(.SD, function(x, 
                    w) {
                    if (!is.numeric(x)) {
                      x <- as.double(x)
                    }
                    sel <- !is.na(x)
                    if (!any(sel)) {
                      return(NA_real_)
                    }
                    sum((x * w)[sel])/sum(w[sel])
                  }, w = count), keyby = group_columns, .SDcols = var_name]
                  melted_table <- data.table::melt(collapsed_table, 
                    id.vars = group_columns, measure.vars = var_name, 
                    variable.factor = FALSE, variable.name = "Variable", 
                    value.name = "Value")
                  melted_table$Group <- ii
                  melted_table
                })
                collapsed <- data.table::rbindlist(collapsed)
                print(collapsed)
            }
            return(collapsed)
        }), deps = c("input_groups", "roi_var", "raw_table", 
        "var_name"), cue = targets::tar_cue("thorough"), pattern = NULL, 
        iteration = "list"), reshape_collapsed_data_to_matrix = targets::tar_target_raw(name = "collapsed_array", 
        command = quote({
            {
                collapsed$Unit <- sprintf("%s^%s^%s", collapsed$Subject, 
                  collapsed$Electrode, collapsed$Variable)
                collapsed_array <- reshape2::acast(data = collapsed, 
                  value.var = "Value", formula = Unit ~ Time + 
                    Group, fill = NA_real_, fun.aggregate = function(x) {
                    sel <- is.na(x)
                    if (all(sel)) {
                      return(NA_real_)
                    }
                    mean(x[!sel])
                  })
                dnames <- dimnames(collapsed_array)
                names(dnames) <- c("Unit", "Time")
                time_mat <- stringr::str_split_fixed(dnames$Time, 
                  "_", n = 2)
                time_order <- order(as.integer(time_mat[, 2]), 
                  as.double(time_mat[, 1]))
                dnames$Time <- dnames$Time[time_order]
                collapsed_array <- collapsed_array[, time_order, 
                  drop = FALSE]
                dimnames(collapsed_array) <- dnames
                time_mat <- as.data.frame(stringr::str_split_fixed(dnames$Time, 
                  "_", n = 2))
                names(time_mat) <- c("Time", "Group")
                time_mat$Time <- as.double(time_mat$Time)
                time_mat$Group <- as.integer(time_mat$Group)
                attr(collapsed_array, "time_table") <- time_mat
                item_table <- stringr::str_split_fixed(dnames$Unit, 
                  "\\^", n = 3)
                item_table <- as.data.frame(item_table)
                names(item_table) <- c("Subject", "Electrode", 
                  "Event")
                item_table$Electrode <- as.integer(item_table$Electrode)
                attr(collapsed_array, "item_table") <- item_table
                print(names(attributes(collapsed_array)))
            }
            return(collapsed_array)
        }), deps = "collapsed", cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), `z-score_baseline_collapsed_array` = targets::tar_target_raw(name = "baseline", 
        command = quote({
            {
                library(rutabaga)
                time_table <- attr(collapsed_array, "time_table")
                baseline <- lapply(seq_along(input_groups), function(ii) {
                  column_sel <- time_table$Group == ii
                  baseline_sel <- column_sel & (time_table$Time %within% 
                    baseline_time)
                  bl <- collapsed_array[, baseline_sel, drop = FALSE]
                  blmean <- rowMeans(bl, na.rm = TRUE)
                  if (sum(baseline_sel) < 2) {
                    stop("Cannot z-score data. Please increase your baseline range.")
                  }
                  blstdev <- apply(bl, 1, stats::sd, na.rm = TRUE)
                  re <- ((collapsed_array[, column_sel, drop = FALSE]) - 
                    as.vector(blmean))/as.vector(blstdev)
                  list(data = re, time = time_table$Time[column_sel])
                })
            }
            return(baseline)
        }), deps = c("collapsed_array", "input_groups", "baseline_time"
        ), cue = targets::tar_cue("thorough"), pattern = NULL, 
        iteration = "list"), `column-bind_baseline_list` = targets::tar_target_raw(name = "baseline_array", 
        command = quote({
            {
                baseline_array <- do.call("cbind", lapply(baseline, 
                  function(x) {
                    x$data
                  }))
                attributes(baseline_array) <- attributes(collapsed_array)
            }
            return(baseline_array)
        }), deps = c("baseline", "collapsed_array"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), slice_data_by_anslysis_window = targets::tar_target_raw(name = "indata_analysis", 
        command = quote({
            {
                library(rutabaga)
                indata_analysis <- lapply(baseline, function(item) {
                  item$data[, item$time %within% time_window, 
                    drop = TRUE]
                })
                indata_analysis <- do.call("cbind", indata_analysis)
                sel <- stats::complete.cases(indata_analysis)
                if (!sum(sel)) {
                  stop("The analysis window cannot have missing data, but all signals contain missing values. Try narrowing the analysis range")
                }
                indata_analysis <- indata_analysis[sel, , drop = FALSE]
                attr(indata_analysis, "complete_cases") <- sel
            }
            return(indata_analysis)
        }), deps = c("baseline", "time_window"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), slice_data_by_ploting_window = targets::tar_target_raw(name = "indata_plot", 
        command = quote({
            {
                library(rutabaga)
                sel <- attr(indata_analysis, "complete_cases")
                indata_plot <- lapply(baseline, function(item) {
                  item$data[sel, , drop = TRUE]
                })
                indata_plot <- do.call("cbind", indata_plot)
            }
            return(indata_plot)
        }), deps = c("indata_analysis", "baseline"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), measure_distance = targets::tar_target_raw(name = "dis", 
        command = quote({
            {
                if (isTRUE(distance_method == "1 - correlation")) {
                  dis = as.dist(1 - cor(t(indata_analysis)))
                } else if (isTRUE(distance_method == "DTW")) {
                  dis = as.dist(dtw::dtwDist(indata_analysis))
                } else {
                  dis = dist(indata_analysis, method = distance_method)
                }
            }
            return(dis)
        }), deps = c("distance_method", "indata_analysis"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), apply_clustering = targets::tar_target_raw(name = "clusters", 
        command = quote({
            {
                n_clust = min(input_nclusters, nrow(dis))
                item_table <- attr(collapsed_array, "item_table")
                sel <- attr(indata_analysis, "complete_cases")
                item_table <- item_table[sel, , drop = FALSE]
                if (input_method == "H-Clust") {
                  hcl = stats::hclust(dis, method = hclust_method)
                  item_table$Cluster <- stats::cutree(hcl, k = n_clust)
                } else if (input_method == "PAM") {
                  km <- cluster::pam(dis, k = n_clust, cluster.only = TRUE, 
                    keep.data = FALSE, keep.diss = FALSE)
                  item_table$Cluster <- km
                }
                clusters <- item_table
                print(dim(clusters))
                print(head(clusters))
            }
            return(clusters)
        }), deps = c("input_nclusters", "dis", "collapsed_array", 
        "indata_analysis", "input_method", "hclust_method"), 
        cue = targets::tar_cue("thorough"), pattern = NULL, iteration = "list"), 
    get_mse = targets::tar_target_raw(name = "mse", command = quote({
        {
            library(dipsaus)
            cluster_idx <- sort(unique(clusters$Cluster))
            mse <- lapply(cluster_idx, function(ci) {
                apply(indata_plot[clusters$Cluster == ci, , drop = FALSE], 
                  2, function(x) {
                    dipsaus::mean_se(x, na.rm = TRUE, se_na_as_zero = TRUE)
                  })
            })
        }
        return(mse)
    }), deps = c("clusters", "indata_plot"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"))
