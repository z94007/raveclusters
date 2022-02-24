library(targets)
library(raveio)
source("common.R", local = TRUE, chdir = TRUE)
...targets <- list(`__Check_settings_file` = targets::tar_target_raw("settings_path", 
    "settings.yaml", format = "file"), `__Load_settings` = targets::tar_target_raw("settings", 
    quote({
        load_yaml(settings_path)
    }), deps = "settings_path"), input_new_input = targets::tar_target_raw("new_input", 
    quote({
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
        }
        return(project)
    }), deps = "project_name", cue = targets::tar_cue("thorough"), 
    pattern = NULL, iteration = "list"), get_input_file_meta_information = targets::tar_target_raw(name = "source_metas", 
    command = quote({
        {
            search_paths <- project$group_path("power_explorer")
            find_source = function(search_paths, fname) {
                fpaths = file.path(search_paths, "exports", fname)
                fexists = file.exists(fpaths)
                if (!any(fexists)) {
                  return(NULL)
                }
                return(fpaths[which(fexists)[1]])
            }
            source_metas = lapply(source_files, function(fpath) {
                fpath = find_source(search_paths, fpath)
                if (is.null(fpath)) {
                  return(NULL)
                }
                dat <- fst::read_fst(fpath, from = 1, to = 1)
                list(fpath = fpath, header = names(dat))
            })
            source_metas = dipsaus::drop_nulls(source_metas)
        }
        return(source_metas)
    }), deps = c("project", "source_files"), cue = targets::tar_cue("thorough"), 
    pattern = NULL, iteration = "list"), load_input_files = targets::tar_target_raw(name = "source_data", 
    command = quote({
        {
            library(rutabaga)
            headers = unique(unlist(lapply(source_metas, "[[", 
                "header")))
            tbls = dipsaus::drop_nulls(lapply(source_metas, function(x) {
                print("trying to load " %&% x$fpath)
                tbl <- fst::read_fst(x$fpath, as.data.table = TRUE)
                tbl = tbl[tbl$Project %in% project_name, ]
                if (!nrow(tbl)) {
                  return(NULL)
                }
                mish = headers[!headers %in% names(tbl)]
                for (m in mish) {
                  tbl[[m]] <- NA
                }
                conf = NULL
                yaml_path = paste0(x$fpath, ".yaml")
                if (file.exists(yaml_path)) {
                  conf = raveio::load_yaml(yaml_path)
                }
                print("returning loaded data ")
                return(list(data = tbl, conf = conf, path = x$fpath, 
                  subject = tbl$Subject[[1]]))
            }))
            res = do.call("rbind", lapply(tbls, "[[", "data"))
            if (!is.data.frame(res) || !nrow(res)) {
                res = NULL
            } else {
                try({
                  res$Electrode = as.character(res$Electrode)
                  res$Subject = as.character(res$Subject)
                  res$Condition = as.character(res$Condition)
                }, silent = TRUE)
                subjects = sapply(tbls, "[[", "subject")
                confs = lapply(tbls, "[[", "conf")
                names(confs) = subjects
                res = list(data = res, subjects = subjects, confs = confs, 
                  headers = names(res))
            }
            source_data <- res
        }
        return(source_data)
    }), deps = c("source_metas", "project_name"), cue = targets::tar_cue("thorough"), 
    pattern = NULL, iteration = "list"), get_ROI_var = targets::tar_target_raw(name = "roi_var", 
    command = quote({
        {
            roi_list <- c("VAR_IS_ROI_Hemisphere", "VAR_IS_ROI_freesurferlabel", 
                "VAR_IS_ROI_Group", "VAR_IS_ROI_Block")
            roi_var <- paste0("VAR_IS_ROI_", roi_options$variable)
        }
        return(roi_var)
    }), deps = "roi_options", cue = targets::tar_cue("thorough"), 
    pattern = NULL, iteration = "list"), find_data_column_name = targets::tar_target_raw(name = "var_name", 
    command = quote({
        {
            var_name = sprintf("%s_%s", baseline_method, analysis_event)
        }
        return(var_name)
    }), deps = c("baseline_method", "analysis_event"), cue = targets::tar_cue("thorough"), 
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
            raw_table = raw_table[, !names(raw_table) %in% excluded_roi, 
                with = FALSE]
            use_regex <- (roi_options$roi_ignore_gyrus_sulcus || 
                roi_options$roi_ignore_hemisphere)
            table_apply_roi <- function(table, roi_column, roi, 
                use_regex) {
                var <- table[[roi_column]]
                if (use_regex) {
                  pattern <- paste0("(", roi, ")", collapse = "|")
                  idx <- str_detect(var, pattern)
                } else {
                  idx <- var %in% roi
                }
                return(table[idx, ])
            }
            raw_table <- table_apply_roi(table = raw_table, roi_column = roi_var, 
                roi = filter_by_roi, use_regex = use_regex)
        }
        return(raw_table)
    }), deps = c("source_data", "roi_var", "roi_options", "filter_by_roi"
    ), cue = targets::tar_cue("thorough"), pattern = NULL, iteration = "list"), 
    apply_time_filter = targets::tar_target_raw(name = "collapsed", 
        command = quote({
            {
                library(rutabaga)
                collapsed = lapply(seq_along(input_groups), function(ii) {
                  group = input_groups[[ii]]
                  group_name = group$group_name
                  if (is.null(group_name) && group_name == "") {
                    group_name = sprintf("Group %d", ii)
                  }
                  group_condition = group$group_conditions
                  sub_plot = raw_table[raw_table$Condition %in% 
                    group_condition & raw_table$Time %within% 
                    plot_time_window, ]
                  sub_plot$Time = paste0(sub_plot$Time, "_", 
                    ii)
                  sub = sub_plot[sub_plot$Time %within% time_window, 
                    ]
                  sub_time = paste0(sub$Time, "_", ii)
                  fml <- Subject + Electrode + VAR_IS_ROI_freesurferlabel ~ 
                    Time
                  fml[[2]][[3]] <- parse(text = roi_var)[[1]]
                  collapsed_mean <- lapply(var_name, function(var) {
                    reshape2::dcast(sub_plot, fml, fun.aggregate = mean, 
                      value.var = var)
                  })
                  merged <- Reduce(function(a, b) {
                    merge(a, b, all = FALSE, by = c("Subject", 
                      "Electrode", roi_var))
                  }, collapsed_mean)
                  return(list(collapsed_mean = merged, group_name = group_name, 
                    group_index = ii, sub_time = sub_time))
                })
            }
            return(collapsed)
        }), deps = c("input_groups", "raw_table", "plot_time_window", 
        "time_window", "roi_var", "var_name"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), data_merging = targets::tar_target_raw(name = "merged", 
        command = quote({
            {
                merged = Reduce(function(a, b) {
                  list(collapsed_mean = merge(a$collapsed_mean, 
                    b$collapsed_mean, all = FALSE, by = c("Subject", 
                      "Electrode", roi_var)), sub_time = c(a$sub_time, 
                    b$sub_time))
                }, collapsed, right = FALSE)
            }
            return(merged)
        }), deps = c("roi_var", "collapsed"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), data_scaling = targets::tar_target_raw(name = "baseline", 
        command = quote({
            {
                library(rutabaga)
                baseline = lapply(seq_along(input_groups), function(ii) {
                  group = input_groups[[ii]]
                  group_name = group$group_name
                  if (is.null(group_name) && group_name == "") {
                    group_name = sprintf("Group %d", ii)
                  }
                  group_condition = group$group_conditions
                  baseline_raw = raw_table[raw_table$Condition %in% 
                    group_condition & raw_table$Time %within% 
                    baseline_time, ]
                  fml <- Subject + Electrode + VAR_IS_ROI_freesurferlabel ~ 
                    Time
                  fml[[2]][[3]] <- parse(text = roi_var)[[1]]
                  baseline <- lapply(var_name, function(var) {
                    reshape2::dcast(baseline_raw, fml, fun.aggregate = mean, 
                      value.var = var)
                  })
                  return(baseline)
                })
                baseline_merged = Reduce(function(a, b) {
                  baseline_mean = merge(a, b, all = FALSE, by = c("Subject", 
                    "Electrode", roi_var))
                  baseline_mean
                }, baseline, right = FALSE)
                baseline_mean_indata <- baseline_merged[, !names(baseline_merged) %in% 
                  c("Subject", "Electrode", roi_var)]
                baseline_mean <- rowMeans(baseline_mean_indata)
                baseline_sd <- apply(baseline_mean_indata, 1, 
                  sd)
                baseline <- list(baseline_mean = baseline_mean, 
                  baseline_sd = baseline_sd)
            }
            return(baseline)
        }), deps = c("input_groups", "raw_table", "baseline_time", 
        "roi_var", "var_name"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), data_decomposation = targets::tar_target_raw(name = "collapsed_data", 
        command = quote({
            {
                collapsed_data = merged$collapsed_mean
                if (check_scale) {
                  collapsed_data[, !names(collapsed_data) %in% 
                    c("Subject", "Electrode", roi_var)] <- t(scale(t(collapsed_data[, 
                    !names(collapsed_data) %in% c("Subject", 
                      "Electrode", roi_var)]), center = baseline$baseline_mean, 
                    baseline$baseline_sd))
                }
            }
            return(collapsed_data)
        }), deps = c("merged", "check_scale", "roi_var", "baseline"
        ), cue = targets::tar_cue("thorough"), pattern = NULL, 
        iteration = "list"), analysis_data = targets::tar_target_raw(name = "indata", 
        command = quote({
            {
                indata = collapsed_data[, !names(collapsed_data) %in% 
                  c("Subject", "Electrode", roi_var)]
                if (check_scale) {
                  indata = t(scale(t(indata), center = baseline$baseline_mean, 
                    baseline$baseline_sd))
                }
            }
            return(indata)
        }), deps = c("collapsed_data", "roi_var", "check_scale", 
        "baseline"), cue = targets::tar_cue("thorough"), pattern = NULL, 
        iteration = "list"), measure_distance = targets::tar_target_raw(name = "dis", 
        command = quote({
            {
                if (isTRUE(distance_method == "1 - correlation")) {
                  dis = as.dist(1 - cor(t(indata)))
                } else if (isTRUE(distance_method == "DTW")) {
                  dis = as.dist(dtw::dtwDist(indata))
                } else {
                  dis = dist(indata, method = distance_method)
                }
            }
            return(dis)
        }), deps = c("distance_method", "indata"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), apply_clustering = targets::tar_target_raw(name = "clusters", 
        command = quote({
            {
                n_clust = min(input_nclusters, nrow(indata))
                if (input_method == "H-Clust") {
                  hcl = stats::hclust(dis, method = hclust_method)
                  clusters <- stats::cutree(hcl, k = n_clust)
                } else if (input_method == "PAM") {
                  km <- cluster::pam(dis, k = n_clust, cluster.only = TRUE, 
                    keep.data = FALSE, keep.diss = FALSE)
                  clusters <- km
                }
            }
            return(clusters)
        }), deps = c("input_nclusters", "indata", "input_method", 
        "dis", "hclust_method"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), get_mse = targets::tar_target_raw(name = "mse", 
        command = quote({
            {
                library(dipsaus)
                mse <- lapply(sort(unique(clusters)), function(ci) {
                  apply(collapsed_data[clusters == ci, !names(collapsed_data) %in% 
                    c("Subject", "Electrode", roi_var), drop = FALSE], 
                    2, dipsaus::mean_se)
                })
            }
            return(mse)
        }), deps = c("clusters", "collapsed_data", "roi_var"), 
        cue = targets::tar_cue("thorough"), pattern = NULL, iteration = "list"))
