#' @name rave-cluster-data-import
#' @title Data import functions used to locate data exported by power explorer
#' module.
#' @param search_paths directories to search for data
#' @param pattern search regular expression pattern
#' @returns potential file names to be loaded
#' @export
scan_power_explorer_exports <- function(search_paths, pattern = '\\.fst$') {
  
  # # DEBUG
  # 
  # search_paths <- '~/rave_data/data_dir/demo/_project_data/power_explorer/exports/'
  # update <- TRUE
  # new_selected <- NULL
  # pattern <- '\\.fst$'
  # 
  # # End: DEBUG
  
  if(!length(search_paths)){
    return(NULL)
  }
  
  choices <- unlist(lapply(search_paths, list.files, pattern = pattern, ignore.case = TRUE))
  
  # Order file names by date-time (descending order)
  dt = stringr::str_extract(choices, '[0-9]{8}-[0-9]{6}')
  od = order(strptime(dt, '%Y%m%d-%H%M%S'), decreasing = TRUE)
  choices = choices[od]
  
  return(choices)
}



#' @rdname rave-cluster-data-import
#' @param fname character vector of length 1: file name to locate within \code{search_paths}
#' @export
find_source <- function(search_paths, fname){
  
  # # DEBUG
  # 
  # search_paths <- '~/rave_data/data_dir/demo/_project_data/power_explorer/exports/'
  # fname <- 'YCK_pow_by_cond-20211214-141952.fst'
  # # End: DEBUG
  
  fpaths <- file.path(search_paths, fname)
  fexists <- file.exists(fpaths)
  if(!any(fexists)){ return(NULL) }
  
  return(fpaths[fexists][[1]])
}

#' @rdname rave-cluster-data-import
#' @param file_names vector of \code{fname} to import
#' @export
import_configurations <- function(search_paths, file_names){

  # # DEBUG
  # search_paths <- '~/rave_data/data_dir/demo/_project_data/power_explorer/exports/'
  # file_names <- c('YCK_pow_by_cond-20211214-141952.fst', 'YAK_MDM-20191105-162604.csv')
  # fpath <- file_names[[1]]
  # # End: DEBUG

  progress <- dipsaus::progress2("Importing data", max = length(file_names), shiny_auto_close = TRUE)

  meta <- lapply(file_names, function(fpath){
    fpath <- find_source(search_paths, fpath)
    if( is.null(fpath) ){ return(NULL) }
    if(endsWith(tolower(fpath), suffix = "fst")) {
      sample <- fst::read_fst(fpath, from = 1, to = 1)
      header <- names(sample)
      get_table <- function(){
        fst::read_fst(fpath, as.data.table = TRUE)
      }
    } else if (endsWith(tolower(fpath), suffix = "csv")) {
      header <- unname(unlist(utils::read.csv(fpath, header = FALSE, nrows = 1)))
      sample <- utils::read.csv(fpath, header = TRUE, nrows = 1)
      names(sample) <- header
      get_table <- function(){
        data.table::fread(fpath, header = TRUE)
        names(table) <- header
        table
      }
    } else {
      return(NULL)
    }


    project_name <- sample$Project
    subject_code <- sample$Subject

    # check if project_name and subject_code are valid
    if(length(project_name) != 1 || is.na(project_name) || !nchar(trimws(project_name))) {
      warning("Invalid project name: ", basename(fpath))
      return(NULL)
    }
    if(length(subject_code) != 1 || is.na(subject_code) || !nchar(trimws(subject_code))) {
      warning("Invalid subject code: ", basename(fpath))
      return(NULL)
    }

    if(!all(c("Time", "Trial", "Electrode") %in% header)) {
      warning("One or more columns (`Time`, `Trial`, `Electrode`) are missing from table ", basename(fpath))
      return(NULL)
    }

    # try to load headers as well
    yaml_path <- paste0(fpath, ".yaml")
    conf <- list()
    if(file.exists(yaml_path)) {
      conf <- try({ raveio::load_yaml(yaml_path) }, silent = TRUE)
    }

    # get variables to be stored as file-arrays
    vars <- header[
      grepl(pattern = "^(Pct_Change)", x = header, ignore.case = TRUE) |
        grepl(pattern = "^(z_score)", x = header, ignore.case = TRUE) |
        grepl(pattern = "^(decibel)", x = header, ignore.case = TRUE)
    ]

    if(!length(vars)) {
      warning("No power data (columns starting with Pct_change, z_score, decibel) in the table: ",
              basename(fpath))
      return(NULL)
    }

    progress$inc(detail = basename(fpath))


    table <- get_table()
    
    
    if("TrialIsOutlier" %in% names(table)) {
      table <- subset(table, !table$TrialIsOutlier)
    }
    table <- data.table::groupingsets(
      table, by = c("Condition", "Time", "Electrode"),
      c(list(count=.N), lapply(.SD, function(x){
        if(!length(x)) { return(NA)}
        if(is.numeric(x)) { return(mean(x)) }
        return(x[[1]])
      })),
      sets = list(c("Condition", "Time", "Electrode")),
      .SDcols = c(header[startsWith(header, "VAR_IS_ROI")], vars),
      id = TRUE
    )

    return(dipsaus::list_to_fastmap2(list(
      project_name = project_name,
      subject_code = subject_code,
      header = header,
      conf = conf,
      table = table
    )))
  })

  # remove NULL values
  meta <- dipsaus::drop_nulls(meta)
  
  if(!length(meta)) { return(NULL) }

  # get headers
  headers <- unique(unlist(lapply(meta, function(item){
    names(item$table)
  })))
  
  table <- data.table::rbindlist(lapply(meta, function(item){
    # item <- meta[[1]]
    nms <- names(item$table)
    nms <- headers[!headers %in% nms]
    # for(nm in nms) {
    #   item$table[[nm]] <- NA
    # }
    item$table$Subject <- item$subject_code
    item$table$Electrode <- as.character(item$table$Electrode)
    item$table$Condition <- as.character(item$table$Condition)
    item$table
  }), use.names = TRUE, fill = TRUE)
  
  if(!nrow(table)) { return(NULL) }
  
  subjects = sapply(meta, '[[', 'subject_code')
  confs = lapply(meta, '[[', 'conf')
  names(confs) = subjects
  
  res = list(
    data = table,
    subjects = subjects,
    confs = confs,
    headers = headers
  )
  
  
}



# 
# # internally used to generate filearray based on table
# filearray_from_table <- function(
#   filebase, table, columns, conditions
# ) {
# 
#   # # DEBUG
#   # columns <- 'Pct_Change_Power_Trial_Onset'
#   # # End: DEBUG
# 
#   margin_names <- c("Time", "Electrode", "Trial")
#   if(!all(c(margin_names, columns) %in% names(table))) {
#     stop("Invalid column name(s) found in function `filearray_from_table`")
#   }
# 
# 
#   time <- unique(table, by = "Time")[["Time"]]
#   electrode_table <- unique(table, by = "Electrode")
#   nms <- names(electrode_table)
#   nms <- nms[!nms %in% c("Trial", "Time", "TrialIsOutlier", "Project", "Subject", "Condition", "ConditionGroup", columns)]
#   electrode_table <- electrode_table[, nms, with = FALSE]
# 
#   electrode <- electrode_table[["Electrode"]]
#   trial_table <- unique(table, by = "Trial")
#   trial <- trial_table[["Trial"]]
#   if("TrialIsOutlier" %in% names(trial_table)) {
#     trial_outlier <- trial_table$TrialIsOutlier
#   } else {
#     trial_outlier <- rep(FALSE, length(trial))
#   }
# 
#   if(!is.null(conditions)) {
#     conditions <- sapply(trial, function(x){
#       conditions$Condition[conditions$Trial == as.character(x)][[1]]
#     })
#   }
# 
#   dm <- c(length(time), length(electrode), length(trial), length(columns))
# 
# 
#   # save to filebase
#   if(dir.exists(filebase)) {
#     unlink(filebase, recursive = TRUE, force = TRUE)
#   }
#   arr <- filearray::filearray_create(
#     filebase = filebase,
#     dimension = dm,
#     type = "float",
#     partition_size = 1L,
#     initialize = FALSE
#   )
#   for(ii in seq_along(columns)) {
#     arr[,,,ii] <- table[[columns[[ii]]]]
#   }
#   dimnames(arr) <- list(
#     Time = time,
#     Electrode = electrode,
#     Trial = trial,
#     Variable = columns
#   )
#   arr$.header$electrode_table <- as.data.frame(electrode_table)
#   arr$.header$trial_outlier <- trial_outlier
#   arr$set_header("conditions", conditions)
#   arr
# }
# 
# 
# import_configurations <- function(search_paths, file_names){
# 
#   # # DEBUG
#   # search_paths <- '~/rave_data/data_dir/demo/_project_data/power_explorer/exports/'
#   # file_names <- c('YCK_pow_by_cond-20211214-141952.fst', 'YAK_MDM-20191105-162604.csv')
#   # fpath <- file_names[[1]]
#   # # End: DEBUG
# 
#   progress <- dipsaus::progress2("Importing data", max = length(file_names) * 3, shiny_auto_close = TRUE)
# 
#   meta <- lapply(file_names, function(fpath){
#     fpath <- find_source(search_paths, fpath)
#     if( is.null(fpath) ){ return(NULL) }
#     if(endsWith(tolower(fpath), suffix = "fst")) {
#       sample <- fst::read_fst(fpath, from = 1, to = 1)
#       header <- names(sample)
#     } else if (endsWith(tolower(fpath), suffix = "csv")) {
#       header <- unname(unlist(utils::read.csv(fpath, header = FALSE, nrows = 1)))
#       sample <- utils::read.csv(fpath, header = TRUE, nrows = 1)
#       names(sample) <- header
#     } else {
#       return(NULL)
#     }
# 
# 
#     project_name <- sample$Project
#     subject_code <- sample$Subject
# 
#     # check if project_name and subject_code are valid
#     if(length(project_name) != 1 || is.na(project_name) || !nchar(trimws(project_name))) {
#       warning("Invalid project name: ", basename(fpath))
#       return(NULL)
#     }
#     if(length(subject_code) != 1 || is.na(subject_code) || !nchar(trimws(subject_code))) {
#       warning("Invalid subject code: ", basename(fpath))
#       return(NULL)
#     }
# 
#     if(!all(c("Time", "Trial", "Electrode") %in% header)) {
#       warning("One or more columns (`Time`, `Trial`, `Electrode`) are missing from table ", basename(fpath))
#       return(NULL)
#     }
# 
#     # try to load headers as well
#     yaml_path <- paste0(fpath, ".yaml")
#     conf <- list()
#     if(file.exists(yaml_path)) {
#       conf <- try({ raveio::load_yaml(yaml_path) }, silent = TRUE)
#     }
# 
#     # get variables to be stored as file-arrays
#     vars <- header[
#       grepl(pattern = "^(Pct_Change)", x = header, ignore.case = TRUE) |
#         grepl(pattern = "^(z_score)", x = header, ignore.case = TRUE) |
#         grepl(pattern = "^(decibel)", x = header, ignore.case = TRUE)
#     ]
# 
#     if(!length(vars)) {
#       warning("No power data (columns starting with Pct_change, z_score, decibel) in the table: ",
#               basename(fpath))
#       return(NULL)
#     }
# 
#     progress$inc(detail = basename(fpath))
# 
#     filebase <- gsub("\\.(fst|csv)$", "", fpath)
#     arr <- tryCatch({
#       # try to load existing file array
#       progress$inc(detail = "Trying to load from previous cache")
#       arr <- filearray::filearray_checkload(
#         filebase = filebase, mode = "readonly", symlink_ok = FALSE,
#         project_name = project_name, subject_code = subject_code,
#         header_names = vars, file_name = basename(fpath),
#         rave_module = "power_explorer", ready = TRUE
#       )
#       progress$inc(detail = "Table loaded")
#       arr
#     }, error = function(e){
#       if(dir.exists(filebase)) {
#         unlink(filebase, recursive = TRUE, force = TRUE)
#       }
# 
#       progress$inc(detail = "Reading from fst files")
#       if(endsWith(tolower(fpath), suffix = "fst")) {
#         table <- fst::read_fst(fpath)
#       } else {
#         table <- data.table::fread(fpath, header = TRUE)
#         names(table) <- header
#       }
#       table <- data.table::setorder(table, "Trial", "Electrode", "Time")
# 
#       if("Condition" %in% names(table)) {
#         cond <- unique(table, by = c("Trial", "Condition"))
#       } else {
#         cond <- NULL
#       }
# 
#       progress$inc(detail = "Reshaping the table")
#       arr <- filearray_from_table(filebase, table, vars, cond)
# 
#       # set headers
#       arr$.header$project_name <- project_name
#       arr$.header$subject_code <- subject_code
#       arr$.header$header_names <- vars
#       arr$.header$file_name <- basename(fpath)
#       arr$.header$rave_module <- "power_explorer"
# 
#       arr$.header$full_header <- header
#       arr$.header$conf <- as.list(conf)
# 
#       arr$set_header(key = "ready", value = TRUE)
#       arr$.mode <- "readonly"
#       arr
#     })
# 
#     return(arr)
#   })
# 
#   # remove NULL values
#   meta <- dipsaus::drop_nulls(meta)
# 
#   # get headers
#   headers <- unique(unlist(lapply(meta, '[[', 'header')))
# }
# 

