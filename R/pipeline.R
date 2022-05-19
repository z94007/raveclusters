#' @export
ravecluster_default_opts <- function(){
  original_path <- system.file("pipelines", pipeline_name, package = "raveclusters")
  raveio::pipeline_settings_get(pipeline_settings_path = file.path(original_path, "settings.yaml"))
}

#' @export
ravecluster <- function(names, ..., 
                        .scheduler = "none", .type = "smart",
                        .workdir = tempdir(check = TRUE),
                        .verbose_level = c("verbose", "silent", "summary", "timestamp"),
                        .upgrade = FALSE, .values = TRUE) {
  
  if(!dir.exists(.workdir)) {
    stop("Invalid working directory: the path does not exists.")
  }
  .verbose_level <- match.arg(.verbose_level)
  
  pipeline_name <- "rave-builtin-cluster"
  original_path <- system.file("pipelines", pipeline_name, package = "raveclusters")
  desc <- raveio::pipeline_description(file.path(original_path, "DESCRIPTION"))
  
  pipeline_path <- file.path(.workdir, pipeline_name, desc$Version)
  
  .workdir <- raveio::dir_create2(.workdir)
  cwd <- getwd()
  setwd(.workdir)
  on.exit({ setwd(cwd) }, add = TRUE, after = TRUE)
  
  if(.upgrade || !dir.exists(pipeline_path)) {
    if(dir.exists(pipeline_path)) {
      unlink(pipeline_path, recursive = TRUE)
    }
    raveio::pipeline_install_local(
      src = original_path, to = "workdir", 
      upgrade = FALSE, force = FALSE)
    raveio::pipeline_build(pipe_dir = pipeline_path)
  }
  
  # get current options
  args <- list(...)
  nms <- attr(args, "names")
  if(length(nms)) {
    nms <- nms[!nms %in% ""]
    if(length(nms)) {
      args <- args[nms]
      settings_path <- file.path(pipeline_path, "settings.yaml")
      settings <- raveio::load_yaml(settings_path)
      dipsaus::list_to_fastmap2(args, map = settings)
      raveio::save_yaml(settings, settings_path)
    }
  }
  
  
  all_names <- raveio::pipeline_target_names(pipe_dir = pipeline_path)
  if(missing(names) || !length(names)) {
    names <- NULL
  } else {
    names <- names[names %in% all_names]
  }
  
  values <- dipsaus::fastmap2()
  
  raveio::pipeline_run_bare(pipe_dir = pipeline_path, scheduler = .scheduler, type = .type, envir = new.env(parent = globalenv()), names = names, reporter = .verbose_level)
  
}


