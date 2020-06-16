# Imports from other packages
# There are two ways to import
# First method is to import the entire package by @import pkg
# Second is to import a specific function @importFrom pkg function
#
# If your package depends heavily on one package, use the first one
# otherwise, it's recommended to use the second method to avoid potential conflicts

# Make sure to declare ALL dependencies here to make sure R can find them.

#' @import shiny
#' @import rlang
#' @import rave
#' @import rutabaga
#' @import dipsaus
#' @import graphics
#' @import Rtsne
#' @importFrom magrittr %>%
#' @import stringr
#' @importFrom magrittr %<>%
#' @importFrom magrittr %$%
#' @importFrom magrittr extract2
#' @importFrom magrittr extract
#' @importFrom magrittr set_rownames
#' @importFrom magrittr equals

NULL

shiny_cex.main = 1.5

#' Function to load all dev funtions and wrap them within an environment
#' @param expose_functions logical indicating whether to expose all dev functions to the global environment
#' @param reload logical, do you want to fast-reload the package before load the functions?
#' @export
dev_raveclusters <- function(expose_functions = FALSE, reload = TRUE){
  .__rave_context__. = 'rave_module_debug'
  .__rave_package__. = 'raveclusters'
  if(reload){
    env <- rave::reload_module_package(expose_functions)
  }else{
    if(expose_functions){
      env = globalenv()
    }else{
      env = new.env(parent = globalenv())
    }
    rave::load_rave_module_package(env, 'rave_module_debug')
  }
  rave::rave_context(spos = 1L, tenv = globalenv())
  env
}


