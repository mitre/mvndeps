# .datatable.aware=TRUE

.onLoad <- function(libname, pkgname) {  
  assign("pkg_globals", new.env(), envir=parent.env(environment()))
}

set_var <- function(varstr, val) {

  # cache value
  assign(varstr, val, pkg_globals)
}

is_cached_var <- function(varstr) {
  return(!is.null(get_var(varstr)))
}

get_var <- function(varstr) {
  res <- try(get(varstr, pkg_globals), silent=TRUE)
  if(inherits(res, "try-error")){
    return(NULL)
  } else {
    return(res)
  }
}

get_var_setter <- function(varstr) {
  return(get_var(paste0(varstr, ".setter")))
}

# internal function to check null in getOption and return user setting
# or package-specified default as appropriate
get_default <- function(option_name, default_value) {
  value <- getOption(option_name)
  if (!is.null(value))
    return(value)
  return(default_value)
}

is_windows <- function() {
  return(.Platform$OS.type=='windows')
}

is_unix <- function() {
  return(.Platform$OS.type=='unix')
}

is_unsupported_os <- function() {
  return(!(is_windows() | is_unix()))
}

#' The mvndeps Package
#' 
#' A package to interface with maven in handy ways.
#' 
#' @section Inteded Use:
#' 
#' This package is intendend for use by other packages. The idea is to absolve other packages from
#' having to manage java dependencies (i.e., include compiled jars in repositories) if the following
#' constraints can be met:
#' 
#' \itemize{
#'   \item{A reasonable expectation may be made that any user of the package will have maven installed
#'         and configured on their system. This package does not ship with maven, but uses maven system
#'         commands on the assumption that those are available.}
#'   \item{The java dependency is available in a maven repository that any user of the package is
#'         likely to have configured.}
#' }
"_PACKAGE"