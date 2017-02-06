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
