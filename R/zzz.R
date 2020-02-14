utils::globalVariables(".")

#' @importFrom purrr %||%
.onLoad <- function(libname, pkgname) {
}

is_windows <- function() {
  return(identical(.Platform$OS.type, "windows"))
}

is_unix <- function() {
  return(identical(.Platform$OS.type, "unix"))
}
