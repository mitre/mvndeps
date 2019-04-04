#' Java for Maven
#'
#' Maven needs java to run. In this case it means that java needs to be available
#' for maven system commands.
#'
#' @section set/clear JAVA_HOME:
#'
#' Maven needs to know where java is to run. Setting the JAVA_HOME
#' environment variable can however cause issues with \code{rJava}. If
#' issues do arise use \code{clear_java_home} after your dependencies are
#' resolved.
#'
#' @param java_home Character. Indicates location of java on the system. If
#'     not provided then the function will attempt to find java using standard
#'     install paths.
#' @param quiet Logical. If \code{FALSE} the new JAVA_HOME setting will be printed.
#' @export
set_java_home <- function(java_home=find_java(), quiet=FALSE) {

  Sys.setenv(JAVA_HOME=java_home)

  if (!quiet)
    message(paste("JAVA_HOME now set to", java_home))
}

#' @section find java:
#'
#' Find a JRE/JDK on the system. This is a convenience function to find java
#' on the system. It will use \code{getOption(".java.home")} if that is found.
#' Otherwise it will look in standard install locations depending on the
#' system platform.
#'
#' @rdname set_java_home
#' @export
find_java <- function() {

  # defer to set option if it is found
  java_home <- getOption("JAVA_HOME")
  if (!is.null(java_home) && dir.exists(java_home)) {
    return(java_home)
  }
  java_home <- NULL

  # no option set, start looking
  if (is_windows()) {
    standard_path <- "C:/Program Files/Java/"
    java_dirs <- dir(standard_path, full.names=TRUE)

  } else if (is_unix()) {
    java_dirs <- suppressWarnings(system("which java", intern=TRUE, ignore.stderr=TRUE))
    if (length(java_dirs)==0 || grepl("^which: no java", java_dirs)) {
      standard_path <- "/usr/lib"
      java_dirs <- dir(standard_path, full.names=TRUE)
    } else {
      java_home <- gsub("/bin/java$", "", java_dirs)
    }

  } else {
    warning("Unsupported operating system.")
    return("")
  }

  if (is.null(java_home)) {
    java_dirs <- java_dirs[grepl("^jre|jdk", basename(java_dirs))]
    if (length(java_dirs)==0) {
      warning("Unable to find java. Use set_java_home with full path input.")
      return("")
    }

    java_home <- sort(java_dirs, decreasing=TRUE)[1]
  }

  return(java_home)
}

#' @rdname set_java_home
#' @export
clear_java_home <- function(quiet=FALSE) {
  Sys.unsetenv("JAVA_HOME")
  if (!quiet)
    message("JAVA_HOME is now unset")

}
