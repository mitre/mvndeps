#' Set/clear JAVA_HOME
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

#' @rdname set_java_home
#' @export
find_java <- function() {
  
  # defer to set option if it is found
  java_home <- getOption(".java.home")
  if (!is.null(java_home) && dir.exists(find_java)) {
    return(java_home)
  }
  
  # no option set, start looking
  if (is_windows()) {
    standard_path <- "C:/Program Files/Java/"
    java_dirs <- dir(standard_path, full.names=TRUE)
    
  } else if (is_unix()) {
    java_dirs <- suppressWarnings(system("which java", intern=TRUE, ignore.stderr=TRUE))
    if (length(java_dirs)==0 || grepl("^which: no java", java_dirs)) {
      standard_path <- "/usr/bin"
      java_dirs <- dir(standard_path, full.names=TRUE)
    }
    
  } else {
    warning("Unsupported operating system.")
    return("")
  }
  
  java_dirs <- java_dirs[grepl("^jre|jdk|java", basename(java_dirs))]
  if (length(java_dirs)==0) {
    warning("Unable to find java. Use set_java_home with full path input.")
    return("")
  }
  
  return(sort(java_dirs, decreasing=TRUE)[1])
}

#' @rdname set_java_home
#' @export
clear_java_home <- function() {
  Sys.setenv(JAVA_HOME="")
}

#' @section java availability:
#' 
#' This function runs \code{mvn -version} to determine if java is on the system path used
#' by maven when executed from R. This test is a little different from, for example, 
#' executing \code{system("java -version")}. There are cases where java may be available
#' for system commands but not specifically set on a system path and hence unavailable for
#' use by maven.
#' 
#' @rdname set_java_home
#' @export
is_java_available <- function() {
  suppressWarnings(result <- system(paste(find_mvn(), "-version"), intern=TRUE))
  if (any(grepl("Error: JAVA_HOME not found", result)))
    return(FALSE)
  
  return(TRUE)
}