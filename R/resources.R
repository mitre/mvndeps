#' Find maven on the system
#' 
#' The package uses maven to resolve dependencies. It does not come packaged with
#' maven. As such it is required thta the user independently install maven. This
#' function will try to find the maven executable assuming that maven is already
#' installed.
#' 
#' @export
find_mvn <- function() {
  
  mvn <- get_var(".mvn.executable")
  if (!is.null(mvn))
    return(mvn)
  
  value <- getOption(".mvn.executable")
  if (!is.null(value)) {
    # intentionally not setting local .mvn.executable so that power user can swith quickly
    return(value)
  }
  if (is_windows()) {
    standard_path <- "C:/maven"
    mvn_dirs <- dir(standard_path, full.names=TRUE)
    
  } else if (is_unix()) {
    mvn_dirs <- system("which mvn", intern=TRUE)
    if (grepl("^which: no mvn", mvn_dir)) {
      standard_path <- "/usr/depot"
      mvn_dirs <- dir(standard_path, full.names=TRUE)
    }
  } else {
    stop("Unsupported operating system.")
  }
  
  mvn_dirs <- mvn_dirs[grepl("^apache-maven", basename(mvn_dirs))]
  if (length(mvn_dirs)==0)
    stop("Unable to find mvn. Try setting options(.mvn.executable=...)")
  mvn_dir <- sort(mvn_dirs, decreasing=TRUE)[1]
  mvn <- file.path(mvn_dir, "bin", "mvn")
  set_var(".mvn.executable", mvn)
  return(mvn)
}

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
#' @return Logical. \code{TRUE} if JAVA_HOME was set and \code{FALSE} otherwise.
#' @export
set_java_home <- function(java_home) {
  
  # use what is provided (will trigger return in next block)
  if (!missing(java_home)) 
    Sys.setenv(JAVA_HOME=java_home)
  
  # don't worry about things if it is already set
  if (!is.null(Sys.getenv("JAVA_HOME")))
    return(TRUE)
      
  # now we need to go find it
  java_home <- getOption(".java.home")
  if (!is.null(java_home)) {
    Sys.setenv(JAVA_HOME=java_home)
    return(TRUE)
  }
  
  if (is_windows()) {
    standard_path <- "C:/Program Files/Java/"
    java_dirs <- dir(standard_path, full.names=TRUE)
    
  } else if (is_unix()) {
    java_dirs <- system("which java", intern=TRUE)
    if (grepl("^which: no mvn", java_dir)) {
      standard_path <- "/usr/bin"
      java_dirs <- dir(standard_path, full.names=TRUE)
    }
    
  } else {
    warning("Unsupported operating system.")
    return(FALSE)
  }
  
  java_dirs <- java_dirs[grepl("^jre|java", basename(java_dirs))]
  if (length(mvn_dirs)==0) {
    warning("Unable to find java. Try setting options(.java.home=...)")
    return(FALSE)
  }
  
  java_dir <- sort(java_dirs, decreasing=TRUE)[1]
  Sys.setenv(JAVA_HOME=java_dir)
  return(TRUE)
}

#' @rdname set_java_home
#' @export
clear_java_home <- function() {
  Sys.setenv(JAVA_HOME="")
}

#' Find your local maven repository
#' 
#' This is primarily intended for internal use. It may however be useful for some to have
#' a programatic way of identifying the local maven reposiory.
#' 
#' @export
find_local_mvn_repo <- function(mvn=find_mvn()) {
  
  # report back cahced value if known
  m2repo <- get_var(".mvn.repository")
  if (!is.null(m2repo))
    return(m2repo)
  
  # final local repo
  cmd <- paste(mvn, "help:evaluate -Dexpression=settings.localRepository")
  res <- system(cmd, intern=TRUE)
  m2repo <- res[!grepl("[", res, fixed=TRUE)]
  if (length(m2repo)==0)
    stop("Unable to find local maven repository. Ensure maven is properly installed and found.")
  
  # fix path on windows to use forward slashes (what R expects)
  m2repo <- gsub("\\\\", "/", m2repo)
  
  # cache & return
  set_var(".mvn.repository", m2repo)
  return(m2repo)
}

get_mvn_settings <- function(mvn=find_mvn()) {
  cmd <- paste(mvn, "help:effective-settings")
  settings <- system(cmd, intern=TRUE)
  return(settings[!grepl("\\[", settings, fixed=TRUE)])
}