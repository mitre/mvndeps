#' Find maven on the system
#' 
#' The package uses maven to resolve dependencies. It does not come packaged with
#' maven. As such it is required that the user independently install maven. This
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
    mvn_dirs <- suppressWarnings(system("which mvn", intern=TRUE, ignore.stderr=TRUE))
    if (length(mvn_dirs)==0 || grepl("^which: no mvn", mvn_dirs)) {
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
  
  if (!is_java_available())
    warning(paste("Maven is found, but java is unavailable. See ?set_java_home for help."))
  
  return(mvn)
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