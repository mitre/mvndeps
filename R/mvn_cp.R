#' Get a java classpath
#' 
#' This function requires a simple pom.xml to function, so its usability is limited. However it can
#' be very handy for packages with java dependencies where there is not a fat jar to depend upon.
#' 
#' @param path Character. Path to the directory containing a pom.xml to derive paths for
#' @param mvn Character. The path the the maven installation.
#' @param java_home Character. Path to java. If not provided the standard install paths
#'   (platform dependent) will be checked.
#'   
#' @export
get_classpath_from_pom <- function(path, mvn=find_mvn(), java_home) {
  
  # mvn system checks
  if (!missing(java_home))
    set_java_home(java_home)
  check_mvn_settings_xml(mvn=mvn)
  
  # normalize path - bad things happen when, for example, a path starts with "~"
  path <- normalizePath(path, winslash="/", mustWork=TRUE)
  
  # get classpath
  cmd <- paste(mvn, "dependency:build-classpath -f ", path)
  res <- system(cmd, intern=TRUE)
  
  # report errors
  if (any(grepl("^\\[ERROR\\]", res)))
    stop(paste0("Unable to get classpath from inputs. Maven log follows:\n", paste0(res, collapse="\n")))
  
  return(res[!grepl("^\\[INFO\\]", res)])
}
