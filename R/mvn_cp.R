#' Get a java classpath from dependency list
#' 
#' Given one or more dependencies retrieve a classpath that can be used to set up
#' rJava correctly.
#' 
#' @section On Specifying Dependencies:
#' 
#' The \code{group} and \code{version} arguments are provided mostly for compatibility with
#' \code{\link{find_dependency_path}} and friends. When more than one direct dependency needs
#' to be specified it is probably less confusing (unless the values are coming from a \code{data.frame})
#' to provide a vector of completely specified dependencies in the \code{dep} argument.
#' 
#' @inheritParams find_dependency_path
#' @export
get_classpath <- function(dep, group, version, mvn=find_mvn(), java_home, transitive = TRUE, quiet=FALSE) {

  # if not transitive, then this is a synonym for just finding the file location of the
  # specified dependency
  if (!transitive)
    return(find_dependency_path(dep=dep, group=group, version=version, mvn=mvn,
                                java_home=java_home, quiet=quiet))
  
  # write pom
  pom <- write_pom(dep, group, version)
  
  # now that a pom exists we can use get_classpath_from_pom
  cp <- get_classpath_from_pom(path=pom, mvn=mvn, java_home=java_home, quiet=quiet)
  
  # cleanup and return
  unlink(pom)
  return(cp)
}

#' Get a java classpath from POM
#' 
#' This function requires a simple pom.xml to function, so its usability is limited. However it can
#' be very handy for packages with java dependencies where there is not a fat jar to depend upon.
#' If you have a list of dependencies but no actual pom.xml, see \code{\link{get_classpath}}
#' 
#' @param path Character. Path to the directory containing a pom.xml to derive paths for
#' @param mvn Character. The path the the maven installation.
#' @param java_home Character. Path to java. If not provided the standard install paths
#'   (platform dependent) will be checked.
#'   
#' @export
get_classpath_from_pom <- function(path, mvn=find_mvn(), java_home, quiet=FALSE) {
  
  configure_mvndeps(mvn=mvn, java_home=java_home, quiet=quiet)
  
  # normalize path - bad things happen when, for example, a path starts with "~"
  path <- normalizePath(path, winslash="/", mustWork=TRUE)
  
  # get classpath
  cmd <- paste(mvn, "dependency:build-classpath -f ", path)
  res <- system(cmd, intern=TRUE)
  
  unconfigure_mvndeps(quiet=quiet)
  return(parse_classpath_from_mvn(res))
}

parse_classpath_from_mvn <- function(res) {
  # report errors
  if (any(grepl("^\\[ERROR\\]", res)))
    stop(paste0("Unable to get classpath from inputs. Maven log follows:\n", paste0(res, collapse="\n")))
  
  # filter results
  cp <- res[!grepl("^\\[|Downloading", res)]
  
  # get only the last entry to get rid of some downloading status messages if present
  cp <- cp[length(cp)]
  
  # turn semi-colon separated list into character vector
  cp <- unlist(strsplit(cp, split=";"))
  
  return(cp) 
}
