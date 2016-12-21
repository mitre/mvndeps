
#' Have maven download a dependency
#' 
#' This function will use the maven dependency get funtionality to download a (java)
#' dependency. 
#'  
#' @param dep Character. The dependency name. This could be the entire name (e.g.,
#'   group:dependency:version) in which case the other inputs may be omitted. Otherwise
#'   it can be just the dependency name (i.e., the part in the middle of the above example).
#' @param group Character. The maven artifact group name.
#' @param version Character. The desired version of the dependency.
#' @param mvn Character. The path the the maven installation.
#' @param m2 Character. Path to local maven repository. If no input is provided a standard
#'   location will be guessed at (platform dependent).
#' @export
download_dependency <- function(dep, group, version, mvn=find_mvn(), java_home) {
  
  set_java_home(java_home)

  if (!missing(group))
    dep <- paste0(group, ":", dep)
  
  if (!missing(version))
    dep <- paste0(dep, ":", version)
  
  system(paste0(mvn, " dependency:get -Dartifact=", dep))
}

#' Have maven find a dependency
#' 
#' This function will find a maven dependency in your local maven repository. Note that
#' if the dependency has not yet been downloaded (e.g., via \code{\link{download_dependency}})
#' then the return value will be \code{NULL}.
#'  
#' @inheritParams download_dependency
#' @param quiet Logical. If \code{TRUE} warnings about missing dependencies will be suppressed.
#' @seealso download_dependency
#' @export
find_dependency_path <- function(dep, group, version, mvn=find_mvn(), java_home, quiet=FALSE) {
  
  set_java_home(java_home)

  # put dependency together
  if (!missing(group))
    dep <- paste0(group, ":", dep)
  
  if (!missing(version))
    dep <- paste0(dep, ":", version)
  
  # pull dependency apart. this is necessary to support use cases where the group and
  # version inputs are missing
  dep_parts <- unlist(strsplit(dep, ":"))
  dep_parts[1:length(dep_parts)-1] <- gsub("\\.", "/", dep_parts[1:length(dep_parts)-1])
  path <- paste0(find_local_mvn_repo(mvn), "/", paste0(dep_parts, collapse="/"))
  if (!dir.exists(path)) {
    if (!quiet)
      warning(paste("Dependency not found in", path))
    return(NULL)
  }
  return(path)
}

#' @rdname find_dependency_path
#' @export
find_dependency_jar <- function(dep, group, version, mvn=find_mvn(), java_home, quiet=FALSE) {
 
  # put dependency together
  if (!missing(group))
    dep <- paste0(group, ":", dep)
  
  if (!missing(version))
    dep <- paste0(dep, ":", version)
  
  # find path
  path <- find_dependency_path(dep=dep, mvn=mvn, java_home=java_home, quiet=quiet)
  
  # get jar name by pealing off group
  dep_parts <- unlist(strsplit(dep, ":"))
  if (length(dep_parts) != 3)
    stop(paste0("Dependency (", dep, ") does not follow expected group:name:version pattern."))
  jar_name <- paste0(dep_parts[2], "-", dep_parts[3], ".jar")
  jar_path <- file.path(path, jar_name)
  
  # find jar
  if (!file.exists(jar_path))
    stop(paste0("The jar '", jar_name, "' was not found in the expected location '", path, "'."))
  
  return(jar_path)
}

#' Have maven get a dependency
#' 
#' This function wraps \code{\link{download_dependency}} and \code{link{find_dependency}}. 
#' It will look to see if the dependency is already available in the local maven repository.
#' If it is it will return the path. If it isn't then it will download the dependency and then
#' return the path.
#' 
#' @inheritParams download_dependency
#' @export
get_dependency_path <- function(dep, group, version, mvn=find_mvn(), java_home) {
  path <- find_dependency_path(dep=dep, group=group, version=version, mvn=mvn, java_home=java_home, quiet=TRUE)
  if (!is.null(path))
    return(path)
  
  download_dependency(dep=dep, group=group, version=version, mvn=mvn, java_home=java_home)
  path <- find_dependency_path(dep=dep, group=group, version=version, mvn=mvn, java_home=java_home, quiet=FALSE)
  return(path)
}
