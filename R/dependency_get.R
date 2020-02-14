#' Force Download a Java Dependency
#'
#' This function will ensure that the specified java dependency is available in the user's local maven
#' repository. It will not resolve the classpath in the sense of returning a list of jars to be added
#' to the java classpath. For that use case see \code{\link{build_classpath}}.
#'
#' Under the hood this is using the maven \code{dependency:get} plugin. See
#' https://maven.apache.org/plugins/maven-dependency-plugin/get-mojo.html for details. One consequence
#' of this is that each artifact in the input must be downloaded separately. However common transitive
#' dependencies will not be redownloaded as maven will detect that a given transitive dependency is already
#' available when resolving the second main dependency.
#'
#' @param artifacts Character vector. Should be of the form 'groupId:artifactId:version[:packaging[:classifier]]'
#'   where the packaging and classifier inputs are optional.
#' @param transitive Logical. Should transitive dependencies (i.e., dependencies of the dependency) be retrieved?
#' @param remote_repos Character vector. A vector of repositories to look in for the artifact. These should be
#'   of the form 'id::[layout]::url', or simply using the URL is ok. For example, for maven central you may use
#'   'central::default::http://repo1.maven.apache.org/maven2'. If missing the repositories configured in the user's
#'   \code{settings.xml} will be used.
#' @param dep_plugin_version Character. The version of the maven dependency plugin. See
#'   https://github.com/apache/maven-dependency-plugin/releases for release versions. If nothing is provided then
#'   a default version is chosen (may change over time with releases of this package).
#' @param verbose Logical. Dump maven output to console.
#'
#' @importFrom purrr %>% map map2 map_int pluck
#' @return an integer vector of status codes, one for each artifact listed. Status code 0 indicates that the artifact
#'   was successfully retrieved.
#' @export
get_dependencies <- function(artifacts,
                             transitive = TRUE,
                             remote_repos = NULL,
                             dep_plugin_version = NULL,
                             verbose = FALSE) {

  if (is.null(dep_plugin_version))
    dep_plugin_version <- .globals$dep_plugin_version

  results <- artifacts %>%
    map(.dependency_get_args,
        transitive = transitive,
        remote_repos = remote_repos,
        dep_plugin_version = dep_plugin_version) %>%
    map(execute_mvn_cmd)


  # even if verbose = FALSE I still want to print messages for non-zero exit status
  map2(artifacts, results, print_get_dependencies_messages, verbose = verbose)

  results %>%
    map_int(~ pluck(.x, "status")) %>%
    return()
}

#' Build the command to have maven download a dependency (package-private)
#'
#' This function will build a system command using the maven dependency get funtionality to download a (java)
#' dependency.
#'
#' @keywords internal
.dependency_get_args <- function(artifact, transitive = TRUE, remote_repos = NULL, dep_plugin_version) {

  mvn_dep_plugin <- paste0("org.apache.maven.plugins:maven-dependency-plugin:", dep_plugin_version)
  args <- c(paste0(mvn_dep_plugin, ":get"),
            paste0("-Dartifact=", artifact),
            paste0("-Dtransitive=", ifelse(transitive,'true','false')))

  if (!is.null(remote_repos)) {
    remotes <- paste(remote_repos, collapse = ",")
    args <- c(args, paste0("-DremoteRepositories=", remotes))
  }

  return(args)
}

print_get_dependencies_messages <- function(artifact, sys_res, verbose) {

  if (sys_res$status != 0) {
    message(paste0("Unable to retrieve '", artifact, "' (status code ", sys_res$status, ")"))
    print_messages(sys_res)
  } else if (verbose) {
    message(paste0("Retrieved '", artifact, "' with the following maven output:"))
    print_messages(sys_res)
  }
}

#' Have maven find a dependency
#'
#' This function will find a maven dependency in your local maven repository. Note that
#' if the dependency has not yet been downloaded (e.g., via \code{\link{download_dependency}})
#' then the return value will be \code{NULL}.
#'
#' @inheritParams get_dependencies
#' @seealso \link{get_dependencies}
#' @importFrom purrr %>% map_chr
#' @export
find_artifacts <- function(artifacts, verbose = FALSE) {

  artifacts %>%
    map_chr(find_artifact, verbose = verbose) %>%
    return()
}

find_artifact <- function(artrifact, verbose) {
  # pull dependency apart. this is necessary to support use cases where the group and
  # version inputs are missing
  parsed_dep <- parse_dependency(artrifact)
  parsed_dep[c("groupid")] <- gsub("\\.", "/", parsed_dep[c("groupid")])
  path <- paste0(find_local_mvn_repo(), "/", paste0(parsed_dep, collapse="/"))
  if (!dir.exists(path)) {
    if (verbose)
      warning(paste("Dependency not found in", path))
    path <- NULL
  }
  # unconfigure_mvndeps(quiet=quiet)
  artifact_name <- paste0(parsed_dep["artifactid"], "-", parsed_dep["version"], ".jar")
  return(file.path(path, artifact_name))
}

concatenate_dependency <- function(dep, group, version) {
  if (!missing(group))
    dep <- paste0(group, ":", dep)

  if (!missing(version))
    dep <- paste0(dep, ":", version)

  return(dep)
}

# intended for 1 dependency at a time, not vectorized
parse_dependency <- function(dep, group, version) {
  if (!missing(group) && !missing(version))
    return(c(groupid=group, artifactid=dep, version=version))
  else if (!missing(group) || !missing(version))
    dep <- concatenate_dependency(dep, group, version)

  # parse deps out
  dep_parts <- unlist(strsplit(dep, ":"))
  if (length(dep_parts) != 3)
    stop(paste0("Dependency (", dep, ") does not follow expected group:name:version pattern."))

  return(c(groupid=dep_parts[1], artifactid=dep_parts[2], version=dep_parts[3]))
}
