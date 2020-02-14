.globals <- new.env(parent = emptyenv())
.globals$which_mvn <- getOption("maven.path")
.globals$remote_repos <- getOption("maven.remote.repos")
.globals$local_repo <- getOption("maven.local.repo")
.globals$dep_plugin_version <- getOption("maven.dependency.plugin.version") %||% "3.1.1"


#' Set/Append Maven Artifact Repositories
#'
#' \code{set_mvn_repos} will overwrite any existing repos and
#' \code{add_mvn_repos} will append/prepend to existing.
#'
#' These repositories are only used by the \code{dependency:get} maven plugin, so they
#' will be used when calling \code{\link{get_dependencies}} but not when calling
#' \code{\link{build_classpath}} (which uses the \code{dependency:build-classpath} maven
#' plugin under the hood). To set repositories for building classpaths it is necessary
#' to update the maven settings.xml file. This is typically found in \code{~/.m2/settings.xml}.
#'
#' @param repos Character vector. Should be maven repo URLs
#' @param prepend Logical. If \code{TRUE} the new paths will come before the old.
#' @export
set_mvn_repos <- function(repos) {
  .globals$mvn_repos <- paste(repos, collapse = ",")
}

#' @rdname set_mvn_repos
#' @export
add_mvn_repos <- function(repos, prepend = FALSE) {
  all_repos <- if (prepend) {
    c(repos, .globals$mvn_repos)
  } else {
    c(.globals$mvn_repos, repos)
  }
  set_mvn_repos(all_repos)
}

