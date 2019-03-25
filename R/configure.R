.globals <- new.env(parent = emptyenv())
.globals$which_mvn <- getOption("maven.path")
.globals$remote_repos <- getOption("maven.remote.repos")
.globals$local_repo <- getOption("maven.local.repo")
.globals$dep_plugin_version <- getOption("maven.dependency.plugin.version") %||% "3.1.1"

configure_mvndeps <- function(mvn, java_home, quiet=FALSE) {
  # if (!missing(java_home))
  #   set_java_home(java_home, quiet=quiet)
  # if (Sys.getenv("JAVA_HOME")=="") {
  #   set_java_home(quiet=quiet)
  #   set_var("temp.java.home", TRUE)
  # } else {
  #   set_var("temp.java.home", FALSE)
  # }
}

unconfigure_mvndeps <- function(quiet=FALSE) {
  # if (get_var("temp.java.home")==TRUE) {
  #   clear_java_home(quiet=quiet)
  #   set_var("temp.java.home", FALSE)
  # }
}


#' Set/Append Maven Artifact Repositories
#'
#' \code{set_mvn_repos} will overwrite any existing repos and
#' \code{add_mvn_repos} will append/prepend to existing.
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

