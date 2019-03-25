#' @rdname build_classpath
#' @importFrom rJava .jinit .jaddClassPath .jclassPath
#' @param ... Arguments passed to \code{\link[rJava]{.jinit}}
augment_classpath <- function(artifacts, pom_path, pkg_name, pom_name = "pom.xml", verbose = FALSE, ...) {

  cp <- build_classpath(artifacts, pom_path, pkg_name = pkg_name, pom_name = pom_name, verbose = verbose)
  .jinit(...)
  .jaddClassPath(cp)
  if (verbose) {
    msg <- paste("Java class path is now:\n\t", paste(.jclassPath(), collapse = "\n\t"))
    message(msg)
  }
}

#' Get a java classpath from dependency list
#'
#' Given one or more dependencies retrieve a classpath that can be used to set up
#' rJava correctly.
#'
#' @section On Specifying Dependencies:
#'
#' Resolve the java classpath from either a vector of dependencies or a pom.xml. Maven will
#' attempt to download any dependencies (direct or transitive) that are not found in the
#' local maven repository.
#'
#' The \code{deps}, \code{pom_path}, and \code{pkgname} inputs are mutually exclusive.
#' Use exactly one or an error will result.
#'
#' \code{build_classpath} will return a character vector of jars to be added to the classpath
#' but will not actually change the classpath. \code{augment_classpath} will update the java
#' classpath (and initilize the JVM if necessary).
#'
#' There are several parameters available for the Maven dependency:build-classpath
#' plugin. See https://maven.apache.org/plugins/maven-dependency-plugin/build-classpath-mojo.html
#' for details. Only the parameters likely to be of most use in the context of resolving
#' a java classpath for use from R are directly exposed. Any custom parameters beyond these
#' can be entered via the \code{mvn_args} input.
#'
#' @inheritParams get_dependencies
#' @param pom_path Character. Path to a pom.xml to build a classpath from.
#' @param pkg_name Character. Name of a package that has a pom.xml in the inst/java directory.
#' @param  pom_name Character. Name of the pom file (in case of nonstandard names, like mvndeps.xml).
#'   Only used if \code{pkg_name} is provided.
#' @param includeScope Character. Scope to include. An Empty string indicates all scopes. Possible
#'   scopes are "runtime", "compile", "test", "provided", "system"
#' @param local_repo Character. Path to local maven repository. If missing then the default location
#'   or the location specified in a user's settings.xml will be used.
#' @param absolute_paths Logical. Output absolute filename for resolved artifacts.
#' @param overwrite_if_newer Logical. If \code{TRUE} overwrite artifacts that don't exist or are older than the source.
#' @param overwrite_releases Logical. If \code{TRUE} overwrite release artifacts.
#' @param overwrite_snapshots Logical. If \code{TRUE} overwrite snapshot artifacts.
#' @param mvn_args Character vector. Additional arguments to pass to the Maven dependency:build-classpath
#'   plugin. Should be of the form \code{c("-Dparam1=value1", -Dparam2=value2")}.
#'
#' @importFrom purrr %>%
#' @export
build_classpath <- function(artifacts, pom_path, pkg_name, pom_name = "pom.xml", dep_plugin_version,
                            include_scope = "runtime", local_repo, absolute_paths = TRUE,
                            overwrite_if_newer = TRUE, overwrite_releases = FALSE, overwrite_snapshots = FALSE,
                            mvn_args, verbose = FALSE) {

  if (sum(c(missing(artifacts), missing(pom_path), missing(pkg_name))) != 2)
    stop("Exactly one of `artifacts`, `pom_path`, and `pkg_name` should be specified. ",
         "These represent alternate, but mutually exclusive, ways of specifying dependencies.")

  check_mvn()

  if (missing(dep_plugin_version))
    dep_plugin_version <- .globals$dep_plugin_version

  # write pom
  if (!missing(artifacts)) {
    pom_path <- write_pom(artifacts)
  } else if (!missing(pkg_name)) {
    pom_path <- system.file("java", pom_name, package = pkg_name)
    if (pom_path == "")
      stop("No ", pom_name, " found in the java directory of the '", pkg_name, "' ",
           "(", system.file("java", package = pkg_name), ")")
  }

  # normalize path - bad things happen when, for example, a path starts with "~"
  pom_path <- normalizePath(pom_path, winslash="/", mustWork = TRUE)
    # mvn args
  mvn_dep_plugin <- paste0("org.apache.maven.plugins:maven-dependency-plugin:", dep_plugin_version)
  args <- c(paste0(mvn_dep_plugin, ":build-classpath"),
            paste("-f", pom_path),
            paste0("-DincludeScope=", include_scope),
            ifelse(missing(local_repo), NA_character_, paste0("-DlocalRepoProperty=", local_repo)),
            paste0("-DoutputAbsoluteArtifactFilename=", tolower(absolute_paths)),
            paste0("-DoverWriteIfNewer=", tolower(overwrite_if_newer)),
            paste0("-DoverWriteReleases=", tolower(overwrite_releases)),
            paste0("-DoverWriteSnapshots=", tolower(overwrite_snapshots))) %>%
    na.omit()
  if (!missing(mvn_args)) {
    args <- c(args, mvn_args)
  }

  # now that a pom exists we can use get_classpath_from_pom
  cp <- get_classpath_from_pom(args = args, cp_file = cp_file, verbose = verbose)

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
#' @inheritParams download_dependency
#'
#' @export
get_classpath_from_pom <- function(args, cp_file, verbose = FALSE) {

  # get classpath
  res <- execute_mvn_cmd(args, use_system2 = TRUE)
  mvn_spew <- rawToChar(res$stdout)

  if (res$status != 0) {
    message(mvn_spew)
    message(rawToChar(res$stderr))
    stop("Failed to build a classpath with status ", res$status)
  }

  if (verbose) {
    message(mvn_spew)
  }

  # unconfigure_mvndeps(quiet=quiet)
  return(parse_classpath_from_mvn(mvn_spew))
}

parse_classpath_from_mvn <- function(res) {

  res <- unlist(strsplit(res, "\\n"))

  # report errors
  if (any(grepl("^\\[ERROR\\]", res)))
    stop(paste0("Unable to get classpath from inputs. Maven log follows:\n", paste0(res, collapse="\n")))

  # filter results
  cp <- res[!grepl("^\\[|Downloading", res)]

  # get only the last entry to get rid of some downloading status messages if present
  cp <- cp[length(cp)]

  # turn semi-colon separated list into character vector
  cp <- unlist(strsplit(cp, split=ifelse(is_windows(), ";", ":")))

  return(cp)
}

#' mvn dependency:build-classpath only works with a pom.xml - so this function writes a pom to make maven happy
#'
#' @noRd
write_pom <- function(deps, path=tempdir(), artifactid="temp-proj",
                      groupid="temp-group", artifactversion="0.0.1-SNAPSHOT") {

  # open connection to write pom
  filepath <- file.path(path, "pom.xml")
  con <- file(filepath, open="wt")

  # header stuff
  writeLines('<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"', con)
  writeLines('xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd">', con)
  writeLines('\t<modelVersion>4.0.0</modelVersion>', con)

  # project info
  writeLines(paste0('\n\t<artifactId>', artifactid, '</artifactId>'), con)
  writeLines(paste0('\t<groupId>', groupid, '</groupId>'), con)
  writeLines(paste0('\t<version>', artifactversion, '</version>'), con)

  # dependencies
  writeLines('\n\t<dependencies>', con)
  for (dep in deps) {
    parsed_dep <- parse_dependency(dep)
    writeLines('\t\t<dependency>', con)
    writeLines(paste0('\t\t\t<groupId>', parsed_dep["groupid"], '</groupId>'), con)
    writeLines(paste0('\t\t\t<artifactId>', parsed_dep["artifactid"], '</artifactId>'), con)
    writeLines(paste0('\t\t\t<version>', parsed_dep["version"], '</version>'), con)
    writeLines('\t\t</dependency>', con)
  }
  writeLines('\t</dependencies>', con)
  writeLines('</project>', con)
  close(con)
  return(filepath)
}

