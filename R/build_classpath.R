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
#' @section Build or Augment a Classpath:
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
#' @section Classpath From a pom.xml:
#'
#' This function requires a simple pom.xml to function, so its usability is limited. However it can
#' be very handy for packages with java dependencies where there is not a fat jar to depend upon.
#' If you have a list of dependencies but no actual pom.xml, see \code{\link{build_classpath}}
#'
#' @inheritParams get_dependencies
#' @param pkg_name Character. Name of a package that has a pom.xml in the inst/java directory. In this case
#'   a \code{pom_path} may be omitted and the pom will be discovered in the specified package.
#'
#' @importFrom purrr %>%
#' @export
build_classpath <- function(artifacts, pom_path, pkg_name, dep_plugin_version,
                            include_scope = "runtime", local_repo, absolute_paths = TRUE,
                            overwrite_if_newer = TRUE, overwrite_releases = FALSE, overwrite_snapshots = FALSE,
                            mvn_args, verbose = FALSE) {

  if (sum(c(missing(artifacts), missing(pom_path), missing(pkg_name))) != 2)
    stop("Exactly one of `artifacts`, `pom_path`, and `pkg_name` should be specified. ",
         "These represent alternate, but mutually exclusive, ways of specifying dependencies.")

  check_mvn()

  # write pom
  unlink_pom <- FALSE # don't delete pom that we didn't just write
  if (!missing(artifacts)) {
    pom_path <- write_pom(artifacts)
    unlink_pom <- TRUE # do delete a temporary pom written by this function
  } else if (!missing(pkg_name)) {
    pom_path <- system.file("java", "pom.xml", package = pkg_name)
    if (pom_path == "")
      stop("No ", pom_name, " found in the java directory of the '", pkg_name, "' ",
           "(", system.file("java", package = pkg_name), ")")
  }

  # now that a pom exists we can use build_classpath_from_pom
  cp <- build_classpath_from_pom(pom_path = pom_path,
                                 include_scope = include_scope,
                                 absolute_paths = absolute_paths,
                                 overwrite_if_newer = overwrite_if_newer,
                                 overwrite_releases = overwrite_releases,
                                 overwrite_snapshots = overwrite_snapshots,
                                 dep_plugin_version = dep_plugin_version,
                                 local_repo = local_repo,
                                 mvn_args = mvn_args,
                                 verbose = verbose)

  # cleanup and return
  if (unlink_pom)
    unlink(pom_path)

  return(cp)
}

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

#' @rdname build_classpath
#' @inheritParams get_dependencies
#' @param pom_path Character. Path to a pom.xml to build a classpath from.
#' @param include_scope Character. Scope to include. An Empty string indicates all scopes. Possible
#'   scopes are \code{c("runtime", "compile", "test", "provided", "system")}.
#' @param absolute_paths Logical. Output absolute filename for resolved artifacts.
#' @param overwrite_if_newer Logical. If \code{TRUE} overwrite artifacts that don't exist or are older than the source.
#' @param overwrite_releases Logical. If \code{TRUE} overwrite release artifacts.
#' @param overwrite_snapshots Logical. If \code{TRUE} overwrite snapshot artifacts.
#' @param local_repo Character. Path to local maven repository. If missing then the default location
#'   or the location specified in a user's settings.xml will be used.
#' @param mvn_args Character vector. Additional arguments to pass to the Maven dependency:build-classpath
#'   plugin. Should be of the form \code{c("-Dparam1=value1", "-Dparam2=value2")}.
#' @export
build_classpath_from_pom <- function(pom_path,
                                     include_scope = "runtime",
                                     absolute_paths = TRUE,
                                     overwrite_if_newer = TRUE,
                                     overwrite_releases = FALSE,
                                     overwrite_snapshots = FALSE,
                                     dep_plugin_version,
                                     mvn_args,
                                     local_repo,
                                     verbose = FALSE) {

  # get system command
  args <- generate_mvn_dependency_get_args(pom_path = pom_path,
                                           include_scope = include_scope,
                                           absolute_paths = absolute_paths,
                                           overwrite_if_newer = overwrite_if_newer,
                                           overwrite_releases = overwrite_releases,
                                           overwrite_snapshots = overwrite_snapshots,
                                           dep_plugin_version = dep_plugin_version,
                                           local_repo = local_repo,
                                           mvn_args = mvn_args)

  # get classpath
  res <- execute_mvn_cmd(args)
  mvn_spew <- parse_sys_return(res$stdout)

  if (res$status != 0) {
    print_messages(res)
    stop("Failed to build a classpath with status ", res$status)
  }

  if (verbose) {
    print_messages(res)
  }

  # unconfigure_mvndeps(quiet=quiet)
  return(parse_classpath_from_mvn(mvn_spew))
}

#' @keywords internal
#' @importFrom purrr %>%
generate_mvn_dependency_get_args <- function(pom_path,
                                             include_scope = "runtime",
                                             absolute_paths = TRUE,
                                             overwrite_if_newer = TRUE,
                                             overwrite_releases = FALSE,
                                             overwrite_snapshots = FALSE,
                                             dep_plugin_version = NULL,
                                             local_repo = NULL,
                                             mvn_args = NULL) {

  # normalize path - bad things happen when, for example, a path starts with "~"
  pom_path <- normalizePath(pom_path, winslash="/", mustWork = TRUE)

  # do not make users decide on plugin version
  if (missing(dep_plugin_version))
    dep_plugin_version <- .globals$dep_plugin_version

  # the mvn plugin to run
  mvn_dep_plugin <- paste0("org.apache.maven.plugins:maven-dependency-plugin:", dep_plugin_version)
  args <- c(paste0(mvn_dep_plugin, ":build-classpath"),
            "-f", pom_path,
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
  return(args)
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
  cp <- unlist(strsplit(cp, split=ifelse(is_windows(), ";", ":")))

  return(cp)
}

#' Write a Super Simple POM
#'
#' \code{mvn dependency:build-classpath} only works with a pom.xml - so this function writes a pom to make maven happy.
#' This can also be handy if you want to use \code{\link{build_classpath_from_pom}} with a package and you need to
#'  actually compose a pom.xml to do that.
#'
#'  Note that \code{artifact_id}, \code{group_id}, and \code{version} are required to write a pom file, but have no
#'  practical meaning here since this pom should never be deployed anywhere (for other things to rely on it).
#'  It is really just a mechanism for specifying dependencies.
#'
#'  If you need more complicated pom settings (e.g., an excludes section) you need to write that manually, or use
#'  some other tool to create your pom.xml. This function is not doing anything smart. It just tries to write
#'  a valid pom to specify dependencies for maven to resolve.
#'
#' @inheritParams get_dependencies
#' @param path Character. The path to write the pom.xml file
#' @param artifact_id Character. The name of your project
#' @param group_id Character. Typically the name of your organization
#' @param version Character. Some version number.
#' @export
write_pom <- function(deps,
                      path=tempdir(),
                      artifact_id="temp-proj",
                      group_id="temp-group",
                      version="0.0.1-SNAPSHOT") {

  # open connection to write pom
  filepath <- file.path(path, "pom.xml")
  con <- file(filepath, open="wt")

  # header stuff
  writeLines('<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"', con)
  writeLines('xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd">', con)
  writeLines('\t<modelVersion>4.0.0</modelVersion>', con)

  # project info
  writeLines(paste0('\n\t<artifactId>', artifact_id, '</artifactId>'), con)
  writeLines(paste0('\t<groupId>', group_id, '</groupId>'), con)
  writeLines(paste0('\t<version>', version, '</version>'), con)

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

