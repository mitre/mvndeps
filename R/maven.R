#' Find maven on the system
#'
#' Determine where maven is installed.
#'
#' There are a few options for setting which maven install to use. If maven is installed in
#' a standard location then \code{find_mvn} will probably work with no help. Users may also
#' choose a maven version by setting a \code{maven.path} option or by calling the
#' \code{use_mvn} function. Once called, the package will remember the setting until detached.
#'
#' @importFrom purrr %>% map_chr detect
#' @export
find_mvn <- function() {

  # if there is an options setting, use it
  if (!is.null(.globals$which_mvn) &&
      test_mvn(.globals$which_mvn)) {
    return(.globals$which_mvn)
  }

  # try to find it automatically
  mvn <- Sys.which("mvn")
  if (nchar(mvn) > 0 &&
      test_mvn(mvn)) {
    return(mvn)
  }

  stop("Unable to find mvn. Try setting options(maven.path = ...) or using install_mvn() to install")
}

#' @rdname find_mvn
#' @param mvn Character. A path to a maven executable
#' @export
use_mvn <- function(mvn) {

  if (!test_mvn(mvn))
    stop(paste0("Cannot set maven path to '", mvn, "'. Maven not runnable from that path."))
  .globals$which_mvn <- mvn
}

#' Get maven version string
#'
#' @export
mvn_version <- function(verbose = FALSE) {

  args <- "-version"
  res <- execute_mvn_cmd(args)
  mvn_spew <- rawToChar(res$stdout)

  if (res$status != 0) {
    print_messages(res)
  }

  version_spew <- system(paste(.globals$which_mvn, "-version"), intern = TRUE)
  parts <- strsplit(version_spew[1], split = "\\s")
  return(parts[[1]][3])
}

#' Kind of like test_mvn, but will set the .globals option if it is null and find_mvn
#' finds a version that tests well. No action if .globals$which_mv is set
#'
#' @noRd
check_mvn <- function() {
  if (!is.null(.globals$which_mvn))
    return()

  mvn <- find_mvn()
  if (test_mvn(mvn))
    .globals$which_mvn <- mvn
  else
    stop("Could not find Maven, see ?install_maven for help")
}

#' Test Maven
#'
#' Test to ensure that maven is working on your system by trying to run a command
#' to get the maven version you're running.
#'
#' @return Logical. If \code{TRUE} maven is working fine. Otherwise it isn't.
#' @export
test_mvn <- function(mvn, verbose = FALSE) {

  args <- "-version"
  res <- execute_mvn_cmd(cmd = mvn, args = args, check = FALSE)

  if (verbose) {
    print_messages(res)
  }
  return(res$status == 0)
}

#' Find your local maven repository
#'
#' This is primarily intended for internal use. It may however be useful for some to have
#' a programatic way of identifying the local maven reposiory.
#'
#' @param verbose Logical. Dump maven output
#' @export
find_local_mvn_repo <- function(verbose = FALSE) {

  # report back cahced value if known
  m2repo <- .globals$local_repo
  if (!is.null(m2repo))
    return(m2repo)

  # final local repo
  args <- c("help:evaluate", "-Dexpression=settings.localRepository")
  res <- execute_mvn_cmd(args)
  if (res$status != 0) {
    print_messages(res)
    stop("Unable to find local repository - see system output for cause")
  } else if (verbose) {
    print_messages(res, stdout_only = TRUE)
  }

  mvn_spew <- parse_sys_return(res$stdout)
  m2repo <- mvn_spew[!grepl("[", mvn_spew, fixed=TRUE)]
  if (length(m2repo) == 0)
    stop("Unable to find local maven repository.")

  # sometimes if other dependencies are coming in there is still junk in the array, take last entry
  if (length(m2repo) > 1)
    m2repo <- m2repo[length(m2repo)]

  # fix path on windows to use forward slashes (what R expects)
  m2repo <- gsub("\\\\", "/", m2repo)

  # cache & return
  .globals$local_repo <- m2repo
  return(m2repo)
}

get_mvn_settings <- function() {
  args <- "help:effective-settings"
  res <- execute_mvn_cmd(args)
  mvn_spew <- rawToChar(res$stdout)
  if (res$status != 0) {
    message(mvn_spew)
    message(rawToChar(res$stderr))
    stop("Unable to get maven settings - see system command output for the cause")
  }
  return(settings[!grepl("\\[", mvn_spew, fixed=TRUE)])
}

#' @noRd
#' @importFrom purrr %>%
#' @importFrom sys exec_internal
execute_mvn_cmd <- function(args, cmd = .globals$which_mvn, check = TRUE) {

  # make sure mvn is available
  if (check)
    check_mvn()

  # to support purrr pipelines need to be able to handle list args
  if (is.list(args))
    args <- unlist(args)

  tryCatch(exec_internal(cmd, args = args, error = FALSE),
           error = function(e) {
             msg <- paste0("Error: Failed to execute '", cmd, "' (No such file or directory)")
             return(list(status = 1,
                         stdout = raw(0),
                         stderr = charToRaw(msg)))
           })
}

#' @importFrom purrr %>% when
#' @noRd
parse_sys_return <- function(raw) {
  parsed <- raw %>%
    rawToChar() %>%
    # deal with windows-style \r\n newline (mvn prints its own \n line, so on
    # windwows there is a fun mix of newline characters)
    when( is_windows() ~ gsub(pattern = "\\r", replacement = "", x = .),
         !is_windows() ~ .) %>%
    strsplit(split = "\\n") %>%
    unlist() %>%
    return()
}

print_messages <- function(res, stdout_only = FALSE) {
  message(rawToChar(res$stdout))
  if (!stdout_only)
    message(rawToChar(res$stderr))
}
