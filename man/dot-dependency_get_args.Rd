% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dependency_get.R
\name{.dependency_get_args}
\alias{.dependency_get_args}
\title{Build the command to have maven download a dependency (package-private)}
\usage{
.dependency_get_args(
  artifact,
  transitive = TRUE,
  remote_repos = NULL,
  dep_plugin_version
)
}
\description{
This function will build a system command using the maven dependency get funtionality to download a (java)
dependency.
}
\keyword{internal}
