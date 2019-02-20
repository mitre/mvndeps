#' Write a basic pom.xml
#' 
#' The purpose of this function is to write a pom.xml to support classpath management when using rJava on
#' dependencies with dependencies of their own. For the most part this is probably not a function that users
#' should be calling directly.
#' 
#' @inheritParams get_classpath
#' @param path Character. The path where the pom should be written. 
#' @param artifactid Character. The \code{artifactId} entry in the resulting pom.xml. Unless you intend
#'   to save this pom for future use this entry does not really matter and the default is fine.
#' @param groupid Character. The \code{groupId} entry in the resulting pom.xml. See \code{artifactid}
#'   for use case.
#' @param artifactversion Character. The \code{version} entry in the resulting pom.xml. See \code{artifactid}
#'   for use case.
#' @return the file path of the newly written pom.xml
#' @export
write_pom <- function(dep, group, version, path=tempdir(), artifactid="tempproj", 
                      groupid="tempgroup", artifactversion="0.0.1-SNAPSHOT") {
  
  # put all dependency information together to support all input signatures
  deps <- concatenate_dependencies(dep, group, version)
  
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