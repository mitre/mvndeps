concatenate_dependency <- concatenate_dependencies <- function(dep, group, version) {
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