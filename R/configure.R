configure_mvndeps <- function(mvn, java_home, quiet=FALSE) {
  if (!missing(java_home))
    set_java_home(java_home, quiet=quiet)
  if (Sys.getenv("JAVA_HOME")=="") {
    set_java_home(quiet=quiet)
    set_var("temp.java.home", TRUE)
  } else {
    set_var("temp.java.home", FALSE)
  }
}

unconfigure_mvndeps <- function(quiet=FALSE) {
  if (get_var("temp.java.home")==TRUE) {
    clear_java_home(quiet=quiet)
    set_var("temp.java.home", FALSE)
  }
}