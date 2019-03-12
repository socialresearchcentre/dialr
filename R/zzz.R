
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.dialr <- list(
    dialr.name = "DIALR",
    dialr.home = "AU",
    dialr.format = "NATIONAL"
  )
  toset <- !(names(op.dialr) %in% names(op))
  if(any(toset)) options(op.dialr[toset])
  
  invisible()
}

#' @import rJava
.onAttach <- function(libname, pkgname) {
  rJava::.jpackage("dialrjars")
  rJava::.jpackage(pkgname, lib.loc = libname)  # needed to load RInterface.java
  
  # what's your java  version?  Need > 1.5.0.
  jversion <- .jcall('java.lang.System','S','getProperty','java.version')
  if (jversion < "1.5.0")
    stop(paste("Your java version is ", jversion,
               ".  Need 1.5.0 or higher.", sep=""))
  
  # initialise PhoneNumberUtil singleton
  invisible(.get_phoneNumberUtil())
}
