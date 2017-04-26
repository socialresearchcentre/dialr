
#' @import rJava
.onAttach <- function(libname, pkgname)
{
  .jpackage(pkgname)  # needed to load RInterface.java
  
  # what's your java  version?  Need > 1.5.0.
  jversion <- .jcall('java.lang.System','S','getProperty','java.version')
  if (jversion < "1.5.0")
    stop(paste("Your java version is ", jversion,
                 ".  Need 1.5.0 or higher.", sep=""))

}

