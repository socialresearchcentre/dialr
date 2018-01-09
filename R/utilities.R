
# check for updates to libphonenumber
#' @import stringr
#' @import xml2
.update_libphonenumber <- function() {
  message("dialr: checking for latest version of libphonenumber")
  jar_file <- list.files(system.file("java", package = "dialr"), ".*.jar$")

  current <- str_replace(jar_file, "^libphonenumber-(.*).jar$", "\\1")

  latest <- read_xml("http://repo1.maven.org/maven2/com/googlecode/libphonenumber/libphonenumber/maven-metadata.xml") %>%
    xml_find_first("//latest") %>%
    xml_text

  tryCatch({
    if (current != latest) {
      message("dialr: updating libphonenumber from version ", current, " to ", latest)
      download.file(str_c("http://repo1.maven.org/maven2/com/googlecode/libphonenumber/libphonenumber/",
                          latest, "/libphonenumber-", latest, ".jar"),
                    str_c(system.file("java", package = "dialr"), "/libphonenumber-", latest, ".jar"),
                    quiet = TRUE)
      
      invisible(file.remove(system.file("java", jar_file, package = "dialr")))
    }
    message("dialr: up to date!")
  },
  error = function(e) { message("dialr: libphonenumber update failed, continuing with version ", current) })
}

#' @import rJava
.onAttach <- function(libname, pkgname) {
  # check for libphonenumber updates
  .update_libphonenumber()
  
  .jpackage(pkgname)  # needed to load RInterface.java
  
  # what's your java  version?  Need > 1.5.0.
  jversion <- .jcall('java.lang.System','S','getProperty','java.version')
  if (jversion < "1.5.0")
    stop(paste("Your java version is ", jversion,
               ".  Need 1.5.0 or higher.", sep=""))
  
}
