# nocov start
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.dialr <- list(
    dialr.name = "DIALR",
    dialr.home = "AU",
    dialr.format = "E164",
    dialr.show_progress = TRUE
  )
  toset <- !(names(op.dialr) %in% names(op))
  if (any(toset)) options(op.dialr[toset])
  
  register_s3_method("pillar", "pillar_shaft", "phone")
  register_s3_method("pillar", "type_sum", "phone")
  register_s3_method("pillar", "is_vector_s3", "phone")
  register_s3_method("pillar", "obj_sum", "phone")
  
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
               ".  Need 1.5.0 or higher.", sep = ""))
  
  # initialise PhoneNumberUtil singleton
  invisible(.get_phoneNumberUtil())
}

register_s3_method <- function(pkg, generic, class, fun = NULL) {
  stopifnot(is.character(pkg), length(pkg) == 1)
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)
  
  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else {
    stopifnot(is.function(fun))
  }
  
  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }
  
  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
}

# nocov end
