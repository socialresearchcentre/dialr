
#' Old one shot phone functions
#' 
#' @description
#' 
#' This set of methods is a relic of the early development of dialr. They are
#' significantly slower than the current approach. They were soft deprecated in
#' 0.2.0, hard deprecated in 0.4.0 and will be entirely removed in the next
#' major release.
#' 
#' All functions have a corresponding function in the current version of dialr.
#' Due to the changes in approach, the new versions of each function are not
#' drop-in replacements and most need a parsed phone vector as input. See the
#' introductory vignette or the [phone] class documentation for more detail.
#' 
#' * `ph_valid()` -> [is_valid()]
#' * `ph_region()` -> [get_region()]
#' * `ph_possible()` -> [is_possible()]
#' * `ph_format()` -> [format.phone()]
#' * `ph_type()` -> [get_type()]
#' * `ph_example()` -> [get_example()]
#' 
#' @keywords internal
#' @name phone_old
#' @aliases NULL
NULL


#' @rdname phone_old
#' @export
ph_valid <- function(phone, country, strict = FALSE) {
  stop("ph_valid() is deprecated as of dialr 0.4.0\nplease use is_valid() instead", call. = FALSE)
}

#' @rdname phone_old
#' @export
ph_region <- function(phone, country) {
  stop("ph_region() is deprecated as of dialr 0.4.0\nplease use get_region() instead", call. = FALSE)
}

#' @rdname phone_old
#' @export
ph_possible <- function(phone, country, detailed = FALSE) {
  stop("ph_possible() is deprecated as of dialr 0.4.0\nplease use is_possible() instead", call. = FALSE)
}

#' @rdname phone_old
#' @export
ph_format <- function(phone, country, format = "NATIONAL", home = NULL, clean = FALSE) {
  stop("ph_format() is deprecated as of dialr 0.4.0\nplease use format.phone() instead", call. = FALSE)
}

#' @rdname phone_old
#' @export
ph_type <- function(phone, country) {
  stop("ph_type() is deprecated as of dialr 0.4.0\nplease use get_type() instead", call. = FALSE)
}

#' @rdname phone_old
#' @export
ph_example <- function(country, type = NULL, home = NULL, clean = FALSE) {
  stop("ph_example() is deprecated as of dialr 0.4.0\nplease use get_example() instead", call. = FALSE)
}
