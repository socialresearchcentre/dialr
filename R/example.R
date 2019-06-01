#' Get an example phone number
#'
#' Produces example phone numbers for the given [`region`][dialr-region],
#' [`type`][dialr-type] and `valid` combinations. Input vectors are recycled as
#' necessary if a vector of length 1 is provided.
#'
#' @section libphonenumber reference:
#'
#'   `get_example()`: `PhoneNumberUtil.getExampleNumberForType()`;
#'   `PhoneNumberUtil.getExampleNumber()` if `type` is `NULL` or `NA`;
#'   `PhoneNumberUtil.getInvalidExampleNumber()` if `valid` is `FALSE`.
#'
#' @param region A character vector of [ISO country codes][dialr-region].
#' @param type A character vector of [phone number types][dialr-type] for each
#'   region. If `NULL` (default), returns an example "FIXED_LINE" number.
#'   Returns an empty phone number if `type` is not valid for the provided
#'   `region`.
#' @param valid A logical vector. For each `FALSE` entry, `get_example` returns
#'   an example invalid number, and `type` is ignored.
#' @return A [phone] vector.
#' @examples
#' # Get a basic example number
#' get_example("AU")
#' 
#' # Get an example mobile number
#' get_example("AU", type = "MOBILE")
#' 
#' # Example phone number for an invalid type
#' get_example("AU", type = "VOICEMAIL")
#' 
#' # Get an example invalid number
#' get_example("AU", valid = FALSE)
#' 
#' # Get a combination of the previous examples
#' get_example(c("AU", "AU",     "AU",        "AU" ),
#'             c(NA,   "MOBILE", "VOICEMAIL", NA   ),
#'             c(TRUE, TRUE,     TRUE,        FALSE))
#' @seealso [get_supported_regions()] for valid region codes,
#'   [get_types_for_region()] to get valid phone types for a region.
#' @export
get_example <- function(region, type = NULL, valid = TRUE) {
  vec_length <- max(length(region), length(type), length(valid))
  
  if (vec_length > 1 & length(region) == 1) region <- rep(region, vec_length)
  validate_phone_region(region)
  
  if (is.null(type)) {
    type <- rep(NA_character_, vec_length)
  } else {
    if (vec_length > 1 & length(type) == 1) type <- rep(type, vec_length)
    validate_phone_type(type[!is.na(type)])
  }
  
  if (vec_length > 1 & length(valid) == 1) valid <- rep(valid, vec_length)
  stopifnot(is.logical(valid))
  
  if (!all(length(region) == vec_length,
           length(type) == vec_length,
           length(valid) == vec_length))
    stop("`region`, `type` and `valid` vectors must be the same length where provided.", call. = FALSE)
  
  phone_util <- .get_phoneNumberUtil()
  format <- .get_phone_format_from_string("E164")
  
  out <- structure(
    mapply(function(r, t, v) {
      if (!v) {
        pn <- .jcall(phone_util,
                     "Lcom/google/i18n/phonenumbers/Phonenumber$PhoneNumber;",
                     "getInvalidExampleNumber",
                     r)
      } else if (is.na(t)) {
        pn <- .jcall(phone_util,
                     "Lcom/google/i18n/phonenumbers/Phonenumber$PhoneNumber;",
                     "getExampleNumber",
                     r)
      } else {
        pn <- .jcall(phone_util,
                     "Lcom/google/i18n/phonenumbers/Phonenumber$PhoneNumber;",
                     "getExampleNumberForType",
                     r, .get_phone_type_from_string(t))
      }
      if (is.null(pn)) {
        pn <- NA
        p <- NA_character_
      } else {
        .jcache(pn)
        p <- .jcall(phone_util, "S", "format", pn, format)
      }
      
      list(raw = p,
           region = r,
           jobj = pn)
    },
    region, type, valid,
    SIMPLIFY = FALSE),
    class = "phone"
  )
  
  names(out) <- NULL
  out
}
