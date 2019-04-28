#' Phone number type
#'
#' @description
#'
#' In addition to validity, libphonenumber can identify phone number type - it
#' is able to distinguish Fixed-line, Mobile, Toll-free, Premium Rate, Shared
#' Cost, VoIP, Personal Numbers, UAN, Pager, and Voicemail (whenever feasible).
#' 
#' `get_type(x)` returns the phone number type for each element of a [phone]
#' vector.
#'
#' Valid phone number types differ by region. `get_types_for_region(x)` returns
#' a list of character vectors of valid types for each provided
#' [ISO country code][dialr-region]. Use `get_supported_types()` to see a full
#' list of supported types.
#' 
#' @section libphonenumber reference:
#'
#'   `get_type()`: `PhoneNumberUtil.getNumberType()`
#'   
#'   `get_supported_types()`: `PhoneNumberUtil.PhoneNumberType`
#'   
#'   `get_types_for_region()`: `PhoneNumberUtil.getSupportedTypesForRegion()`
#'   
#' @param x A [phone] vector, or a character vector of [ISO country
#'   codes][dialr-region].
#' @return A character vector of phone types.
#' 
#'   `get_types_for_region()` returns a list of character vectors for each
#'   provided country code.
#' @examples
#'   # Get phone types for a phone vector
#'   x <- phone(c(0, 0123, "0412 345 678", "61412987654", "03 9123 4567", "+12015550123"), "AU")
#'   get_type(x)
#'   
#'   # All supported phone types
#'   get_supported_types()
#'   
#'   # Get supported types for specified regions
#'   get_types_for_region("AU")
#'   get_types_for_region(c("GB", "US"))
#'   get_types_for_region(get_supported_regions())[1:5]
#'   
#' @name dialr-type
#' @family phone functions
#' @export
get_type <- function(x) {
  if (!is.phone(x)) stop("`x` must be a vector of class `phone`.", call. = FALSE)
  phone_util <- .get_phoneNumberUtil()
  
  out <- phone_apply(x, function(pn) {
    .jstrVal(.jcall(phone_util,
                    "Lcom/google/i18n/phonenumbers/PhoneNumberUtil$PhoneNumberType;",
                    "getNumberType",
                    pn))
  }, character(1))
  
  out
}

#' @rdname dialr-type
#' @export
get_supported_types <- function() {
  .get_phoneNumberType()
}

#' @rdname dialr-type
#' @export
get_types_for_region <- function(x) {
  validate_phone_region(x)
  lapply(x, .getSupportedTypesForRegion)
}
