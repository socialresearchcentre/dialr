#' Phone number region
#'
#' @description
#'
#' In libphonenumber a phone number region is represented by a 2 digit ISO
#' country code. `get_region(x)` returns the 2-digit [ISO country
#' code](https://en.wikipedia.org/wiki/ISO_3166-1) for each element of a [phone]
#' vector.
#' 
#' Use `get_supported_regions()` to see a full list of supported regions.
#' 
#' Region can also be retrieved from an international calling code.
#' `get_region_for_calling_code(x)` returns the main region for each provided
#' calling code. Since multiple regions can share a single calling code,
#' `get_regions_for_calling_code(x)` returns a list of character vectors of
#' regions for each.
#'
#' @section libphonenumber reference:
#'
#'   `get_region()`: `PhoneNumberUtil.getRegionCodeForNumber()`
#'
#'   `get_supported_regions()`: `PhoneNumberUtil.getSupportedRegions()`
#'
#'   `get_region_for_calling_code()`:
#'   `PhoneNumberUtil.getRegionCodeForCountryCode()`
#'
#'   `get_regions_for_calling_code()`:
#'   `PhoneNumberUtil.getRegionCodesForCountryCode()`
#'   
#' @param x A [phone] vector, or a vector of calling codes.
#' @return A character vector of country codes.
#' 
#'   `get_regions_for_calling_code()` returns a list of character vectors for
#'   each provided calling code.
#' @examples
#'   # Get regions for a phone vector
#'   x <- phone(c(0, 0123, "0412 345 678", "61412987654", "03 9123 4567", "+12015550123"), "AU")
#'   get_region(x)
#'
#'   # All supported region codes
#'   get_supported_regions()
#'   
#'   # Primary region for a calling code
#'   get_region_for_calling_code(c(1, 61, 84))
#'   
#'   # All regions for a calling code
#'   get_regions_for_calling_code(c(1, 61, 84))
#'   
#' @name dialr-region
#' @family phone functions
#' @export
get_region <- function(x) {
  if (!is.phone(x)) stop("`x` must be a vector of class `phone`.", call. = FALSE)
  phone_util <- .get_phoneNumberUtil()
  
  out <- phone_apply(x, function(pn) {
    res <- .jcall(phone_util, "S", "getRegionCodeForNumber", pn)
    ifelse(is.null(res), NA_character_, res)
  }, character(1))
  
  out
}

#' @rdname dialr-region
#' @export
get_supported_regions <- function() {
  .getSupportedRegions()
}

#' @rdname dialr-region
#' @export
get_region_for_calling_code <- function(x) {
  validate_phone_calling_code(x)
  vapply(x, .getRegionCodeForCountryCode, character(1), USE.NAMES = FALSE)
}

#' @rdname dialr-region
#' @export
get_regions_for_calling_code <- function(x) {
  validate_phone_calling_code(x)
  lapply(x, .getRegionCodesForCountryCode)
}
