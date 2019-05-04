#' Phone number geographical information
#' 
#' @description
#' 
#' Retrieve geographical information related to a phone number.
#' 
#' @section libphonenumber reference:
#'
#'   `get_geocode()`:
#'   `PhoneNumberOfflineGeocoder.getDescriptionForValidNumber()` by
#'   default, or `PhoneNumberOfflineGeocoder.getDescriptionForNumber()` if
#'   `strict = TRUE`.
#' 
#' @param x A [phone] vector.
#' @param strict Should the validity of the phone number be checked? If `TRUE`,
#'   invalid phone numbers return `""`.
#' @return A character vector of text descriptions for each phone number for the
#'   given language code.
#' @examples
#' x <- phone(c(0, 0123, "0412 345 678", "61412987654", "03 9123 4567", "+12015550123"), "AU")
#' get_geocode(x)
#' get_geocode(x, strict = TRUE)
#' 
#' # Specify a home country
#' get_geocode(x, home = "AU")
#' get_geocode(x, home = "US")
#' 
#' # Specify a language
#' get_geocode(x, locale = "de", home = "DE")
#' @export
get_geocode <- function(x, locale = getOption("dialr.locale"), home = NULL, strict = FALSE) {
  if (!is.phone(x)) stop("`x` must be a vector of class `phone`.", call. = FALSE)
  
  validate_phone_region(home)
  
  offline_geocoder <- .get_phoneNumberOfflineGeocoder()
  locale <- .jstr_to_locale(locale) 
  
  if (strict) {
    out <- phone_apply(x, function(pn) {
      .jcall(offline_geocoder, "S", "getDescriptionForNumber", pn, locale, home)
    }, character(1))
  } else {
    out <- phone_apply(x, function(pn) {
      .jcall(offline_geocoder, "S", "getDescriptionForValidNumber", pn, locale, home)
    }, character(1))
  }
  
  out
}