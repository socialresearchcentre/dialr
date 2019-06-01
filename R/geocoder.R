#' Phone number geographical information
#' 
#' @description
#' 
#' Returns a text description for each phone number, in the language provided in
#' `locale`.
#' 
#' @details
#' 
#' The description might consist of the name of the country where the phone
#' number is from, or the name of the geographical area the phone number is from
#' if more detailed information is available.
#' 
#' If a phone number is from the region specified in `home`, only a
#' lower-level description will be returned, if one exists. Otherwise, the phone
#' number's region will be returned, with optionally some more detailed
#' information.
#' 
#' For example, for a user from the region `"US"` (United States), we would show
#' `"Mountain View, CA"` for a particular number, omitting the United States
#' from the description. For a user from the United Kingdom (region `"GB"`), for
#' the same number we may show `"Mountain View, CA, United States"` or even just
#' `"United States"`.
#' 
#' @section libphonenumber reference:
#'
#'   `get_geocode()`:
#'   `PhoneNumberOfflineGeocoder.getDescriptionForValidNumber()`.
#' 
#' @param x A [phone] vector.
#' @param home [ISO country code][dialr-region] for home region. See Details.
#' @param strict Should invalid phone numbers be removed? If `TRUE`, invalid
#'   phone numbers are replaced with `NA`.
#' @param locale The [Java
#'   locale](https://docs.oracle.com/javase/8/docs/api/java/util/Locale.html)
#'   used to retrieve localised results. The default is set in option
#'   `dialr.locale`.
#' @return A text description for each phone number for the given locale, or
#'   `""` if the number is invalid or could belong to multiple countries.
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
#' get_geocode(x, home = "DE", locale = "de")
#' @export
get_geocode <- function(x, home = NULL, strict = FALSE,
                        locale = getOption("dialr.locale")) {
  if (!is.phone(x)) stop("`x` must be a vector of class `phone`.", call. = FALSE)
  
  validate_phone_region(home)
  
  offline_geocoder <- .get_phoneNumberOfflineGeocoder()
  locale <- .jstr_to_locale(locale) 
  
  out <- phone_apply(x, function(pn) {
    .jcall(offline_geocoder, "S", "getDescriptionForValidNumber", pn, locale, home)
  }, character(1))
  if (strict) out[!is_valid(x)] <- NA_character_
  
  out
}