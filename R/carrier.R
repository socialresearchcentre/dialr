#' Phone number carrier information
#' 
#' @description
#' 
#' Returns a carrier name for each phone number, in the language provided
#' in `locale`.
#' 
#' @details
#' 
#' The carrier name is the one the number was originally allocated to, however
#' if the country supports mobile number portability the number might not belong
#' to the returned carrier anymore. If no mapping is found `""` is returned.
#' 
#' @section libphonenumber reference:
#'
#'   `get_geocode()`: `PhoneNumberToCarrierMapper.getNameForValidNumber()` by
#'   default, `PhoneNumberToCarrierMapper.getNameForNumber()` if `check = TRUE`,
#'   or `PhoneNumberToCarrierMapper.getSafeDisplayName()` if `strict = TRUE`.
#' 
#' @param x A [phone] vector.
#' @param check Should the validity of each phone number be checked? If `TRUE`,
#'   invalid phone numbers return `""`.
#' @param strict If `TRUE`, gets the name of the carrier for a given phone
#'   number only when it is 'safe' to display to users. A carrier name is
#'   considered safe if the number is valid and for a region that doesn't
#'   support mobile number portability. All other phone numbers return `""`.
#' @param locale The [Java
#'   locale](https://docs.oracle.com/javase/8/docs/api/java/util/Locale.html)
#'   used to retrieve localised results. The default is set in option
#'   `dialr.locale`.
#' @return A carrier name for each phone number for the given locale, or `""` if
#'   the number is invalid.
#' @examples
#' x <- phone(c(0, 0123, "0412 345 678", "61412987654", "03 9123 4567", "+12015550123"), "AU")
#' get_carrier(x)
#' get_carrier(x, strict = TRUE)
#' @export
get_carrier <- function(x, check = FALSE, strict = FALSE, locale = getOption("dialr.locale")) {
  if (!is.phone(x)) stop("`x` must be a vector of class `phone`.", call. = FALSE)
  if (check && strict) warning("`strict = TRUE` overrides `check`. Ignoring `check = TRUE`.", call. = FALSE)
  
  carrier_mapper <- .get_phoneNumberToCarrierMapper()
  locale <- .jstr_to_locale(locale) 
  
  if (strict) {
    region <- get_region(x)
  
    out <- NA
    out[!is.na(region)] <-
      phone_apply(x[!is.na(region)], function(pn) {
        .jcall(carrier_mapper, "S", "getSafeDisplayName", pn, locale)
      }, character(1))
  } else if (check) {
    out <- phone_apply(x, function(pn) {
      .jcall(carrier_mapper, "S", "getNameForNumber", pn, locale)
    }, character(1))
  } else {
    out <- phone_apply(x, function(pn) {
      .jcall(carrier_mapper, "S", "getNameForValidNumber", pn, locale)
    }, character(1))
  }
  
  out
}
