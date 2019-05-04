#' Phone number time zone
#' 
#' @description
#' 
#' Retrieve a list of [CLDR time
#' zones](http://www.unicode.org/cldr/charts/latest/supplemental/zone_tzid.html)
#' to which a phone number belongs.
#' 
#' @section libphonenumber reference:
#'
#'   `get_timezone()`:
#'   `PhoneNumberToTimeZonesMapper.getTimeZonesForGeographicalNumber()` by
#'   default, or `PhoneNumberToTimeZonesMapper.getTimeZonesForNumber()` if
#'   `strict = TRUE`.
#' 
#' @param x A [phone] vector.
#' @param strict Should the validity of the phone number be checked? If `TRUE`,
#'   invalid phone numbers return the default unknown time zone `"Etc/Unknown"`.
#' @return A character vector of time zones to which each phone number belongs,
#'   separated by `;`.
#' @examples
#' x <- phone(c(0, 0123, "0412 345 678", "61412987654", "03 9123 4567", "+12015550123"), "AU")
#' get_timezone(x)
#' get_timezone(x, strict = TRUE)
#' 
#' # Return a list
#' strsplit(get_timezone(x), ";")
#' @export
get_timezone <- function(x, strict = FALSE) {
  if (!is.phone(x)) stop("`x` must be a vector of class `phone`.", call. = FALSE)
  timezone_mapper <- .get_phoneNumberToTimeZonesMapper()
  
  if (strict) {
    out <- phone_apply(x, function(pn) {
      res <-
        paste0(
          .jset_to_str(
            .jcall(timezone_mapper,
                   "Ljava/util/List;",
                   "getTimeZonesForNumber",
                   pn)
            
          ),
          collapse = ";")
      ifelse(is.null(res), NA_character_, res)
    }, character(1), progress = TRUE)
  } else {
    out <- phone_apply(x, function(pn) {
      res <-
        paste0(
          .jset_to_str(
            .jcall(timezone_mapper,
                   "Ljava/util/List;",
                   "getTimeZonesForGeographicalNumber",
                   pn)
          ),
          collapse = ";")
      ifelse(is.null(res), NA_character_, res)
    }, character(1), progress = TRUE)
  }
  
  out
}