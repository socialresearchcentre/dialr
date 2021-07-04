#' Phone number time zone
#' 
#' @description
#' 
#' Retrieve a list of [CLDR time
#' zones](http://www.unicode.org/cldr/charts/latest/supplemental/zone_tzid.html)
#' to which a phone number belongs.
#' 
#' @details
#' 
#' This function assumes the phone number is geo-localizable. Fixed-line and
#' mobile numbers are considered possible candidates for geo-localization.
#' 
#' @section libphonenumber reference:
#'
#'   `get_timezone()`:
#'   `PhoneNumberToTimeZonesMapper.getTimeZonesForGeographicalNumber()`.
#' 
#' @param x A [phone] vector.
#' @param strict Should invalid phone numbers be removed? If `TRUE`, invalid
#'   phone numbers are replaced with `NA`.
#' @param show_progress Should a progress bar be displayed? Defaults to the
#'   option `dialr.show_progress`.
#' @return A character vector of time zones to which each phone number belongs,
#'   separated by `;`, or the default unknown time zone `"Etc/Unknown"` if no
#'   other time zone was found.
#' @examples
#' x <- phone(c(0, 0123, "0412 345 678", "61412987654", "03 9123 4567", "+12015550123"), "AU")
#' get_timezone(x)
#' get_timezone(x, strict = TRUE)
#' 
#' # Return a list
#' strsplit(get_timezone(x), ";")
#' @export
get_timezone <- function(x, strict = FALSE,
                         show_progress = getOption("dialr.show_progress")) {
  if (!is.phone(x)) stop("`x` must be a vector of class `phone`.", call. = FALSE)
  timezone_mapper <- .get_phoneNumberToTimeZonesMapper()
  
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
  }, character(1), show_progress = show_progress)
  if (strict) out[!is_valid(x)] <- NA_character_
  
  out
}