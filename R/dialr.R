#' Phone number validation in R
#'
#' dialr is an R interface to Google's libphonenumber library.
#'
#' @section Overview:
#'
#' dialr is an R port of [Google's libphonenumber
#' library](https://github.com/googlei18n/libphonenumber). It uses the java
#' implementation of libphonenumber via rJava for all phone number processing.
#'
#' For a full rundown of libphonenumber see their
#' [github](https://github.com/googlei18n/libphonenumber) and
#' [javadocs](https://javadoc.io/doc/com.googlecode.libphonenumber/libphonenumber/).
#'
#' @section Options:
#' - `dialr.home`: The default region used to process phone numbers where no
#' region is provided. (default: `"AU"`).
#' - `dialr.format`: The default format used to print clean phone numbers. See
#' [format.phone()] for details. (default: `"E164"`).
#' - `dialr.show_progress`: Should lengthy operations such as [phone()] show a
#' progress bar? (default: `TRUE`).
#'
#' @examples
#' library(dialr)
#'
#' # Parse phone number vector
#' x <- c(0, 0123, "0404 753 123", "61410123817")
#' x <- phone(x, "AU")
#'
#' is_parsed(x)    # Was the phone number successfully parsed?
#' is_valid(x)     # Is the phone number valid?
#' is_possible(x)  # Is the phone number possible?
#' get_region(x)   # What region (ISO country code) is the phone number from?
#' get_type(x)     # Is the phone number a fixed line, mobile etc.
#' format(x)
#' format(x, home = "AU")
#' @import dialrjars
#' @import rJava
#' @family phone functions
#' @keywords internal
"_PACKAGE"
