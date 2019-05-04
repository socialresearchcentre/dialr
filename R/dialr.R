#' @section Options:
#' - `dialr.home`: The default [region][dialr-region] used to process phone
#' numbers where no region is provided. (default: `"AU"`).
#' - `dialr.format`: The default format used to print clean phone numbers. See
#' [format.phone()] for details. (default: `"E164"`).
#' - `dialr.locale`: The default locale used for [geographical][get_geocode] and
#' [carrier][get_carrier] information (default: `"en"`).
#' - `dialr.show_progress`: Should lengthy operations such as [phone()] show a
#' progress bar? (default: `TRUE`).
#' @import dialrjars
#' @import rJava
#' @family phone functions
#' @keywords internal
"_PACKAGE"
