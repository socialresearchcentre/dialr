
#' Initialise country code lookup table
#'
#' @return Named vector of country names with corresponding ISO country code
.init_cc <- function () {
  cc_file <- system.file("extdata",
                         "cc.csv",
                         package="dialr")

  cc_data <- read.csv(cc_file, stringsAsFactors = FALSE)

  cc_lookup <- toupper(cc_data$code)
  names(cc_lookup) <- toupper(cc_data$country)

  cc_lookup
}

cc_lookup <- .init_cc()

#' Get ISO country code
#'
#' @param country vector of country names to retrieve ISO country codes for
#' @return vector of ISO country codes (NA where not found)
#' @export
get_cc <- function(country) {
  country <- toupper(country)

  as.vector(sapply(country,
                   function(d) {
                     if (d %in% names(cc_lookup)) {
                       cc_lookup[[d]]
                     } else {
                       NA_character_
                     }
                   }))
}

#' Check ISO country code
#'
#' @param country vector of country codes to check
#' @return logical vector flagging which records are valid ISO country codes
#' @export
check_cc <- function(country) {
  country %in% cc_lookup
}

#' internal country code existence check
.validate_cc <- function(country) {
  chk <- check_cc(country)
  if (any(!chk)) stop("invalid country codes: ",
                      paste0(unique(country[!chk]), collapse = ", "),
                      call. = FALSE)
  NULL
}

