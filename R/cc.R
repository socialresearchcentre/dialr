
#' Get ISO country code
#'
#' @param country Vector of country names
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
#' @param country Vector of ISO country codes
#' @return logical vector flagging which elements are valid ISO country codes
#' @export
check_cc <- function(country) {
  country %in% cc_lookup
}

# internal country code existence check - throws error if not found
validate_cc <- function(country) {
  chk <- check_cc(country)
  if (any(!chk)) stop("invalid country codes: ",
                      paste0(unique(country[!chk]), collapse = ", "),
                      call. = FALSE)
  NULL
}

