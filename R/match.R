#' Phone number equality checks
#'
#' @description
#'
#' Check if two vectors contain matching phone numbers. See Details section for
#' a full list of match types. `is_match()` with default arguments is used to
#' implement `==` and `!=` for phone vectors.
#' 
#' `is_match()` accepts phone or atomic vectors. Atomic vectors are converted to
#' character for comparison. Note that although they can contain formatting
#' character vectors are not parsed with a default region, so they will only
#' ever be an `"EXACT_MATCH"` if a country calling code is specified with `+` at
#' the start. See Examples.
#'
#' @details
#' 
#' Possible return values for `is_match(x, detailed = TRUE)`:
#'
#' * `"EXACT_MATCH"`: The country_code, NSN, presence of a leading zero for
#' Italian numbers and any extension present are the same.
#' 
#' * `"NSN_MATCH"`: Either or both values has no region specified, and the NSNs
#' and extensions are the same.
#' 
#' * `"SHORT_NSN_MATCH"`: Either or both values has no region specified, or the
#' region specified is the same, and one NSN could be a shorter version of the
#' other number. This includes the case where one has an extension specified,
#' and the other does not.
#' 
#' * `"NOT_A_NUMBER"`: One of the input phone numbers failed to parse.
#' 
#' * `"NO_MATCH"`: All others.
#' 
#' For example, the numbers `+1 345 657 1234` and `657 1234` are a
#' `"SHORT_NSN_MATCH"`. The numbers `+1 345 657 1234` and `345 657` are a
#' `"NO_MATCH"`.
#'
#' @section libphonenumber reference:
#'
#'   `is_match()`: `PhoneNumberUtil.isNumberMatch()`
#'
#' @param e1 A [phone] or character vector.
#' @param e2 A [phone] or character vector.
#' @param detailed If `FALSE`, `is_match()` returns a logical vector. If `TRUE`,
#'   it returns a character vector with the match type. See Details section for
#'   possible return values.
#' @param strict If `TRUE`, only `"EXACT_MATCH"` is treated as a match. If
#'   `FALSE`, `"EXACT_MATCH"`, `"NSN_MATCH"` and `"SHORT_NSN_MATCH"` are all
#'   considered a match. Ignored if `detailed = TRUE`.
#' @param not_number_na If `TRUE`, `"NOT_A_NUMBER"` is converted to `NA`.
#' @return A logical or character vector.
#' @examples
#' is_match(phone("0412 345 678", "AU"), phone("+61412345678", "AU"))
#' 
#' phone("0412 345 678", "AU") == phone("+61412345678", "AU")
#' phone("0412 345 678", "AU") != phone("+61412345678", "AU")
#' 
#' # character vectors are only fully specified with a country calling code
#' is_match("0412345678", "0412345678", detailed = TRUE)
#' is_match("+61412345678", "+61412345678", detailed = TRUE)
#' 
#' is_match(phone("0412345678", "AU"), "0412345678", detailed = TRUE)
#' is_match(phone("+61412345678", "AU"), "+61412345678", detailed = TRUE)
#' @name dialr-match
NULL

#' @rdname dialr-match
#' @export
is_match <- function(e1, e2, detailed = FALSE, strict = TRUE, not_number_na = TRUE) {
  if (is.atomic(e1)) e1 <- as.character(e1)
  if (is.atomic(e2)) e2 <- as.character(e2)
  
  if (!is.phone(e1) & !is.character(e1))
    stop("`e1` must be a vector of class `phone` or `character`.",
         call. = FALSE)
  if (!is.phone(e2) & !is.character(e2))
    stop("`e2` must be a vector of class `phone` or `character`.",
         call. = FALSE)
  
  # isNumberMatch doesn't take char then phone, so swap around
  if (is.character(e1) & is.phone(e2)) {
    tmp <- e1
    e1 <- e2
    e2 <- tmp
  }
  
  phone_util <- .get_phoneNumberUtil()
  
  out <- mapply(
    function(pn1, pn2) {
      if (is.character(pn1)) pn1 <- list(jobj = .jstr_to_char(pn1))
      if (is.character(pn2)) pn2 <- list(jobj = .jstr_to_char(pn2))
      
      if (!(exists("jobj", pn1) & typeof(pn1$jobj) %in% "S4") |
          !(exists("jobj", pn2) & typeof(pn2$jobj) %in% "S4")) {
        return("NOT_A_NUMBER")
      }
      
      .jstrVal(.jcall(phone_util,
                      "Lcom/google/i18n/phonenumbers/PhoneNumberUtil$MatchType;",
                      "isNumberMatch",
                      pn1$jobj, pn2$jobj))
    },
    unclass(e1), unclass(e2),
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE)
  
  if (not_number_na) out[out == "NOT_A_NUMBER"] <- NA_character_
  
  if (!detailed) {
    if (strict)
      out == "EXACT_MATCH"
    else
      ifelse(is.na(out), NA_character_,
             out %in% c("SHORT_NSN_MATCH", "NSN_MATCH", "EXACT_MATCH"))
  } else {
    as.character(out)
  }
}

#' @export
Ops.phone <- function(e1, e2) {
  switch(
    .Generic,
    `==` = is_match(e1, e2, detailed = FALSE),
    `!=` = !is_match(e1, e2, detailed = FALSE),
    stop("`", .Generic, "` not meaningful for `phone` vectors.", call. = FALSE)
  )
}
