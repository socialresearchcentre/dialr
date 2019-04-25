
ph_parse <- function(phone, country, keep_raw = TRUE) {
  if (length(phone) > 1 & length(country) == 1) country <- rep(country, length(phone))
  if (length(phone) != length(country)) stop("Phone and country vectors must be the same length")
  validate_phone_region(country)
  phone_util <- .get_phoneNumberUtil()
  
  out <- mapply(function(p, c) {
    if (keep_raw) {
      phone_util$parseAndKeepRawInput(p, c)
    } else {
      phone_util$parse(p, c)
    }
  },
  phone, country,
  SIMPLIFY = TRUE)
  
  names(out) <- NULL
  out
  
}

#' Check if a phone number is valid for a given region
#'
#' **ph_valid is soft-deprecated and will be removed in a future release.**
#'
#' @param phone Character vector of phone numbers to check
#' @param country Character vector of region codes. If length = 1, the same
#'                region code is used for all phone numbers
#' @param strict If strict = FALSE, ph_valid checks if the phone number is valid
#'               to dial within the given region, including correctly specified
#'               international numbers. If strict = TRUE, ph_valid checks if the
#'               phone number is a domestic phone number for the given region.
#' @return logical vector flagging valid and invalid phone numbers
#' @export
ph_valid <- function(phone, country, strict = FALSE) {
  warning("ph_valid() is soft deprecated as of dialr 0.2.0\nplease use is_valid() instead", call. = FALSE)
  
  if (length(phone) > 1 & length(country) == 1) country <- rep(country, length(phone))
  if (length(phone) != length(country)) stop("Phone and country vectors must be the same length")
  validate_phone_region(country)
  phone_util <- .get_phoneNumberUtil()
  
  out <- mapply(function(p, c) {
    pn <- tryCatch({
      phone_util$parse(p, c)
    }, error = function(e) {
      return(NULL)
    })
    if (is.null(pn)) return(NA)
    
    if (strict) {
      phone_util$isValidNumberForRegion(pn, c)
    } else {
      phone_util$isValidNumber(pn)
    }
  },
  phone, country,
  SIMPLIFY = TRUE)
  
  names(out) <- NULL
  out
}

#' Retrieve region for phone number
#'
#' **ph_region is soft-deprecated and will be removed in a future release.**
#'
#' @param phone Character vector of phone numbers.
#' @param country Character vector of region codes. If length = 1, the same
#'                region code is used for all phone numbers.
#' @return character vector containing the country code for each phone number.
#' @export
ph_region <- function(phone, country) {
  warning("ph_region() is soft deprecated as of dialr 0.2.0\nplease use get_region() instead", call. = FALSE)
  
  if (length(phone) > 1 & length(country) == 1) country <- rep(country, length(phone))
  if (length(phone) != length(country)) stop("Phone and country vectors must be the same length")
  validate_phone_region(country)
  phone_util <- .get_phoneNumberUtil()
  
  out <- mapply(function(p, c) {
    pn <- tryCatch({
      phone_util$parse(p, c)
    }, error = function(e) {
      return(NULL)
    })
    if (is.null(pn)) return(NA)
    
    res <- phone_util$getRegionCodeForNumber(pn)
    ifelse(is.null(res), NA, res)
  },
  phone, country,
  SIMPLIFY = TRUE)
  
  names(out) <- NULL
  out
}

#' Check if a phone number is possible for a given region
#'
#' **ph_possible is soft-deprecated and will be removed in a future release.**
#'
#' Check whether a phone number is a possible number. It provides a more lenient
#' check than isValidNumber(PhoneNumber) in the following sense:
#' \enumerate{
#'   \item It only checks the length of phone numbers. In particular, it doesn't
#'         check starting digits of the number.
#'   \item It doesn't attempt to figure out the type of the number, but uses
#'         general rules which applies to all types of phone numbers in a
#'         region. Therefore, it is much faster than isValidNumber.
#'   \item For fixed line numbers, many regions have the concept of area code,
#'         which together with subscriber number constitute the national
#'         significant number. It is sometimes okay to dial the subscriber
#'         number only when dialing in the same area. This function will return
#'         true if the subscriber-number-only version is passed in. On the other
#'         hand, because isValidNumber validates using information on both
#'         starting digits (for fixed line numbers, that would most likely be
#'         area codes) and length (obviously includes the length of area codes
#'         for fixed line numbers), it will return false for the
#'         subscriber-number-only version.
#' }
#'
#' @param phone Character vector of phone numbers to check
#' @param country Character vector of region codes. If length = 1, the same
#'                region code is used for all phone numbers
#' @param detailed If detailed = FALSE, ph_possible returns a logical vector
#'                 that indicates whether the number is possible or not.
#'                 If detailed = TRUE, ph_possible returns more detailed
#'                 information about FALSE numbers.
#' @return if detailed = FALSE, logical vector flagging possible phone numbers.
#'         If detailed = TRUE, character vector with detailed information about
#'         failures.
#' @export
ph_possible <- function(phone, country, detailed = FALSE) {
  warning("ph_possible() is soft deprecated as of dialr 0.2.0\nplease use is_possible() instead", call. = FALSE)
  
  if (length(phone) > 1 & length(country) == 1) country <- rep(country, length(phone))
  if (length(phone) != length(country)) stop("Phone and country vectors must be the same length")
  validate_phone_region(country)
  phone_util <- .get_phoneNumberUtil()
  
  out <- mapply(function(p, c) {
    if (detailed) {
      pn <- tryCatch({
        phone_util$parse(p, c)
      }, error = function(e) {
        return(NULL)
      })
      if (is.null(pn)) return(NA)
      
      .jcall(phone_util$isPossibleNumberWithReason(pn), "S", "toString")
    } else {
      phone_util$isPossibleNumber(p, c)
    }
  },
  phone, country,
  SIMPLIFY = TRUE)
  
  names(out) <- NULL
  out
}

#' Format a phone number
#'
#' **ph_format is soft-deprecated and will be removed in a future release.**
#'
#' @param phone Character vector of phone numbers to check
#' @param format Phone number format to use.
#' @param country Character vector of region codes. If length = 1, the same
#'                region code is used for all phone numbers
#' @param home ISO country code for home country. If provided, numbers will be
#'   formatted for dialing from the home country.
#' @param clean Should non-numeric characters be removed? If `TRUE`, keeps
#'   numbers
#' @return a character vector of formatted phone numbers.
#' @export
ph_format <- function(phone, country, format = "NATIONAL", home = NULL, clean = FALSE) {
  warning("ph_format() is soft deprecated as of dialr 0.2.0\nplease use format.phone() instead", call. = FALSE)
  
  if (length(phone) > 1 & length(country) == 1) country <- rep(country, length(phone))
  if (length(phone) != length(country)) stop("Phone and country vectors must be the same length")
  validate_phone_region(country)
  phone_util <- .get_phoneNumberUtil()
  
  out <- mapply(function(p, c) {
    pn <- tryCatch({
      phone_util$parse(p, c)
    }, error = function(e) {
      return(NULL)
    })
    if (is.null(pn)) return(NA)
    
    if (is.null(home)) {
      phone_util$format(pn, eval(parse(text = paste0("phone_util$PhoneNumberFormat$", format))))
    } else {
      phone_util$formatOutOfCountryCallingNumber(pn, home)
    }
  },
  phone, country,
  SIMPLIFY = TRUE)
  
  names(out) <- NULL
  if (clean) out <- gsub("[^0-9]", "", out)
  
  out
}

#' Retrieve type for phone number
#'
#' **ph_type is soft-deprecated and will be removed in a future release.**
#'
#' @param phone Character vector of phone numbers.
#' @param country Character vector of region codes. If length = 1, the same
#'                region code is used for all phone numbers.
#' @return character vector containing the phone number type for each phone number.
#' @export
ph_type <- function(phone, country) {
  warning("ph_type() is soft deprecated as of dialr 0.2.0\nplease use get_type() instead", call. = FALSE)
  
  if (length(phone) > 1 & length(country) == 1) country <- rep(country, length(phone))
  if (length(phone) != length(country)) stop("Phone and country vectors must be the same length")
  validate_phone_region(country)
  phone_util <- .get_phoneNumberUtil()
  
  out <- mapply(function(p, c) {
    pn <- tryCatch({
      phone_util$parse(p, c)
    }, error = function(e) {
      return(NULL)
    })
    if (is.null(pn)) return(NA)
    
    .jcall(phone_util$getNumberType(pn), "S", "toString")
  },
  phone, country,
  SIMPLIFY = TRUE)
  
  names(out) <- NULL
  
  out
}

#' Get example phone numbers
#'
#' **ph_example is soft-deprecated and will be removed in a future release.**
#'
#' @param country Character vector of region codes. If length = 1, the same
#'                region code is used for all phone numbers
#' @param type A character vector of phone number types. If `NULL` (default),
#'   returns an example "FIXED_LINE" number.
#' @param home ISO country code for home country. If provided, numbers will be
#'   formatted for dialing from the home country.
#' @param clean Should non-numeric characters be removed? If `TRUE`, keeps
#'   numbers
#' @return a character vector of formatted example phone numbers.
#' @export
ph_example <- function(country, type = NULL, home = NULL, clean = FALSE) {
  warning("ph_example() is soft deprecated as of dialr 0.2.0\nplease use get_example() instead", call. = FALSE)
  
  if (length(type) > 1 & length(country) == 1) country <- rep(country, length(type))
  if (length(country) > 1 & length(type) == 1) type <- rep(type, length(country))
  if (is.null(type)) type <- rep(NA, length(country))
  if (length(country) != length(type) & !is.null(type)) stop("Country and type vectors must be the same length")
  validate_phone_region(country)
  phone_util <- .get_phoneNumberUtil()
  
  out <- mapply(function(c, t) {
    if (is.na(t)) {
      pn <- phone_util$getExampleNumber(c)
    } else {
      pn <- phone_util$getExampleNumberForType(c, eval(parse(text = paste0("phone_util$PhoneNumberType$", t))))
    }
    
    if (is.null(home)) {
      home <- c
    }
    phone_util$formatOutOfCountryCallingNumber(pn, c)
  },
  country, type,
  SIMPLIFY = TRUE)
  
  names(out) <- NULL
  if (clean) out <- gsub("[^0-9]", "", out)
  
  out
}
