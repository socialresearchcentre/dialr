#' dialr phone class
#'
#' The phone class parses phone numbers into java objects for further
#' processing in libphonenumber.
#' 
#' libphonenumber defines the `PhoneNumberUtil` class, with a set of functions
#' for extracting information from and performing processing on a parsed
#' `Phonenumber` object. A text phone number must be parsed before any other
#' operations (e.g. checking phone number validity, formatting) can be
#' performed. When parsing a phone number a ["default region"][dialr-region] is
#' required to determine the processing context for non-international numbers.
#'
#' The `phone` class stores the raw phone number, the default region and a java
#' phone object. The java object is cached so should persist between R sessions.
#' In case of issues, use `phone_reparse()` to recreate the `phone` vector from
#' the original phone number and region.
#' 
#' @section libphonenumber reference:
#'
#'   `phone()`: Phone numbers are parsed using
#'   `PhoneNumberUtil.parseAndKeepRawInput()`. The `phone` class stores the
#'   returned `Phonenumber.PhoneNumber` object alongside the original raw text
#'   and default region for later reference.
#'
#'   `format()`: `PhoneNumberUtil.format()` by default, or
#'   `PhoneNumberUtil.formatOutOfCountryCallingNumber()` if `home` is provided.
#'   
#' @param x A character vector of phone numbers.
#' @param region A character vector of [ISO country codes][dialr-region].
#' @examples
#'   x <- phone(c(0, 0123, "0412 345 678", "61412987654", "03 9123 4567", "+12015550123"), "AU")
#'   
#'   is.phone(x)
#'   print(x)
#'   format(x)
#'   format(x, home = "AU")
#' @name dialr-phone
#' @family phone functions
#' @export
phone <- function(x, region) {
  if (!is.atomic(x))  stop("`x` must be an atomic vector.", call. = FALSE)
  if (length(x) == 0)  stop("`x` must not be empty.", call. = FALSE)
  if (length(x) > 1 & length(region) == 1) region <- rep(region, length(x))
  if (length(x) != length(region)) stop("`x` and `region` vectors must be the same length.", call. = FALSE)
  validate_phone_region(region)

  x <- as.character(x)
  validate_phone(new_phone(x, region))
}

#' @importFrom utils txtProgressBar
#' @importFrom utils getTxtProgressBar
#' @importFrom utils setTxtProgressBar
new_phone <- function(x, region) {
  stopifnot(is.character(x))
  stopifnot(is.character(region))
  stopifnot(length(x) == length(region))

  phone_util <- .get_phoneNumberUtil()
  jfunc <- function(p, r) {
    .jcall(phone_util,
           "Lcom/google/i18n/phonenumbers/Phonenumber$PhoneNumber;",
           "parseAndKeepRawInput",
           .jcast(.jnew("java/lang/String", p), "java/lang/CharSequence"),
           r)
  }
  
  pb <- txtProgressBar(min = 0, max = length(x), style = 3)
  out <- structure(
    mapply(
      function(p, r) {
        setTxtProgressBar(pb, getTxtProgressBar(pb) + 1)
        pn <- tryCatch({
          jfunc(p, r)
        }, error = function(e) {
          return(NULL)
        })
        if (is.null(pn))
          pn <- NA
        else
          .jcache(pn)
        
        list(raw = p,
             region = r,
             jobj = pn)
      },
      x, region,
      SIMPLIFY = FALSE
    ),
    class = "phone"
  )
  close(pb)
  
  names(out) <- NULL
  out
}

validate_phone <- function(x) {
  x
}

#' @rdname dialr-phone
#' @export
phone_reparse <- function(x) {
  if (!is.phone(x)) stop("`x` must be a vector of class `phone`.")
  
  x <- unclass(x)
  new_phone(sapply(x, function(x) { x$raw }),
            sapply(x, function(x) { x$country }))
}

#' @rdname dialr-phone
#' @export
is.phone <- function(x) inherits(x, "phone")

#' @export
`[.phone` <- function(x, ...) {
  structure(NextMethod(), class = "phone")
}

#' @export
`[[.phone` <- function(x, ...) {
  `[`(x, ...)
}

#' @export
`$.phone` <- function(x, ...) {
  `[`(x, ...)
}

#' @export
`[<-.phone` <- function(x, i, value) {
  if (!is.phone(value) & is.atomic(value)) {
    warning("Only `phone` class values can be inserted into a `phone` vector.\n",
            "The value will be converted to `phone` class with default home region `", getOption("dialr.home"), "`.",
            call. = FALSE)
    value <- new_phone(as.character(value), getOption("dialr.home"))
  } else if (!is.phone(value) & !is.atomic(value)) {
    stop("Only `phone` class values can be inserted into a `phone` vector.\n",
         "The value provided can not be converted to `phone` class.",
         call. = FALSE)
  }
  
  NextMethod()
}

#' @export
`[[<-.phone` <- function(x, i, value) {
  `[<-`(x, i, value)
}

#' @export
`$<-.phone` <- function(x, i, value) {
  `[<-`(x, i, value)
}

#' @export
c.phone <- function(..., recursive = FALSE) {
  out <- lapply(list(...), function(value) {
    if (!is.phone(value)) {
      warning("Only `phone` class values can be added to a `phone` vector.\n",
              "Atomic vectors will be converted to `phone` class with default home region `", getOption("dialr.home"), "`.\n",
              "Other objects will be dropped.",
              call. = FALSE)
      
      if (is.atomic(value))
        value <- new_phone(as.character(value), getOption("dialr.home"))
      else
        value <- NULL
      
      value
    }
    value
  })
  
  structure(unlist(c(lapply(out, unclass)), recursive = FALSE), class = "phone")
}

#' @export
rep.phone <- function(x, ...) {
  structure(NextMethod(), class = "phone")
}

#' @rdname dialr-phone
#' @param n Number of elements to print 
#' @param ...	Additional arguments for specific methods.
#' @export
print.phone <- function(x, n = 10, ...) {
  tot <- length(x)

  cat("# Parsed phone numbers: ",
      tot, " total, ",
      sum(is_parsed(x)), " successfully parsed",
      sep = "")

  x <- vapply(unclass(x), function(x) { x$raw }, "")
  if (tot > n) {
    cat(" (showing first ", n, ")\n", sep = "")
    print.default(head(x, n = n), quote = FALSE)
  } else {
    cat("\n")
    print.default(x, quote = FALSE)
  }
  
  invisible()
}

# Dynamically exported, see zzz.R
type_sum.phone <- function(x) {
  "phone"
}

# Dynamically exported, see zzz.R
pillar_shaft.phone <- function(x, ...) {
  out <- format(x)
  out[is.na(x)] <- NA
  
  valid <- is_valid(x)
  
  out[!valid] <- pillar::style_neg(out[!valid])
  pillar::new_pillar_shaft_simple(out, align = "right")
}

#' @rdname dialr-phone
#' @param format Phone number format to use based on one of four standards:
#'
#'   * "E164": general format for international telephone numbers from [ITU-T
#'   Recommendation E.164](https://en.wikipedia.org/wiki/E.164)
#'
#'   * "NATIONAL": national notation from [ITU-T Recommendation
#'   E.123](https://en.wikipedia.org/wiki/E.123)
#'
#'   * "INTERNATIONAL": international notation from [ITU-T Recommendation
#'   E.123](https://en.wikipedia.org/wiki/E.123)
#'
#'   * "RFC3966": "tel" URI syntax from the IETF [tel URI for Telephone
#'   Numbers](https://datatracker.ietf.org/doc/rfc3966/)
#'   
#'   see notes from the [libphonenumber javadocs](https://static.javadoc.io/com.googlecode.libphonenumber/libphonenumber/8.10.8/index.html?com/google/i18n/phonenumbers/PhoneNumberUtil.PhoneNumberFormat.html)
#'   for more details.
#'   
#' @param home [ISO country code][dialr-region] for home region. If provided,
#'   numbers will be formatted for dialing from the home region.
#' @param clean Should non-numeric characters be removed? If `TRUE`, keeps
#'   numbers and leading `+`
#' @param strict Should invalid phone numbers be removed? If `TRUE` invalid
#'   numbers are replaced with `NA`
#' @export
format.phone <- function(x, format = c("E164", "NATIONAL", "INTERNATIONAL", "RFC3966"),
                         home = NULL, clean = TRUE, strict = FALSE, ...) {
  format <- match.arg(format)

  validate_phone_format(format)
  validate_phone_region(home)

  phone_util <- .get_phoneNumberUtil()
  format <- .get_phone_format_from_string(format)

  out <- phone_apply(x, function(pn) {
    if (is.null(home)) {
      .jcall(phone_util, "S", "format", pn, format)
    } else {
      .jcall(phone_util, "S", "formatOutOfCountryCallingNumber", pn, home)
    }
  })
  if (clean) out <- gsub("[^+0-9]", "", out)
  if (strict) out[!is_valid(x)] <- NA_character_

  out
}

#' @export
summary.phone <- function(object, ...) {
  out <- c(Class   = "dialr phone",
           Numbers = length(object),
           Parsed  = sum(is_parsed(object)))
  class(out) <- c("table")

  out
}

#' @rdname dialr-phone
#' @param raw If `TRUE`, the raw phone number is returned. Otherwise elements are cleaned with `format()`
#' @export
as.character.phone <- function(x, raw = TRUE, ...) {
  if (raw) {
    x <- vapply(unclass(x), function(x) { x$raw }, "")
    NextMethod()
  } else {
    as.character.default(format(x, ...))
  }
}

phone_apply <- function(x, fun) {
  sapply(unclass(x), function(d) {
    if (!typeof(d$jobj) %in% "S4") return(NA)
    fun(d$jobj)
  })
}

#' Phone number validity checks
#'
#' These functions check the validity of phone numbers.
#'
#' @details
#'
#' For each element of `x`:
#'
#' * `is_parsed(x)`: was this successfully parsed?
#'
#' * `is_valid(x)`: is this a valid phone number?
#'
#' * `is_possible(x)`: is this a possible phone number? Return type depends on
#' `detailed`.
#'
#' @section libphonenumber reference:
#'
#'   `is_valid()`: `PhoneNumberUtil.isValidNumber()`
#'
#'   `is_possible()`: `PhoneNumberUtil.isPossibleNumber()`,
#'   `PhoneNumberUtil.isPossibleNumberWithReason()`,
#'   `PhoneNumberUtil.isPossibleNumberForType()` or
#'   `PhoneNumberUtil.isPossibleNumberForTypeWthReason()` depending on the
#'   provided arguments.
#'   
#' @param x A [`phone`] vector
#' @examples
#'   x <- phone(c(0, 0123, "0412 345 678", "61412987654", "03 9123 4567", "+12015550123"), "AU")
#'
#'   is_parsed(x)
#'   is_valid(x)
#'
#'   is_possible(x)
#'   is_possible(x, detailed = TRUE)
#'
#'   is_possible(x, type = "MOBILE")
#'   is_possible(x, detailed = TRUE, type = "MOBILE")
#' @name dialr-valid
#' @family phone functions
NULL

#' @rdname dialr-valid
#' @export
is_parsed <- function(x) {
  if (!is.phone(x)) stop("`x` should be a vector of class `phone`")
  sapply(unclass(x), function(pn) { typeof(pn$jobj) %in% "S4" })
}

#' @rdname dialr-valid
#' @export
is_valid <- function(x) {
  if (!is.phone(x)) stop("`x` should be a vector of class `phone`")
  phone_util <- .get_phoneNumberUtil()
  
  out <- phone_apply(x, function(pn) {
    .jcall(phone_util, "Z", "isValidNumber", pn)
  })
  out[is.na(out)] <- FALSE
  
  out
}

#' @rdname dialr-valid
#' @param detailed If `FALSE`, `is_possible` returns a logical vector. If
#'   `TRUE`, it returns a character vector with "IS_POSSIBLE" or the reason for
#'   failure.
#' @param type If provided, checks if `x` is possible for the given phone number
#'   type.
#' @export
is_possible <- function(x, detailed = FALSE, type = NULL) {
  if (!is.phone(x)) stop("`x` must be a vector of class `phone`", call. = FALSE)
  validate_phone_type(type)
  
  phone_util <- .get_phoneNumberUtil()
  
  if (is.null(type)) {
    out <- phone_apply(x, function(pn) {
      if (detailed) {
        .jstrVal(.jcall(phone_util,
                        "Lcom/google/i18n/phonenumbers/PhoneNumberUtil$ValidationResult;",
                        "isPossibleNumberWithReason",
                        pn))
      } else {
        .jcall(phone_util, "Z", "isPossibleNumber", pn)
      }
    })
  } else {
    type <- .get_phone_type_from_string(type)
    out <- phone_apply(x, function(pn) {
      if (detailed) {
        .jstrVal(.jcall(phone_util,
                        "Lcom/google/i18n/phonenumbers/PhoneNumberUtil$ValidationResult;",
                        "isPossibleNumberForTypeWithReason",
                        pn, type))
      } else {
        .jcall(phone_util, "Z", "isPossibleNumberForType", pn, type)
      }
    })
  }
  
  if (!detailed) out[is.na(out)] <- FALSE
  
  out
}

#' Phone number regions
#'
#' Functions for working with phone number regions.
#'
#' @details
#'
#' In libphonenumber a phone number region is represented by a 2 digit ISO
#' country code. `get_region(x)` returns the ISO country code for each element
#' of a [phone] vector.
#' 
#' Use `get_supported_regions()` to see a full list of supported regions.
#' 
#' Region can also be retrieved from an international calling code.
#' `get_region_for_calling_code(x)` returns the main region for each provided
#' calling code. Since multiple regions can share a single calling code,
#' `get_regions_for_calling_code(x)` returns a list of character vectors of
#' regions for each.
#'
#' @section libphonenumber reference:
#'
#'   `get_region()`: `PhoneNumberUtil.getRegionCodeForNumber()`
#'
#'   `get_supported_regions()`: `PhoneNumberUtil.getSupportedRegions()`
#'
#'   `get_region_for_calling_code()`:
#'   `PhoneNumberUtil.getRegionCodeForCountryCode()`
#'
#'   `get_regions_for_calling_code()`:
#'   `PhoneNumberUtil.getRegionCodesForCountryCode()`
#'   
#' @param x A [`phone`] vector
#' @examples
#'   x <- phone(c(0, 0123, "0412 345 678", "61412987654", "03 9123 4567", "+12015550123"), "AU")
#'   get_region(x)
#'
#'   get_supported_regions()
#'   
#'   get_region_for_calling_code(c(1, 61, 84))
#'   get_regions_for_calling_code(c(1, 61, 84))
#'   
#' @name dialr-region
#' @family phone functions
#' @export
get_region <- function(x) {
  if (!is.phone(x)) stop("`x` should be a vector of class `phone`")
  phone_util <- .get_phoneNumberUtil()
  
  out <- phone_apply(x, function(pn) {
    res <- .jcall(phone_util, "S", "getRegionCodeForNumber", pn)
    ifelse(is.null(res), NA_character_, res)
  })
  
  out
}

#' @rdname dialr-region
#' @export
get_supported_regions <- function() {
  .getSupportedRegions()
}

#' @rdname dialr-region
#' @export
get_region_for_calling_code <- function(x) {
  validate_phone_calling_code(x)
  vapply(x, .getRegionCodeForCountryCode, "")
}

#' @rdname dialr-region
#' @export
get_regions_for_calling_code <- function(x) {
  validate_phone_calling_code(x)
  lapply(x, .getRegionCodesForCountryCode)
}

#' Phone number types
#'
#' Functions for working with phone number types.
#'
#' @details
#'
#' In addition to validity, libphonenumber can identify phone number type - it
#' is able to distinguish Fixed-line, Mobile, Toll-free, Premium Rate, Shared
#' Cost, VoIP, Personal Numbers, UAN, Pager, and Voicemail (whenever feasible).
#' 
#' `get_type(x)` returns the phone number type for each element of a [phone]
#' vector.
#'
#' Valid phone number types differ by region. `get_types_for_region(x)` returns
#' a list of character vectors of valid types for each provided
#' [ISO country code][dialr-region]. Use `get_supported_types()` to see a full
#' list of supported types.
#'
#' @section libphonenumber reference:
#'
#'   `get_type()`: `PhoneNumberUtil.getNumberType()`
#'   
#'   `get_supported_types()`: `PhoneNumberUtil.PhoneNumberType`
#'   
#'   `get_types_for_region()`: `PhoneNumberUtil.getSupportedTypesForRegion()`
#'   
#' @param x A [`phone`] vector, or a character vector of [ISO country codes][dialr-region]
#' @examples
#'   x <- phone(c(0, 0123, "0412 345 678", "61412987654", "03 9123 4567", "+12015550123"), "AU")
#'   get_type(x)
#'   
#'   get_supported_types()
#'   
#'   get_types_for_region("AU")
#'   get_types_for_region(c("GB", "US"))
#'   get_types_for_region(get_supported_regions())
#'   
#' @name dialr-type
#' @family phone functions
#' @export
get_type <- function(x) {
  if (!is.phone(x)) stop("`x` should be a vector of class `phone`")
  phone_util <- .get_phoneNumberUtil()
  
  out <- phone_apply(x, function(pn) {
    .jstrVal(.jcall(phone_util,
                    "Lcom/google/i18n/phonenumbers/PhoneNumberUtil$PhoneNumberType;",
                    "getNumberType",
                    pn))
  })
  
  out
}

#' @rdname dialr-type
#' @export
get_supported_types <- function() {
  .get_phoneNumberType()
}

#' @rdname dialr-type
#' @export
get_types_for_region <- function(x) {
  validate_phone_region(x)
  lapply(x, .getSupportedTypesForRegion)
}

#' Get an example phone number
#'
#' Produces example phone numbers for the given [`region`][dialr-region] and
#' [`type`][dialr-type] combinations. The `region`, `type` and valid vectors
#' are recycled if a vector of length 1 is provided.
#'
#' @section libphonenumber reference:
#'
#'   `get_example()`: `PhoneNumberUtil.getExampleNumberForType()`;
#'   `PhoneNumberUtil.getExampleNumber()` if `type` is `NULL` or `NA`;
#'   `PhoneNumberUtil.getInvalidExampleNumber()` if `valid` is `FALSE`.
#'
#' @param region A character vector of [ISO country codes][dialr-region].
#' @param type A character vector of [phone number types][dialr-type]. If
#'   `NULL` (default), returns an example "FIXED_LINE" number.
#' @param valid A logical vector. For each `FALSE` entry, `get_example` returns
#'   an example invalid number, and `type` is ignored.
#' @return A [`phone`] vector
#' @examples
#' # Get a basic example number
#' get_example("AU")
#' 
#' # Get an example mobile number
#' get_example("AU", type = "MOBILE")
#' 
#' # Get an example invalid number
#' get_example("AU", valid = FALSE)
#' 
#' # Get a combination of the previous examples
#' get_example(c("AU", "AU",     "AU" ),
#'             c(NA,   "MOBILE", NA   ),
#'             c(TRUE, TRUE,     FALSE))
#' @export
get_example <- function(region, type = NULL, valid = TRUE) {
  vec_length <- max(length(region), length(type), length(valid))
  
  if (vec_length > 1 & length(region) == 1) region <- rep(region, vec_length)
  validate_phone_region(region)
  
  if (is.null(type)) {
    type <- rep(NA_character_, vec_length)
  } else {
    if (vec_length > 1 & length(type) == 1) type <- rep(type, vec_length)
    validate_phone_type(type[!is.na(type)])
  }
  
  if (vec_length > 1 & length(valid) == 1) valid <- rep(valid, vec_length)
  stopifnot(is.logical(valid))
  
  if (!all(length(region) == vec_length,
           length(type) == vec_length,
           length(valid) == vec_length))
    stop("`region`, `type` and `valid` vectors must be the same length where provided.", call. = FALSE)
  
  phone_util <- .get_phoneNumberUtil()
  format <- .get_phone_format_from_string("E164")
  
  out <- structure(
    mapply(function(r, t, v) {
      if (!v) {
        pn <- .jcall(phone_util,
                     "Lcom/google/i18n/phonenumbers/Phonenumber$PhoneNumber;",
                     "getInvalidExampleNumber",
                     r)
      } else if (is.na(t)) {
        pn <- .jcall(phone_util,
                     "Lcom/google/i18n/phonenumbers/Phonenumber$PhoneNumber;",
                     "getExampleNumber",
                     r)
      } else {
        pn <- .jcall(phone_util,
                     "Lcom/google/i18n/phonenumbers/Phonenumber$PhoneNumber;",
                     "getExampleNumberForType",
                     r, .get_phone_type_from_string(t))
      }
      .jcache(pn)
      
      p <- .jcall(phone_util, "S", "format", pn, format)
      
      list(raw = p,
           region = r,
           jobj = pn)
    },
    region, type, valid,
    SIMPLIFY = FALSE),
    class = "phone"
  )
  
  names(out) <- NULL
  out
}
