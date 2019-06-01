#' Phone number parsing and formatting
#'
#' A phone vector stores phone numbers parsed with libphonenumber for formatting
#' and further processing.
#'
#' libphonenumber defines the `PhoneNumberUtil` class, with a set of functions
#' for extracting information from and performing processing on a parsed
#' `Phonenumber` object. A text phone number must be parsed before any other
#' operations (e.g. checking phone number validity, formatting) can be
#' performed. When parsing a phone number a ["default region"][dialr-region] is
#' required to determine the processing context for non-international numbers.
#'
#' A phone vector stores the raw phone number, the default region and a java
#' `Phonenumber` object for each element. The java object is cached so should
#' persist between R sessions. In case of issues, use `phone_reparse()` to
#' recreate the phone vector from the original phone number and region.
#'
#' Phone number parsing functions display a progress bar in interactive sessions
#' by default. This can be disabled by setting option `dialr.show_progress` to
#' `FALSE`.
#'
#' @section libphonenumber reference:
#'
#'   `phone()`: Phone numbers are parsed using
#'   `PhoneNumberUtil.parseAndKeepRawInput()`. A phone vector stores the
#'   returned `Phonenumber.PhoneNumber` object alongside the original raw text
#'   and default region for later reference.
#'
#'   `format()`: `PhoneNumberUtil.format()` by default, or
#'   `PhoneNumberUtil.formatOutOfCountryCallingNumber()` if `home` is provided.
#'   
#' @param x A character vector of phone numbers.
#' @param region A character vector of [ISO country codes][dialr-region] with
#'   the default region for each phone number in `x`. A `region` vector of
#'   length 1 will be recycled to the length of `x`.
#'   
#'   If `NA` or `""`, numbers written in international format (with a leading
#'   `+`) will be parsed without a default region.
#' @examples
#'   # Create a phone vector
#'   x <- phone(c(0, 0123, "0412 345 678", "61412987654", "03 9123 4567", "+12015550123"), "AU")
#'   
#'   is.phone(x)
#'   print(x)
#'   as.character(x)
#'   format(x)
#'   format(x, home = "AU")
#'   
#'   # Parse international number with no default region
#'   phone("+61412345678", NA)
#'   
#'   # Will fail to parse if number is not in international format
#'   phone("0412345678", NA)
#'   
#'   # A combination can be used
#'   phone(c("+61412345678", "0412345678"), c(NA, "AU"))
#' @name dialr-phone
#' @family phone functions
#' @export
phone <- function(x, region) {
  if (!is.atomic(x))  stop("`x` must be an atomic vector.", call. = FALSE)
  if (length(x) == 0)  stop("`x` must not be empty.", call. = FALSE)
  if (length(x) > 1 & length(region) == 1) region <- rep(region, length(x))
  if (length(x) != length(region)) stop("`x` and `region` vectors must be the same length.", call. = FALSE)
  
  region[!is.na(region) & region == ""] <- NA_character_
  validate_phone_region(region[!is.na(region)])

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
           .jstr_to_char(p),
           r)
  }
  
  show_pb <- isTRUE(getOption("dialr.show_progress")) && interactive()
  
  if (show_pb) pb <- txtProgressBar(min = 0, max = length(x), style = 3)
  out <- structure(
    mapply(
      function(p, r) {
        if (show_pb) setTxtProgressBar(pb, getTxtProgressBar(pb) + 1)
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
  if (show_pb) close(pb)
  
  names(out) <- NULL
  out
}

validate_phone <- function(x) {
  if (!inherits(x, "phone"))
    stop("`x` must be a vector of class `phone`.", call. = FALSE)
  
  x_raw <- unclass(x)
  
  if ((!is.list(x_raw)) | (!all(vapply(x_raw, is.list, logical(1)))))
    stop("`x` must be a list of lists.", call. = FALSE)

  # check structure
  if (!(all(vapply(x_raw, length, integer(1)) == 3L) &
        all(vapply(x_raw, function(x) { exists("raw", x, mode = "character", inherits = FALSE) }, logical(1))) &
        all(vapply(x_raw, function(x) { exists("region", x, mode = "character", inherits = FALSE) }, logical(1))) &
        all(vapply(x_raw, function(x) { exists("jobj", x, inherits = FALSE) }, logical(1))))) {
    stop(
      "The structure of `x` is incorrect.\n",
      "`x` should be a list.\n",
      "All elements of `x` should be named lists with 3 elements:\n",
      " * raw: a character vector of length 1\n",
      " * region: a character vector of length 1\n",
      " * jobj: an S4 `jobjRef` or `NULL`",
      call. = FALSE
    )
  }

  x
}

#' @rdname dialr-phone
#' @export
phone_reparse <- function(x) {
  if (!is.phone(x)) stop("`x` must be a vector of class `phone`.", call. = FALSE)
  
  x <- unclass(x)
  new_phone(vapply(x, function(x) { x$raw }, character(1), USE.NAMES = FALSE),
            vapply(x, function(x) { x$region }, character(1), USE.NAMES = FALSE))
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
  stop("$ operator is invalid for `phone` vectors.", call. = FALSE)
}

#' @export
`[<-.phone` <- function(x, i, value) {
  if (!is.phone(value) & is.atomic(value)) {
    warning("Only `phone` class objects can be inserted into a `phone` vector.\n",
            "The value provided will be converted to `phone` class with default home region `", getOption("dialr.home"), "`.",
            call. = FALSE)
    value <- new_phone(as.character(value),
                       rep(getOption("dialr.home"), length(value)))
  } else if (!is.phone(value) & !is.atomic(value)) {
    stop("Only `phone` class objects can be inserted into a `phone` vector.\n",
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
  stop("$ operator is invalid for `phone` vectors.", call. = FALSE)
}

#' @export
`length<-.phone` <- function(x, value) {
  structure(NextMethod(), class = "phone")
}

#' @export
c.phone <- function(..., recursive = FALSE) {
  out <- lapply(list(...), function(value) {
    if (!is.phone(value)) {
      warning("Only `phone` class objects can be added to a `phone` vector.\n",
              "Atomic vectors will be converted to `phone` class with default home region `", getOption("dialr.home"), "`.\n",
              "Other objects will be dropped.",
              call. = FALSE)
      
      if (is.atomic(value))
        value <- new_phone(as.character(value),
                           rep(getOption("dialr.home"), length(value)))
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
#' @param n Number of elements to print.
#' @param ...	Additional arguments for specific methods.
#' @export
print.phone <- function(x, n = 10, ...) {
  tot <- length(x)

  cat("# Parsed phone numbers: ",
      tot, " total, ",
      sum(is_parsed(x)), " successfully parsed",
      sep = "")

  x_raw <- vapply(unclass(x), function(x) { x$raw }, character(1), USE.NAMES = FALSE)
  if (tot > n) {
    cat(" (showing first ", n, ")\n", sep = "")
    print.default(head(x_raw, n = n), quote = FALSE)
  } else {
    cat("\n")
    print.default(x_raw, quote = FALSE)
  }
  
  invisible(x)
}

#' @rdname dialr-phone
#' @param format Phone number format to use from one of four standards:
#'
#'   * `"E164"`: general format for international telephone numbers from [ITU-T
#'   Recommendation E.164](https://en.wikipedia.org/wiki/E.164)
#'
#'   * `"NATIONAL"`: national notation from [ITU-T Recommendation
#'   E.123](https://en.wikipedia.org/wiki/E.123)
#'
#'   * `"INTERNATIONAL"`: international notation from [ITU-T Recommendation
#'   E.123](https://en.wikipedia.org/wiki/E.123)
#'
#'   * `"RFC3966"`: "tel" URI syntax from the IETF [tel URI for Telephone
#'   Numbers](https://datatracker.ietf.org/doc/rfc3966/)
#'   
#'   See notes from the [libphonenumber
#'   javadocs](https://static.javadoc.io/com.googlecode.libphonenumber/libphonenumber/8.10.10/index.html?com/google/i18n/phonenumbers/PhoneNumberUtil.PhoneNumberFormat.html)
#'   for more details.
#'   
#'   `format` defaults to `"E164"`. The default can be set in option
#'   `dialr.format`.
#'   
#' @param home [ISO country code][dialr-region] for home region. If provided,
#'   numbers will be formatted for dialing from the home region.
#' @param clean Should non-numeric characters be removed? If `TRUE`, keeps
#'   numbers and leading `"+"`.
#' @param strict Should invalid phone numbers be removed? If `TRUE`, invalid
#'   phone numbers are replaced with `NA`.
#' @export
format.phone <- function(x, format = c("E164", "NATIONAL", "INTERNATIONAL", "RFC3966"),
                         home = NULL, clean = TRUE, strict = FALSE, ...) {
  if (identical(format, eval(formals()$format)))
    format <- getOption("dialr.format")
  
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
  }, character(1))
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
#' @param raw If `TRUE`, the raw phone number is returned. Otherwise elements
#'   are cleaned with `format()`.
#' @export
as.character.phone <- function(x, raw = TRUE, ...) {
  if (raw) {
    x <- vapply(unclass(x), function(x) { x$raw }, character(1), USE.NAMES = FALSE)
    NextMethod()
  } else {
    as.character.default(format(x, ...))
  }
}

phone_apply <- function(x, fun, fun.value, progress = FALSE) {
  show_pb <- isTRUE(progress) && isTRUE(getOption("dialr.show_progress")) && interactive()
  if (show_pb) pb <- txtProgressBar(min = 0, max = length(x), style = 3)
  
  out <- vapply(unclass(x), function(d) {
    if (show_pb) setTxtProgressBar(pb, getTxtProgressBar(pb) + 1)
    
    if (!typeof(d$jobj) %in% "S4") return(fun.value[NA])
    fun(d$jobj)
  }, fun.value, USE.NAMES = FALSE)
  
  if (show_pb) close(pb)
  
  out
}

#' Phone number validity checks
#'
#' @description
#'
#' For each element of `x`:
#'
#' * `is_parsed(x)`: Was this successfully parsed?
#'
#' * `is_valid(x)`: Is this a valid phone number?
#'
#' * `is_possible(x)`: Is this a possible phone number? Return type depends on
#' `detailed`.
#' 
#' @details
#' 
#' Possible return values for `is_possible(x, detailed = TRUE)`:
#'
#' * `"INVALID_COUNTRY_CODE"`: The number has an invalid country calling code.
#
#' * `"INVALID_LENGTH"`: The number is longer than the shortest valid numbers
#' for this region, shorter than the longest valid numbers for this region,
#' and does not itself have a number length that matches valid numbers for
#' this region.
#'
#' * `"IS_POSSIBLE"`: The number length matches that of valid numbers for this
#' region.
#'
#' * `"IS_POSSIBLE_LOCAL_ONLY"`: The number length matches that of local numbers
#' for this region only (i.e. numbers that may be able to be dialled within an
#' area, but do not have all the information to be dialled from anywhere inside
#' or outside the country).
#'
#' * `"TOO_LONG"`: The number is longer than all valid numbers for this region.
#'
#' * `"TOO_SHORT"`: The number is shorter than all valid numbers for this
#' region.
#'
#' @section libphonenumber reference:
#'
#'   `is_valid()`: `PhoneNumberUtil.isValidNumber()`
#'
#'   `is_possible()`: `PhoneNumberUtil.isPossibleNumber()`
#'   
#'   `is_possible(detailed = TRUE)`: `PhoneNumberUtil.isPossibleNumberWithReason()`
#'   
#'   `is_possible(type = type)`: `PhoneNumberUtil.isPossibleNumberForType()`
#'   
#'   `is_possible(detailed = TRUE, type = type)`: `PhoneNumberUtil.sPossibleNumberForTypeWthReason()`
#'   
#' @param x A [phone] vector.
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
  if (!is.phone(x)) stop("`x` must be a vector of class `phone`.", call. = FALSE)
  vapply(unclass(x), function(pn) { typeof(pn$jobj) %in% "S4" }, logical(1), USE.NAMES = FALSE)
}

#' @rdname dialr-valid
#' @export
is_valid <- function(x) {
  if (!is.phone(x)) stop("`x` must be a vector of class `phone`.", call. = FALSE)
  phone_util <- .get_phoneNumberUtil()
  
  out <- phone_apply(x, function(pn) {
    .jcall(phone_util, "Z", "isValidNumber", pn)
  }, logical(1))
  out[is.na(out)] <- FALSE
  
  out
}

#' @rdname dialr-valid
#' @param detailed If `FALSE`, `is_possible` returns a logical vector. If
#'   `TRUE`, it returns a character vector with `"IS_POSSIBLE"` or the reason
#'   for failure. See Details section for possible return values.
#' @param type If provided, checks if `x` is possible for the given [phone
#'   number type][dialr-region].
#' @export
is_possible <- function(x, detailed = FALSE, type = NULL) {
  if (!is.phone(x)) stop("`x` must be a vector of class `phone`.", call. = FALSE)
  validate_phone_type(type)
  
  phone_util <- .get_phoneNumberUtil()
  
  if (is.null(type)) {
    if (detailed) {
      out <- phone_apply(x, function(pn) {
        .jstrVal(.jcall(phone_util,
                        "Lcom/google/i18n/phonenumbers/PhoneNumberUtil$ValidationResult;",
                        "isPossibleNumberWithReason",
                        pn))
      }, character(1))
    } else {
      out <- phone_apply(x, function(pn) {
        .jcall(phone_util, "Z", "isPossibleNumber", pn)
      }, logical(1))
    }
  } else {
    type <- .get_phone_type_from_string(type)
    if (detailed) {
      out <- phone_apply(x, function(pn) {
        .jstrVal(.jcall(phone_util,
                        "Lcom/google/i18n/phonenumbers/PhoneNumberUtil$ValidationResult;",
                        "isPossibleNumberForTypeWithReason",
                        pn, type))
      }, character(1))
    } else {
      out <- phone_apply(x, function(pn) {
        .jcall(phone_util, "Z", "isPossibleNumberForType", pn, type)
      }, logical(1))
    }
  }
    
  if (!detailed) out[is.na(out)] <- FALSE
  
  out
}
