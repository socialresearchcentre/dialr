#' dialr phone class
#'
#' The phone class parses phone numbers and stores the java `Phonenumber` object
#' alongside the original raw text.
#' 
#' @param x a character vector of phone numbers.
#' @param country a character vector of ISO country codes.
#' @return 
#' 
#' @examples
#'   x <- phone(c(0, 0123, "0412 345 678", "61412987654", "03 9123 4567", "+12015550123"), "AU")
#'   is_parsed(x)
#'   is_valid(x)
#'   is_possible(x)
#'   get_region(x)
#'   get_type(x)
#'   format(x)
#'   format(x, home = "AU")
#' @name dialr-phone
#' @importFrom dplyr progress_estimated
#' @export
phone <- function(x, country) {
  if (!is.atomic(x))  stop("`x` must be an atomic vector.", call. = FALSE)
  if (length(x) == 0)  stop("`x` must not be empty.", call. = FALSE)
  if (length(x) > 1 & length(country) == 1) country <- rep(country, length(x))
  if (length(x) != length(country)) stop("`x` and `country` vectors must be the same length.", call. = FALSE)
  validate_phone_country(country)

  x <- as.character(x)
  validate_phone(new_phone(x, country))
}

new_phone <- function(x, country) {
  stopifnot(is.character(x))
  stopifnot(is.character(country))
  stopifnot(length(x) == length(country))

  phone_util <- .get_phoneNumberUtil()
  jfunc <- function(p, c) {
    .jcall(phone_util,
           "Lcom/google/i18n/phonenumbers/Phonenumber$PhoneNumber;",
           "parseAndKeepRawInput",
           .jcast(.jnew("java/lang/String", p), "java/lang/CharSequence"),
           c)
  }
  
  pb <- progress_estimated(length(x))
  out <- structure(
    mapply(
      function(p, c) {
        pb$tick()$print()
        pn <- tryCatch({
          jfunc(p, c)
        }, error = function(e) {
          return(NULL)
        })
        if (is.null(pn))
          pn <- NA
        else
          .jcache(pn)
        
        list(raw = p,
             country = c,
             jobj = pn)
      },
      x, country,
      SIMPLIFY = FALSE
    ),
    class = "phone"
  )
  pb$stop()$print()
  
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
  
  phone_util <- .get_phoneNumberUtil()
  jfunc <- function(p, c) {
    .jcall(phone_util,
           "Lcom/google/i18n/phonenumbers/Phonenumber$PhoneNumber;",
           "parseAndKeepRawInput",
           .jcast(.jnew("java/lang/String", p), "java/lang/CharSequence"),
           c)
  }
  
  pb <- progress_estimated(length(x))
  out <- structure(
    lapply(unclass(x), function(d) {
      pb$tick()$print()
      if (is.jnull(d$jobj)) {
        pn <- tryCatch({
          jfunc(d$raw, d$country)
        }, error = function(e) {
          return(NULL)
        })
        if (is.null(pn)) pn <- NA
        d$jobj <- pn
      }
      d
    }),
    class = "phone"
  )
  pb$stop()$print()
  out
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
            "The value will be converted to `phone` class with default home country `", getOption("dialr.home"), "`.",
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
              "Atomic vectors will be converted to `phone` class with default home country `", getOption("dialr.home"), "`.\n",
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
#' @param n number of elements to print 
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

#' @importFrom pillar type_sum
#' @export
type_sum.phone <- function(x) {
  "phone"
}

#' @importFrom pillar pillar_shaft
#' @export
pillar_shaft.phone <- function(x, ...) {
  out <- format(x)
  out[is.na(x)] <- NA
  
  valid <- is_valid(x)
  
  out[!valid] <- pillar::style_neg(out[!valid])
  pillar::new_pillar_shaft_simple(out, align = "right")
}

#' @rdname dialr-phone
#' @param format phone number format to use.
#' @param home ISO country code for home country. If provided, numbers will be formatted for dialing from the home country.
#' @param clean should non-numeric characters be removed? If `TRUE`, keeps numbers and leading `+`
#' @param strict should invalid phone numbers be removed? If `TRUE` invalid numbers are replaced with `NA`
#' @export
format.phone <- function(x, format = "NATIONAL", home = NULL, clean = TRUE, strict = FALSE, ...) {
  validate_phone_format(format)
  validate_phone_country(home)
  
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
#' @param raw if `TRUE`, the raw phone number is returned. Otherwise elements are cleaned with `format()`
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
  pb <- progress_estimated(length(x))
  out <- sapply(unclass(x), function(d) {
    pb$tick()$print()
    # # Re-parse if phone jobjs have expired (e.g. reloading a data frame from memory)
    # if (is.jnull(d$jobj)) stop("The `phone` vector in `x` needs to be reparsed. ",
    #                            "This is usually caused by loading a `phone` object from disk. ",
    #                            "Please run `phone_reparse()` on `x` to get it working again.")
    if (!typeof(d$jobj) %in% "S4") return(NA)
    fun(d$jobj)
  })
  pb$stop()$print()
  out
}

#' @export
is_parsed <- function(x) {
  if (!is.phone(x)) stop("`x` should be a vector of class `phone`")
  sapply(unclass(x), function(pn) { typeof(pn$jobj) %in% "S4" })
}

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

#' @export
is_possible <- function(x, detailed = FALSE, type = NULL) {
  if (!is.phone(x)) stop("`x` should be a vector of class `phone`")
  validate_phone_type(type)
  phone_util <- .get_phoneNumberUtil()
  
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
  
  if (!detailed) out[is.na(out)] <- FALSE
  
  out
}

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
