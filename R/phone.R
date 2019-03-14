#' dialr phone class
#' 
#' The 
#' 
#' @param phone
#' @param country
#' @return 
#' 
#' @examples
#'   x <- phone(c(0, 0123, "0412 345 678", "61412987654", "03 9123 4567", "+12015550123"), "AU")
#'   is_parsed(x)
#'   is_valid(x)
#'   is.possible(x)
#'   get_region(x)
#'   get_type(x)
#'   format(x)
#'   format(x, home = "AU")
#' @importFrom dplyr progress_estimated
#' @export
phone <- function(x, country) {
  if (length(x) > 1 & length(country) == 1) country <- rep(country, length(x))
  if (length(x) != length(country)) stop("`x` and `country` vectors must be the same length.", call. = FALSE)

  validate_country(country)

  x <- as.character(x)
  validate_phone(new_phone(x, country))
}

new_phone <- function(x, country) {
  stopifnot(is.character(x))
  stopifnot(is.character(country))
  stopifnot(length(x) == length(country))

  phone_util <- .get_phoneNumberUtil()

  pb <- progress_estimated(length(x))
  out <- structure(
    mapply(
      function(p, c) {
        pb$tick()$print()
        pn <- tryCatch({
          phone_util$parseAndKeepRawInput(p, c)
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

validate_country <- function(x) {
  regions <- .getSupportedRegions()
  if (!all(x %in% regions)) {
    stop(
      "Some `x` values are unsupported regions: ",
      paste0(unique(x[!x %in% regions]), collapse = ", "),
      call. = FALSE
    )
  }
  
  x
}

#' @export
phone_reparse <- function(x) {
  if (!is.phone(x)) stop("`x` must be a vector of class `phone`.")
  phone_util <- .get_phoneNumberUtil()
  
  pb <- progress_estimated(length(x))
  out <- structure(
    lapply(unclass(x), function(d) {
      pb$tick()$print()
      if (is.jnull(d$jobj)) {
        pn <- tryCatch({
          phone_util$parseAndKeepRawInput(d$raw, d$country)
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

phone_apply <- function(x, fun) {
  pb <- progress_estimated(length(x))
  out <- sapply(unclass(x), function(d) {
    pb$tick()$print()
    # Re-parse if phone jobjs have expired (e.g. reloading a data frame from memory)
    if (is.jnull(d$jobj)) stop("The `phone` vector in `x` needs to be reparsed. ",
                               "This is usually caused by loading a `phone` object from disk. ",
                               "Please run `phone_reparse()` on `x` to get it working again.")
    if (!typeof(d$jobj) %in% "S4") return(NA)
    fun(d$jobj)
  })
  pb$stop()$print()
  out
}

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
  if (!is.phone(value)) {
    warning("Only `phone` class values can be inserted into a `phone` vector.\n",
            "The value will be converted to `phone` class with default home country `", getOption("dialr.home"), "`.",
            call. = FALSE)
    value <- phone(value, getOption("dialr.home"))
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
      warning("Only `phone` class values can be to a `phone` vector.\n",
              "Atomic vectors will be converted to `phone` class with default home country `", getOption("dialr.home"), "`.\n",
              "Other objects will be dropped.",
              call. = FALSE)
      
      if (is.atomic(value))
        value <- phone(value, getOption("dialr.home"))
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

#' @export
format.phone <- function(x, format = "NATIONAL", home = NULL, clean = TRUE, strict = FALSE, ...) {
  phone_util <- .get_phoneNumberUtil()
  
  out <- phone_apply(x, function(pn) {
    if (is.null(home)) {
      phone_util$format(pn, eval(parse(text = paste0("phone_util$PhoneNumberFormat$", format))))
    } else {
      phone_util$formatOutOfCountryCallingNumber(pn, home)
    }
  })
  if (clean) out <- gsub("[^+0-9]", "", out)
  if (strict) out[!is_valid(x)] <- NA
  
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

#' @export
as.character.phone <- function(x, raw = TRUE, ...) {
  if (raw) {
    x <- vapply(unclass(x), function(x) { x$raw }, "")
    NextMethod()
  } else {
    as.character.default(format(x, clean = TRUE, ...))
  }
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
    phone_util$isValidNumber(pn)
  })
  out[is.na(out)] <- FALSE
  
  out
}

#' @export
is_possible <- function(x, detailed = FALSE, type = NULL) {
  if (!is.phone(x)) stop("`x` should be a vector of class `phone`")
  phone_util <- .get_phoneNumberUtil()
  
  out <- phone_apply(x, function(pn) {
    if (detailed) {
      .jstrVal(phone_util$isPossibleNumberWithReason(pn))
    } else {
      phone_util$isPossibleNumber(pn)
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
    res <- phone_util$getRegionCodeForNumber(pn)
    ifelse(is.null(res), NA, res)
  })
  
  out
}

#' @export
get_type <- function(x) {
  if (!is.phone(x)) stop("`x` should be a vector of class `phone`")
  phone_util <- .get_phoneNumberUtil()
  
  out <- phone_apply(x, function(pn) {
    .jstrVal(phone_util$getNumberType(pn))
  })
  
  out
}