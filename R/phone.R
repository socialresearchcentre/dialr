
#' @export
phone <- function(phone, country) {
  if (length(phone) > 1 & length(country) == 1) country <- rep(country, length(phone))
  if (length(phone) != length(country)) stop("`phone` and `country` vectors must be the same length.", call. = FALSE)
  validate_cc(country)
  phone_util <- .get_phoneNumberUtil()
  
  phone <- as.character(phone)
  
  pb <- progress_estimated(length(phone))
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
      phone, country,
      SIMPLIFY = FALSE
    ),
    class = "phone"
  )
  pb$stop()$print()
  
  names(out) <- NULL
  out
}

#' @export
phone_reparse <- function(x) {
  if (!is.phone(x)) stop("`x` must be a vector of class `phone`.")
  phone_util <- .get_phoneNumberUtil()
  
  pb <- progress_estimated(length(x))
  out <- structure(
    lapply(x, function(d) {
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

#' @export
`[.phone` <- function(x, ...) {
  structure(NextMethod(), class = "phone")
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

  x <- vapply(x, function(x) { x$raw }, "")
  if (tot > n) {
    cat(" (showing first ", n, ")\n")
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
  
  out[!is_valid(x)] <- pillar::style_neg(out[!is_valid(x)])
  pillar::new_pillar_shaft_simple(out, align = "right")
}

#' @export
is.phone <- function(x) inherits(x, "phone")

phone_apply <- function(x, fun) {
  pb <- progress_estimated(length(x))
  out <- sapply(x, function(d) {
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
format.phone <- function(x, format = "NATIONAL", home = NULL, clean = TRUE, strict = FALSE, ...) {
  phone_util <- .get_phoneNumberUtil()
  
  out <- phone_apply(x, function(pn) {
    if (is.null(home)) {
      phone_util$format(pn, eval(parse(text = paste0("phone_util$PhoneNumberFormat$", format))))
    } else {
      phone_util$formatOutOfCountryCallingNumber(pn, home)
    }
  })
  if (clean) out <- str_replace_all(out, "[^0-9]", "")
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
    x <- vapply(x, function(x) { x$raw }, "")
    NextMethod()
  } else {
    as.character.default(format(x, clean = TRUE, ...))
  }
}

#' @export
is_parsed <- function(x) {
  if (!is.phone(x)) stop("`x` should be a vector of class `phone`")
  sapply(x, function(pn) { typeof(pn$jobj) %in% "S4" })
}

#' @export
is_valid <- function(x) {
  if (!is.phone(x)) stop("`x` should be a vector of class `phone`")
  phone_util <- .get_phoneNumberUtil()
  
  out <- phone_apply(x, function(pn) {
    res <- phone_util$isValidNumber(pn)
  })
  out[is.na(out)] <- FALSE
  
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
get_type <- function(phone) {
  if (!is.phone(phone)) stop("Phone should be a vector of class `phone`")
  phone_util <- .get_phoneNumberUtil()
  
  out <- phone_apply(phone, function(pn) {
    .jcall(phone_util$getNumberType(pn), "S", "toString")
  })
  
  out
}