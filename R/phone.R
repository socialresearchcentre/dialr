
#' @export
phone <- function(phone, country) {
  if (length(phone) > 1 & length(country) == 1) country <- rep(country, length(phone))
  if (length(phone) != length(country)) stop("Phone and country vectors must be the same length")
  validate_cc(country)
  phone_util <- .get_phoneNumberUtil()
  
  phone <- as.character(phone)
  
  out <- structure(
    mapply(
      function(p, c) {
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
  
  names(out) <- NULL
  out
}

#' @export
phone_reparse <- function(x) {
  if (!is.phone(x)) stop("Phone should be a vector of class `phone`")
  phone_util <- .get_phoneNumberUtil()
  
  structure(
    lapply(x, function(d) {
      if (is.jnull(d$jobj)) {
        phone_util <- .get_phoneNumberUtil()
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
}

#' @export
`[.phone` <- function(x, ...) {
  structure(NextMethod(), class = "phone")
}

#' @export
print.phone <- function(x, ...) {
  cat("# Parsed phone numbers: ",
      length(x), " total\n",
      # length(x), " total, ",
      # sum(is_valid(x)), " valid\n",
      sep = "")
  # print(table(get_region(x), get_type(x), useNA = "always"))
  
  x <- vapply(x, function(x) { x$raw }, "")
  print.default(x, quote = FALSE)
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
  sapply(x, function(d) {
    # Re-parse if phone jobjs have expired (e.g. reloading a data frame from memory)
    if (is.jnull(d$jobj)) stop("Your phone vector needs to be reparsed. ",
                               "This is usually caused by loading a phone object from disk. ",
                               "Please run `phone_reparse` on the vector to get it working again.")
    if (!typeof(d$jobj) %in% "S4") return(NA)
    fun(d$jobj)
  })
}

#' @export
format.phone <- function(x, format = "NATIONAL", home = NULL, clean = TRUE, strict = FALSE, ...) {
  phone_util <- .get_phoneNumberUtil()
  
  out <- phone_apply(x, function(pn) {
    if (is.null(home)) {
      phone_util$format(pn, eval(parse(text=paste0("phone_util$PhoneNumberFormat$", format))))
    } else {
      phone_util$formatOutOfCountryCallingNumber(pn, home)
    }
  })
  if (clean) out <- str_replace_all(out, "[^0-9]", "")
  if (strict) out[!is_valid(x)] <- NA
  
  out
}

# #' @export
# summary.phone <- function(object, ...) {
#   cat("<Parsed phone number field>\n", sep = "")
#   out <- c(Numbers = length(object), Valid = sum(is_valid(object)))
#   class(out) <- "table"
#   print(out)
# 
#   print(table(get_region(x$phone1), get_type(x$phone1), useNA = "always"))
#   
#   invisible(NextMethod())
# }

# #' @export
# as.character.phone <- function(x, raw = FALSE, ...) {
#   if (raw) {
#     NextMethod()
#   } else {
#     as.character.default(format(x, clean = T, ...))
#   }
# }

#' @export
is_valid <- function(phone) {
  if (!is.phone(phone)) stop("Phone should be a vector of class `phone`")
  phone_util <- .get_phoneNumberUtil()
  
  out <- phone_apply(phone, function(pn) {
    res <- phone_util$isValidNumber(pn)
  })
  out[is.na(out)] <- FALSE
  
  out
}

#' @export
get_region <- function(phone) {
  if (!is.phone(phone)) stop("Phone should be a vector of class `phone`")
  phone_util <- .get_phoneNumberUtil()
  
  out <- phone_apply(phone, function(pn) {
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