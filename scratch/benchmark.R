
library(dialr)
library(dplyr)
library(tidyr)
library(magrittr)

#-------------------------------------------------------------------------------

# Benchmarking sizes
size_check <-
  bench::press(
    times = c(10, 100, 1000, 10000, 100000),
    {
      bench::mark(parse = phone(rep("0412345678", times = times), "AU"))
    }
  )

size_check %>% select(-result:gc)

bench_check <-
  bench::press(
    times = c(10, 100, 1000, 10000, 100000),
    {
      x <- phone(rep("0412345678", times = times), "AU")
      bench::mark(is_valid(x),
                  is_possible(x),
                  get_region(x),
                  get_type(x),
                  format(x),
                  format(x, home = "AU"),
                  check = FALSE)
    }
  )

bench_check %>% select(-result:gc)

#-------------------------------------------------------------------------------

# Benchmarking .jstrVal vs .jcall

get_type_new2 <- function(x) {
  if (!is.phone(x)) stop("`x` should be a vector of class `phone`")
  phone_util <- .get_phoneNumberUtil()
  func <- phone_util$getNumberType
  
  out <- phone_apply(x, function(pn) {
    .jstrVal(func(pn))
  })
  
  out
}

get_type_new <- function(x) {
  if (!is.phone(x)) stop("`x` should be a vector of class `phone`")
  phone_util <- .get_phoneNumberUtil()
  
  out <- phone_apply(x, function(pn) {
    .jstrVal(phone_util$getNumberType(pn))
  })
  
  out
}

get_type_old <- function(x) {
  if (!is.phone(x)) stop("`x` should be a vector of class `phone`")
  phone_util <- .get_phoneNumberUtil()
  
  out <- phone_apply(x, function(pn) {
    .jcall(phone_util$getNumberType(pn), "S", "toString")
  })
  
  out
}

y <- phone(rep("0412345678", times = 1000), "AU")
get_type_bench <- bench::mark(get_type_new(y), get_type_old(y), get_type_new2(y), iterations = 5)

#-------------------------------------------------------------------------------

# Store function signature on phone parse
new_phone_new <- function(x, region) {
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
  
  show_pb <- isTRUE(getOption("dialr.show_progress")) && interactive()
  
  if (show_pb) pb <- txtProgressBar(min = 0, max = length(x), style = 3)
  out <- structure(
    mapply(
      function(p, r) {
        if (show_pb) setTxtProgressBar(pb, getTxtProgressBar(pb) + 1)
        pn <- tryCatch({
          jfunc(p, r)
        }, error = function(e) {
          return(e$jobj)
        })
        
        if (.jclass(pn) == "com.google.i18n.phonenumbers.NumberParseException") {
          e <- pn
          pn <- NA
        } else {
          e <- NA
          .jcache(pn)
        }
        
        list(raw = p,
             region = r,
             jobj = pn,
             err = e)
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

is_parsed <- function(x, detailed = FALSE) {
  if (!is.phone(x)) stop("`x` must be a vector of class `phone`", call. = FALSE)
  
  if (detailed)
    sapply(unclass(x), function(pn) {
      if (typeof(pn$jobj) %in% "S4")
        "IS_PARSED"
      else
        .jstrVal(.jcall(pn$err,
                        "Lcom/google/i18n/phonenumbers/NumberParseException$ErrorType;",
                        "getErrorType"))
    }, USE.NAMES = FALSE)
  else
    sapply(unclass(x), function(pn) { typeof(pn$jobj) %in% "S4" }, USE.NAMES = FALSE)
}

new_phone_bench <- 
  bench::press(
    times = c(10, 100, 1000),
    {
      bench::mark(#new_phone_old(rep(c("0412345678", NA), times = times / 2), rep("AU", times = times)),
                  new_phone_new(rep(c("0412345678", NA), times = times / 2), rep("AU", times = times)),
                  new_phone(rep(c("0412345678", NA), times = times / 2), rep("AU", times = times)),
                  new_phone_new(rep("0412345678", times = times), rep("AU", times = times)),
                  new_phone(rep("0412345678", times = times), rep("AU", times = times)),
                  new_phone_new(rep(NA_character_, times = times), rep("AU", times = times)),
                  new_phone(rep(NA_character_, times = times), rep("AU", times = times)),
                  iterations = 5, check = FALSE)
    }
  )

#-------------------------------------------------------------------------------

phone_apply_new <- function(x, fun, fun.value) {
  pb <- progress_estimated(length(x))
  out <- vapply(unclass(x), function(d) {
    pb$tick()$print()
    # Re-parse if phone jobjs have expired (e.g. reloading a data frame from memory)
    # if (is.jnull(d$jobj)) stop("The `phone` vector in `x` needs to be reparsed. ",
    #                            "This is usually caused by loading a `phone` object from disk. ",
    #                            "Please run `phone_reparse()` on `x` to get it working again.")
    if (!typeof(d$jobj) %in% "S4") return(NA)
    fun(d$jobj)
  }, fun.value)
  pb$stop()$print()
  out
}

get_region_new <- function(x) {
  if (!is.phone(x)) stop("`x` should be a vector of class `phone`")
  phone_util <- .get_phoneNumberUtil()
  
  out <- phone_apply_new(x, function(pn) {
    res <- .jcall(phone_util, "S", "getRegionCodeForNumber", pn)
    ifelse(is.null(res), NA, res)
  }, "")
  
  out
}

phone_apply_parallel <- function(x, fun) {
  pb <- progress_estimated(length(x))
  out <- parallel::mcmapply(function(d) {
    pb$tick()$print()
    # Re-parse if phone jobjs have expired (e.g. reloading a data frame from memory)
    if (is.jnull(d$jobj)) stop("The `phone` vector in `x` needs to be reparsed. ",
                               "This is usually caused by loading a `phone` object from disk. ",
                               "Please run `phone_reparse()` on `x` to get it working again.")
    if (!typeof(d$jobj) %in% "S4") return(NA)
    fun(d$jobj)
  }, unclass(x))
  pb$stop()$print()
  out
}

get_region_parallel <- function(x) {
  if (!is.phone(x)) stop("`x` should be a vector of class `phone`")
  phone_util <- dialr:::.get_phoneNumberUtil()
  
  out <- phone_apply_parallel(x, function(pn) {
    res <- phone_util$getRegionCodeForNumber(pn)
    ifelse(is.null(res), NA, res)
  })
  
  out
}

y <- phone(rep("0412345678", times = 1000), "AU")
phone_apply_bench <- bench::mark(get_region(y), get_region_new(y), iterations = 5)
