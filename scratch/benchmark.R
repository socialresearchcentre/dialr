
library(dialr)
library(dplyr)
library(tidyr)
library(magrittr)

#-------------------------------------------------------------------------------

# Benchmarking sizes
size_check <-
  bench::press(
    times = c(100, 1000, 10000, 100000),
    {
      bench::mark(parse = phone(rep("0412345678", times = times), "AU"))
    }
  )

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
                  check = FALSE)
    }
  )

#-------------------------------------------------------------------------------

# Benchmarking .jstrVal vs .jcall

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

y <- phone(rep("0412345678", times = 10000), "AU")
get_type_bench <- bench::mark(get_type_new(y), get_type_old(y), iterations = 5)
