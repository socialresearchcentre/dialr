
ph_raw <- c(0, 0123, "0412 345 678", "61412987654", "03 9123 4567", "+12015550123")
ph_raw_regions <- c(NA, NA, "AU", "AU", "AU", "US")
ph_raw_types <- c(NA, "UNKNOWN", "MOBILE", "MOBILE", "FIXED_LINE", "FIXED_LINE_OR_MOBILE")

ph_raw_timezone <-
  c(NA,
    "Australia/Sydney",
    "Australia/Sydney",
    "Australia/Sydney",
    "Australia/Sydney",
    "America/New_York")

ph_raw_timezone_strict <-
  c(NA,
    NA,
    "Australia/Sydney",
    "Australia/Sydney",
    "Australia/Sydney",
    "America/New_York")

regions <- get_supported_regions()
# BL and MF have a weird phone relationship, exclude from test
regions <- regions[regions != "MF"]
  
types <- get_types_for_region(regions)
names(types) <- regions

# Pre R 3.2.0 there is an issue with equality checks for external pointers.
skip_equal <- function() {
  skip_if(getRversion() < "3.2.0", "skip expect_equal tests pre R 3.2.0 - avoid 'cannot unclass an external pointer' error.")
}
