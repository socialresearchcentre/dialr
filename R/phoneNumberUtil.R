
# rJava helpers
.jset_to_str <- function(o) {
  vapply(.jcall(o, "[Ljava/lang/Object;", "toArray"),
         .jstrVal, character(1))
}

.phoneNumberUtil <- new.env(parent = emptyenv())

# Convenience function - get the current PhoneNumberUtil instance
.get_phoneNumberUtil <- function() {
  if (is.null(.phoneNumberUtil$phone_util)) {
    .phoneNumberUtil$phone_util <-
      .jcall("com/google/i18n/phonenumbers/PhoneNumberUtil",
             "Lcom/google/i18n/phonenumbers/PhoneNumberUtil;",
             "getInstance")
  }
  .phoneNumberUtil$phone_util
}

# PhoneNumberUtil enums
.get_leniency <- function() {
  gsub("^.*\\.", "", .jfields(.get_phoneNumberUtil()$Leniency))
}

.get_matchType <- function() {
  gsub("^.*\\.", "", .jfields(.get_phoneNumberUtil()$MatchType))
}

.get_phoneNumberFormat <- function() {
  gsub("^.*\\.", "", .jfields(.get_phoneNumberUtil()$PhoneNumberFormat))
}

validate_phone_format <- function(x) {
  if (is.null(.phoneNumberUtil$phone_format)) {
    .phoneNumberUtil$phone_format <- .get_phoneNumberFormat()
  }
  formats <- .phoneNumberUtil$phone_format
  if (!all(x %in% formats)) {
    stop(
      "Some `x` values are unsupported phone formats: ",
      paste0(unique(x[!x %in% formats]), collapse = ", "),
      call. = FALSE
    )
  }
  
  x
}

.get_phone_format_from_string <- function(x) {
  validate_phone_format(x)
  if (is.null(.phoneNumberUtil$phone_formats)) {
    .phoneNumberUtil$phone_formats <-
      .jcall("com/google/i18n/phonenumbers/PhoneNumberUtil$PhoneNumberFormat",
             "[Lcom/google/i18n/phonenumbers/PhoneNumberUtil$PhoneNumberFormat;",
             "values")
    
    names(.phoneNumberUtil$phone_formats) <-
      sapply(.phoneNumberUtil$phone_formats, .jstrVal)
  }
  
  .phoneNumberUtil$phone_formats[[x]]
}

.get_phoneNumberType <- function() {
  gsub("^.*\\.", "", .jfields(.get_phoneNumberUtil()$PhoneNumberType))
}

validate_phone_type <- function(x) {
  if (is.null(.phoneNumberUtil$phone_type)) {
    .phoneNumberUtil$phone_type <- .get_phoneNumberType()
  }
  types <- .phoneNumberUtil$phone_type
  if (!all(x %in% types)) {
    stop(
      "Some `x` values are unsupported phone types: ",
      paste0(unique(x[!x %in% types]), collapse = ", "),
      call. = FALSE
    )
  }
  
  x
}

.get_phone_type_from_string <- function(x) {
  validate_phone_type(x)
  if (is.null(.phoneNumberUtil$phone_types)) {
    .phoneNumberUtil$phone_types <-
      .jcall("com/google/i18n/phonenumbers/PhoneNumberUtil$PhoneNumberType",
             "[Lcom/google/i18n/phonenumbers/PhoneNumberUtil$PhoneNumberType;",
             "values")
    
    names(.phoneNumberUtil$phone_types) <-
      sapply(.phoneNumberUtil$phone_types, .jstrVal)
  }
  
  .phoneNumberUtil$phone_types[[x]]
}

.get_validationResult <- function() {
  gsub("^.*\\.", "", .jfields(.get_phoneNumberUtil()$ValidationResult))
}

# PhoneNumberUtil "get" functions
.getRegionCodeForCountryCode <- function(x) {
  suppressWarnings(x <- as.integer(x))
  .get_phoneNumberUtil()$getRegionCodeForCountryCode(x)
}

.getRegionCodesForCountryCode <- function(x) {
  suppressWarnings(x <- as.integer(x))
  .jset_to_str(.get_phoneNumberUtil()$getRegionCodesForCountryCode(x))
}

.getSupportedCallingCodes <- function() {
  .jset_to_str(.get_phoneNumberUtil()$getSupportedCallingCodes())
}
  
.getSupportedRegions <- function() {
  .jset_to_str(.get_phoneNumberUtil()$getSupportedRegions())
}

validate_phone_country <- function(x) {
  if (is.null(.phoneNumberUtil$phone_country)) {
    .phoneNumberUtil$phone_country <- .getSupportedRegions()
  }
  regions <- .phoneNumberUtil$phone_country
  if (!all(x %in% regions)) {
    stop(
      "Some `x` values are unsupported phone regions: ",
      paste0(unique(x[!x %in% regions]), collapse = ", "),
      call. = FALSE
    )
  }
  
  x
}

.getSupportedGlobalNetworkCallingCodes <- function() {
  .jset_to_str(.get_phoneNumberUtil()$getSupportedGlobalNetworkCallingCodes())
}

.getSupportedTypesForRegion <- function(x) {
  x <- as.character(x)
  .jset_to_str(.get_phoneNumberUtil()$getSupportedTypesForRegion(x))
}

.getSupportedTypesForNonGeoEntity <- function(x) {
  suppressWarnings(x <- as.integer(x))
  .jset_to_str(.get_phoneNumberUtil()$getSupportedTypesForNonGeoEntity(x))
}
