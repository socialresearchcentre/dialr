
# rJava helpers
.jset_to_str <- function(o) {
  o %>%
    .jcall("[Ljava/lang/Object;", "toArray") %>%
    vapply(.jstrVal, character(1))
}

# Convenience function - get the current PhoneNumberUtil instance
.get_phoneNumberUtil <- function() {
  .jcall("com/google/i18n/phonenumbers/PhoneNumberUtil", "Lcom/google/i18n/phonenumbers/PhoneNumberUtil;", "getInstance")
}

# PhoneNumberUtil enums
.get_leniency <- function() {
  .jfields(.get_phoneNumberUtil()$Leniency) %>% gsub("^.*\\.", "", .)
}

.get_matchType <- function() {
  .jfields(.get_phoneNumberUtil()$MatchType) %>% gsub("^.*\\.", "", .)
}

.get_phoneNumberFormat <- function() {
  .jfields(.get_phoneNumberUtil()$PhoneNumberFormat) %>% gsub("^.*\\.", "", .)
}

validate_phone_format <- function(x) {
  formats <- .get_phoneNumberFormat()
  if (!all(x %in% formats)) {
    stop(
      "Some `x` values are unsupported formats: ",
      paste0(unique(x[!x %in% formats]), collapse = ", "),
      call. = FALSE
    )
  }
  
  x
}

.get_phone_format_from_string <- function(x) {
  validate_phone_format(x)

  .jcall("com/google/i18n/phonenumbers/PhoneNumberUtil$PhoneNumberFormat",
         "Lcom/google/i18n/phonenumbers/PhoneNumberUtil$PhoneNumberFormat;",
         "valueOf",
         x)
}

.get_phoneNumberType <- function() {
  .jfields(.get_phoneNumberUtil()$PhoneNumberType) %>% gsub("^.*\\.", "", .)
}

validate_phone_type <- function(x) {
  types <- .get_phoneNumberType()
  if (!all(x %in% types)) {
    stop(
      "Some `x` values are unsupported types: ",
      paste0(unique(x[!x %in% types]), collapse = ", "),
      call. = FALSE
    )
  }
  
  x
}

.get_phone_type_from_string <- function(x) {
  validate_phone_type(x)
  
  .jcall("com/google/i18n/phonenumbers/PhoneNumberUtil$PhoneNumberType",
         "Lcom/google/i18n/phonenumbers/PhoneNumberUtil$PhoneNumberType;",
         "valueOf",
         x)
}

.get_validationResult <- function() {
  .jfields(.get_phoneNumberUtil()$ValidationResult) %>% gsub("^.*\\.", "", .)
}

# PhoneNumberUtil "get" functions
.getRegionCodeForCountryCode <- function(x) {
  x <- as.integer(x)
  .get_phoneNumberUtil()$getRegionCodeForCountryCode(x)
}

.getRegionCodesForCountryCode <- function(x) {
  x <- as.integer(x)
  .jset_to_str(.get_phoneNumberUtil()$getRegionCodesForCountryCode(x))
}

.getSupportedCallingCodes <- function() {
  .jset_to_str(.get_phoneNumberUtil()$getSupportedCallingCodes())
}
  
.getSupportedRegions <- function() {
  .jset_to_str(.get_phoneNumberUtil()$getSupportedRegions())
}

validate_phone_country <- function(x) {
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

.getSupportedGlobalNetworkCallingCodes <- function() {
  .jset_to_str(.get_phoneNumberUtil()$getSupportedGlobalNetworkCallingCodes())
}

.getSupportedTypesForRegion <- function(x) {
  x <- as.character(x)
  .jset_to_str(.get_phoneNumberUtil()$getSupportedTypesForRegion(x))
}

.getSupportedTypesForNonGeoEntity <- function(x) {
  x <- as.integer(x)
  .jset_to_str(.get_phoneNumberUtil()$getSupportedTypesForNonGeoEntity(x))
}
