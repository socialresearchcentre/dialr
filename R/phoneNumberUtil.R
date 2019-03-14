
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
  phone_util <- .get_phoneNumberUtil()
  .jfields(phone_util$Leniency) %>% gsub("^.*\\.", "", .)
}

.get_matchType <- function() {
  phone_util <- .get_phoneNumberUtil()
  .jfields(phone_util$MatchType) %>% gsub("^.*\\.", "", .)
}

.get_phoneNumberFormat <- function() {
  phone_util <- .get_phoneNumberUtil()
  .jfields(phone_util$PhoneNumberFormat) %>% gsub("^.*\\.", "", .)
}

.get_phoneNumberType <- function() {
  phone_util <- .get_phoneNumberUtil()
  .jfields(phone_util$PhoneNumberType) %>% gsub("^.*\\.", "", .)
}

.get_validationResult <- function() {
  phone_util <- .get_phoneNumberUtil()
  .jfields(phone_util$ValidationResult) %>% gsub("^.*\\.", "", .)
}

# PhoneNumberUtil "get" functions
.getRegionCodeForCountryCode <- function(x) {
  x <- as.integer(x)
  phone_util <- .get_phoneNumberUtil()
  phone_util$getRegionCodeForCountryCode(x)
}

.getRegionCodesForCountryCode <- function(x) {
  x <- as.integer(x)
  phone_util <- .get_phoneNumberUtil()
  .jset_to_str(phone_util$getRegionCodesForCountryCode(x))
}

.getSupportedCallingCodes <- function() {
  phone_util <- .get_phoneNumberUtil()
  .jset_to_str(phone_util$getSupportedCallingCodes())
}
  
.getSupportedRegions <- function() {
  phone_util <- .get_phoneNumberUtil()
  .jset_to_str(phone_util$getSupportedRegions())
}
  
.getSupportedGlobalNetworkCallingCodes <- function() {
  phone_util <- .get_phoneNumberUtil()
  .jset_to_str(phone_util$getSupportedGlobalNetworkCallingCodes())
}

.getSupportedTypesForRegion <- function(x) {
  x <- as.character(x)
  phone_util <- .get_phoneNumberUtil()
  .jset_to_str(phone_util$getSupportedTypesForRegion(x))
}

.getSupportedTypesForNonGeoEntity <- function(x) {
  x <- as.integer(x)
  phone_util <- .get_phoneNumberUtil()
  .jset_to_str(phone_util$getSupportedTypesForNonGeoEntity(x))
}
