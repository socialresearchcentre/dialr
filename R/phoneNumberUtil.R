
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

.get_phoneNumberType <- function() {
  .jfields(.get_phoneNumberUtil()$PhoneNumberType) %>% gsub("^.*\\.", "", .)
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
