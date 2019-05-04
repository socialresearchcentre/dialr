
# rJava helpers
.jset_to_str <- function(o) {
  vapply(.jcall(o, "[Ljava/lang/Object;", "toArray"),
         .jstrVal, "", USE.NAMES = FALSE)
}

.jstr_to_char <- function(o) {
  if (is.na(o))
    NULL
  else
    .jcast(.jnew("java/lang/String", o), "java/lang/CharSequence")
}

.jstr_to_locale <- function(o) {
  if (is.na(o))
    NULL
  else
    .jnew("java/util/Locale", o)
}

# Retrieve and cache singletons

.pnu_cache <- new.env(parent = emptyenv())

.get_phoneNumberUtil <- function() {
  if (is.null(.pnu_cache$phone_util)) {
    .pnu_cache$phone_util <-
      .jcall("com/google/i18n/phonenumbers/PhoneNumberUtil",
             "Lcom/google/i18n/phonenumbers/PhoneNumberUtil;",
             "getInstance")
  }
  .pnu_cache$phone_util
}

.get_phoneNumberToCarrierMapper <- function() {
  if (is.null(.pnu_cache$carrier_mapper)) {
    .pnu_cache$carrier_mapper <-
      .jcall("com/google/i18n/phonenumbers/PhoneNumberToCarrierMapper",
             "Lcom/google/i18n/phonenumbers/PhoneNumberToCarrierMapper;",
             "getInstance")
  }
  .pnu_cache$carrier_mapper
}

.get_phoneNumberOfflineGeocoder <- function() {
  if (is.null(.pnu_cache$offline_geocoder)) {
    .pnu_cache$offline_geocoder <-
      .jcall("com/google/i18n/phonenumbers/geocoding/PhoneNumberOfflineGeocoder",
             "Lcom/google/i18n/phonenumbers/geocoding/PhoneNumberOfflineGeocoder;",
             "getInstance")
  }
  .pnu_cache$offline_geocoder
}

.get_phoneNumberToTimeZonesMapper <- function() {
  if (is.null(.pnu_cache$timezone_mapper)) {
    .pnu_cache$timezone_mapper <-
      .jcall("com/google/i18n/phonenumbers/PhoneNumberToTimeZonesMapper",
             "Lcom/google/i18n/phonenumbers/PhoneNumberToTimeZonesMapper;",
             "getInstance")
  }
  .pnu_cache$timezone_mapper
}

# PhoneNumberUtil enums
.get_leniency <- function() {
  gsub("^.*\\.", "", .jfields(.get_phoneNumberUtil()$Leniency))
}

.get_matchType <- function() {
  gsub("^.*\\.", "", .jfields(.get_phoneNumberUtil()$MatchType))
}

.get_phoneNumberFormat <- function() {
  if (is.null(.pnu_cache$format)) {
    .pnu_cache$format <-
      gsub("^.*\\.", "", .jfields(.get_phoneNumberUtil()$PhoneNumberFormat))
  }
  .pnu_cache$format
}

validate_phone_format <- function(x) {
  formats <- .get_phoneNumberFormat()
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
  if (is.null(.pnu_cache$formats)) {
    .pnu_cache$formats <-
      .jcall("com/google/i18n/phonenumbers/PhoneNumberUtil$PhoneNumberFormat",
             "[Lcom/google/i18n/phonenumbers/PhoneNumberUtil$PhoneNumberFormat;",
             "values")
    
    names(.pnu_cache$formats) <-
      vapply(.pnu_cache$formats, .jstrVal, "", USE.NAMES = FALSE)
  }
  
  .pnu_cache$formats[[x]]
}

.get_phoneNumberType <- function() {
  if (is.null(.pnu_cache$type)) {
    .pnu_cache$type <-
      gsub("^.*\\.", "", .jfields(.get_phoneNumberUtil()$PhoneNumberType))
  }
  .pnu_cache$type
}

validate_phone_type <- function(x) {
  types <- .get_phoneNumberType()
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
  if (is.null(.pnu_cache$types)) {
    .pnu_cache$types <-
      .jcall("com/google/i18n/phonenumbers/PhoneNumberUtil$PhoneNumberType",
             "[Lcom/google/i18n/phonenumbers/PhoneNumberUtil$PhoneNumberType;",
             "values")
    
    names(.pnu_cache$types) <-
      vapply(.pnu_cache$types, .jstrVal, "", USE.NAMES = FALSE)
  }
  
  .pnu_cache$types[[x]]
}

.get_validationResult <- function() {
  gsub("^.*\\.", "", .jfields(.get_phoneNumberUtil()$ValidationResult))
}

# PhoneNumberUtil "get" functions
.getCountryCodeForRegion <- function(x) {
  x <- as.character(x)
  .get_phoneNumberUtil()$getCountryCodeForRegion(x)
}

.getRegionCodeForCountryCode <- function(x) {
  suppressWarnings(x <- as.integer(x))
  .get_phoneNumberUtil()$getRegionCodeForCountryCode(x)
}

.getRegionCodesForCountryCode <- function(x) {
  suppressWarnings(x <- as.integer(x))
  .jset_to_str(.get_phoneNumberUtil()$getRegionCodesForCountryCode(x))
}

.getSupportedCallingCodes <- function() {
  if (is.null(.pnu_cache$calling_code)) {
    .pnu_cache$calling_code <-
      .jset_to_str(.get_phoneNumberUtil()$getSupportedCallingCodes())
  }
  .pnu_cache$calling_code
}

validate_phone_calling_code <- function(x) {
  codes <- .getSupportedCallingCodes()
  if (!all(x %in% codes)) {
    stop(
      "Some `x` values are unsupported international calling codes: ",
      paste0(unique(x[!x %in% codes]), collapse = ", "),
      call. = FALSE
    )
  }
  
  x
}

.getSupportedRegions <- function() {
  if (is.null(.pnu_cache$region)) {
    .pnu_cache$region <-
      .jset_to_str(.get_phoneNumberUtil()$getSupportedRegions())
  }
  .pnu_cache$region
}

validate_phone_region <- function(x) {
  regions <- .getSupportedRegions()
  if (!all(x %in% regions)) {
    stop(
      "Some `x` values are not supported ISO country codes: ",
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
