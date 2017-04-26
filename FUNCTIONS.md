| R Function | Return Type | Method | Description |
|------------|-------------|--------|-------------|
|            | static java.lang.String | convertAlphaCharactersInNumber(java.lang.String number) | Converts all alpha characters in a number to their respective digits on a keypad, but retains existing formatting. |
|            | java.lang.Iterable<PhoneNumberMatch> | findNumbers(java.lang.CharSequence text, java.lang.String defaultRegion) | Returns an iterable over all PhoneNumberMatches in text. |
|            | java.lang.Iterable<PhoneNumberMatch> | findNumbers(java.lang.CharSequence text, java.lang.String defaultRegion, PhoneNumberUtil.Leniency leniency, long maxTries) | Returns an iterable over all PhoneNumberMatches in text. |
| ph_format  | java.lang.String | format(PhoneNumber number, PhoneNumberUtil.PhoneNumberFormat numberFormat) | Formats a phone number in the specified format using default rules. |
|            | void | format(PhoneNumber number, PhoneNumberUtil.PhoneNumberFormat numberFormat, java.lang.StringBuilder formattedNumber) |  |
|            | java.lang.String | formatByPattern(PhoneNumber number, PhoneNumberUtil.PhoneNumberFormat numberFormat, java.util.List<NumberFormat> userDefinedFormats) | Formats a phone number in the specified format using client-defined formatting rules. |
|            | java.lang.String | formatInOriginalFormat(PhoneNumber number, java.lang.String regionCallingFrom) | Formats a phone number using the original phone number format that the number is parsed from. |
|            | java.lang.String | formatNationalNumberWithCarrierCode(PhoneNumber number, java.lang.String carrierCode) | Formats a phone number in national format for dialing using the carrier as specified in the carrierCode. |
|            | java.lang.String | formatNationalNumberWithPreferredCarrierCode(PhoneNumber number, java.lang.String fallbackCarrierCode) | Formats a phone number in national format for dialing using the carrier as specified in the preferredDomesticCarrierCode field of the PhoneNumber object passed in. |
| ph_format  | java.lang.String | formatOutOfCountryCallingNumber(PhoneNumber number, java.lang.String regionCallingFrom) | Formats a phone number for out-of-country dialing purposes. |
|            | java.lang.String | formatOutOfCountryKeepingAlphaChars(PhoneNumber number, java.lang.String regionCallingFrom) | Formats a phone number for out-of-country dialing purposes. |
|            | AsYouTypeFormatter | getAsYouTypeFormatter(java.lang.String regionCode) | Gets an AsYouTypeFormatter for the specific region. |
|            | int | getCountryCodeForRegion(java.lang.String regionCode) | Returns the country calling code for a specific region. |
|            | PhoneNumber | getExampleNumber(java.lang.String regionCode) | Gets a valid number for the specified region. |
|            | PhoneNumber | getExampleNumberForType(java.lang.String regionCode, PhoneNumberUtil.PhoneNumberType type) | Gets a valid number for the specified region and number type. |
|            | static PhoneNumberUtil | getInstance() | Gets a PhoneNumberUtil instance to carry out international phone number formatting, parsing, or validation. |
|            | int | getLengthOfGeographicalAreaCode(PhoneNumber number) | Gets the length of the geographical area code in the nationalNumber_ field of the PhoneNumber object passed in, so that clients could use it to split a national significant number into geographical area code and subscriber number. |
|            | int | getLengthOfNationalDestinationCode(PhoneNumber number) | Gets the length of the national destination code (NDC) from the PhoneNumber object passed in, so that clients could use it to split a national significant number into NDC and subscriber number. |
|            | java.lang.String | getNationalSignificantNumber(PhoneNumber number) | Gets the national significant number of the a phone number. |
|            | java.lang.String | getNddPrefixForRegion(java.lang.String regionCode, boolean stripNonDigits) | Returns the national dialling prefix for a specific region. |
| ph_type    | PhoneNumberUtil.PhoneNumberType | getNumberType(PhoneNumber number) | Gets the type of a phone number. |
|            | java.lang.String | getRegionCodeForCountryCode(int countryCallingCode) | Returns the region code that matches the specific country calling code. |
| ph_region  | java.lang.String | getRegionCodeForNumber(PhoneNumber number) | Returns the region where a phone number is from. |
|            | java.util.Set<java.lang.String> | getSupportedRegions() | Convenience method to enable tests to get a list of what regions the library has metadata for. |
|            | boolean | isAlphaNumber(java.lang.String number) | Checks if the number is a valid vanity (alpha) number such as 800 MICROSOFT. |
|            | boolean | isNANPACountry(java.lang.String regionCode) | Checks if this is a region under the North American Numbering Plan Administration (NANPA). |
|            | PhoneNumberUtil.MatchType | isNumberMatch(PhoneNumber firstNumberIn, PhoneNumber secondNumberIn) | Takes two phone numbers and compares them for equality. |
|            | PhoneNumberUtil.MatchType | isNumberMatch(PhoneNumber firstNumber, java.lang.String secondNumber) | Takes two phone numbers and compares them for equality. |
| ph_possible| boolean | isPossibleNumber(PhoneNumber number) | Convenience wrapper around isPossibleNumberWithReason(PhoneNumber). |
|            | boolean | isPossibleNumber(java.lang.String number, java.lang.String regionDialingFrom) | Check whether a phone number is a possible number given a number in the form of a string, and the region where the number could be dialed from. |
| ph_possible| PhoneNumberUtil.ValidationResult | isPossibleNumberWithReason(PhoneNumber number) | Check whether a phone number is a possible number. |
| ph_valid   | boolean | isValidNumber(PhoneNumber number) | Tests whether a phone number matches a valid pattern. |
| ph_valid   | boolean | isValidNumberForRegion(PhoneNumber number, java.lang.String regionCode) | Tests whether a phone number is valid for a certain region. |
|            | static java.lang.String | normalizeDigitsOnly(java.lang.String number) | Normalizes a string of characters representing a phone number. |
| ph_parse   | PhoneNumber | parse(java.lang.String numberToParse, java.lang.String defaultRegion) | Parses a string and returns it in proto buffer format. |
|            | void | parse(java.lang.String numberToParse, java.lang.String defaultRegion, PhoneNumber phoneNumber) |  |
| ph_parse   | PhoneNumber | parseAndKeepRawInput(java.lang.String numberToParse, java.lang.String defaultRegion) | Parses a string and returns it in proto buffer format. |
|            | void | parseAndKeepRawInput(java.lang.String numberToParse, java.lang.String defaultRegion, PhoneNumber phoneNumber) |  |
|            | boolean | truncateTooLongNumber(PhoneNumber number) | Attempts to extract a valid number from a phone number that is too long to be valid, and resets the PhoneNumber object passed in to that valid version. |

