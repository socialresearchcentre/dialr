
# dialr

[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![CRAN status](https://www.r-pkg.org/badges/version/dialr)](https://cran.r-project.org/package=dialr)
[![Travis build status](https://travis-ci.org/socialresearchcentre/dialr.svg?branch=master)](https://travis-ci.org/socialresearchcentre/dialr)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/socialresearchcentre/dialr?branch=master&svg=true)](https://ci.appveyor.com/project/socialresearchcentre/dialr)

## Overview

dialr is an R port of [Google's libphonenumber library](https://github.com/googlei18n/libphonenumber).
It uses the java implementation of libphonenumber via rJava for all phone number processing.

For a full rundown of libphonenumber see their [github](https://github.com/googlei18n/libphonenumber)
and [javadocs](https://javadoc.io/doc/com.googlecode.libphonenumber/libphonenumber/).

## Installation

A compiled version of dialr can be installed from srclib, or a development version from github.
dialr also needs the dialrjars package to run. This is updated regularly with libphonenumber updates.

``` r
# Install the current compiled version from srclib:
devtools::install_url("socialresearchcentre/srclib/R/dialr.zip")
devtools::install_url("socialresearchcentre/srclib/R/dialrjars.zip")

# Or the the development version from GitHub:
devtools::install_github("socialresearchcentre/dialr")
devtools::install_github("socialresearchcentre/dialrjars")
```

## Usage

### Conceptual basis

dialr is an R interface to Google's libphonenumber java library.
libphonenumber defines the `PhoneNumberUtil` class, with a set of functions for
extracting information from and performing processing on a parsed `Phonenumber`
object. A phone number must be parsed before any other operations (e.g.
checking phone number validity, formatting) can be performed.

When parsing a phone number a "default region" is required to determine the
processing context for non-international numbers. A set of functions in the
`PhoneNumberUtil` class can perform various operations on the resulting
`Phonenumber` object.

dialr provides an interface to these functions to easily parse and process phone numbers in R.

### phone class

The phone class parses phone numbers and stores the java `Phonenumber` object
alongside the original raw text. This removes unnecessary re-parsing time when
performing multiple operations on a vector of phone numbers, and is closer to
the spirit of the libphonenumber package.

``` r
library(dialr)
library(dplyr)

# Parse phone number vector
x <- c(0, 0123, "0404 753 123", "61410123817")
x <- phone(x, "AU")

is_parsed(x)    # Was the phone number successfully parsed?
is_valid(x)     # Is the phone number valid?
is_possible(x)  # Is the phone number possible?
get_region(x)   # What region (ISO country code) is the phone number from?
get_type(x)     # Is the phone number a fixed line, mobile etc.
format(x)
format(x, home = "AU")

# Use with dplyr
y <- tibble(id = 1:4,
            phone1 = c(0, 0123, "0404 753 123", "61410123817"),
            phone2 = c("03 9388 1234", 1234, "+12015550123", 0),
            country = c("AU", "AU", "AU", "AU"))

y %>%
  mutate_at(vars(matches("^phone")), ~phone(., country)) %>%
  mutate_at(vars(matches("^phone")),
            list(valid = is_valid,
                 region = get_region,
                 type = get_type,
                 clean = format))

```

### One-shot methods

The one shot methods parse the provided phone number during the function call.
A country code must be provided in all cases to parse the provided numbers.

The one-shot functions are likely to be deprecated in future.

``` r
library(dialr)

x <- c(0, 0123, "0404 753 123", "61410123817")

ph_valid(x, "AU")    # Is the phone number valid?
ph_possible(x, "AU") # Is the phone number possible?
ph_region(x, "AU")   # What region (ISO country code) is the phone number from?
ph_type(x, "AU")     # Is the phone number a fixed line, mobile etc.
ph_format(x, "AU")
ph_format(x, "AU", home = "AU")
```
