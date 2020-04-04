# dialr 0.3.1

This release fixes a bug introduced by changes to the treatment of list based classes in tibble 3.0.0 that broke `phone` vectors in tibbles (#12). All `phone` vectors now also have the `list` class as recommended by the [tibble changelog](https://tibble.tidyverse.org/news/index.html).

In the future dialr may use the vctrs package to implement the `phone` class once vctrs is a bit more stable.

The libphonenumber repo moved from <https://github.com/googlei18n/libphonenumber/> to <https://github.com/google/libphonenumber/> a little while ago. Docs have been updated accordingly (#11).

# dialr 0.3.0

## New functions

* Retrieve carrier name with `get_carrier()` (#6).

* Add geocoding functions `get_geocode()` and `get_timezone()` (#5).

## Minor changes

* `get_example()` returns an empty phone number if the provided type is invalid for the provided region (#8).

* `phone()` constructor now supports parsing phone numbers in international format without a default region by specifying `NA` or `""` in the `region` argument (#4).

* `get_type()` gets a `strict` argument. If `TRUE`, invalid phone numbers return `NA` instead of `"UNKNOWN"`.

* Rename "One shot methods" vignette to remove redundant "dialr".

# dialr 0.2.1

## Performance improvements

Code calling libphonenumber methods via rJava has been tweaked for performance,
including using more efficient rJava acess methods and caching commonly used
static values. Most operations are now approx. 10 times faster.

## Major changes

* Updated R dependency to R >= 3.2.3. R 3.6.0 just came out and highr, a dependency of knitr, requires >= 3.2.3 so build fails on 3.1.x.

## Minor changes

* New introductory `vignette("dialr")`.

* New `is_match()` function supports proper `phone` equality checks. `==` and
`!=` now use `is_match()` for `phone` vectors. (#1)

* More region and type functions implemented. See `` ?`dialr-region` `` or
`` ?`dialr-type` `` for details.

* New `get_example()` function generates example phone numbers for provided
regions and types.

* Improved function documentation.

# dialr 0.2.0

## Introduction of `dialrjars`

To separate `libphonenumber` updates from structural package updates, the
`libphonenumber` jars have been split into the new package `dialrjars`.
This is a breaking change, and `dialrjars` will need to be installed for
`dialr` to continue to work.

## Addition of the phone class

In addition to the one-shot phone processing functions, dialr now has a
"phone" S3 class to reduce the processing load from re-parsing phone numbers for
every operation. This is a fundamental package change with a full set of
replacement functions, for details see `?phone`.

# dialr 0.1.0

Initial release.
