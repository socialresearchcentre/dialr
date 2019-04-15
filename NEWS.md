
# dialr (development version)

## Performance improvements

Code calling libphonenumber methods via rJava has been tweaked for performance,
including using more efficient rJava acess methods and caching commonly used
static values. Most operations are now approx. 10 times faster.

## Minor improvements

* New introductory `vignette("dialr")`.

* Improved function documentation.

* More region and type functions implemented. See `` ?`dialr-region` `` or
`` ?`dialr-type` `` for details.

* New `get_example` function generates example phone numbers for provided
regions and types.

# dialr 0.2.0

## Introduction of `dialrjars`

To separate `libphonenumber` updates from structural package updates, the
`libphonenumber` jars have been split into the new package `dialrjars`.
This is a breaking change, and `dialrjars` will need to be installed for
`dialr` to continue to work.

## Addition of the `phone` class

In addition to the one-shot phone processing functions, dialr now has a
`phone` S3 class to reduce the processing load from re-parsing phone numbers for
every operation. This is a fundamental package change with a full set of
replacement functions, for details see `?phone`.

# dialr 0.1.0

Initial release.
