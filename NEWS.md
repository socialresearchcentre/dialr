
# dialr 0.2.0

## Introduction of `dialrjars`

To separate `libphonenumber` updates from structural package updates, the
`libphonenumber` jars have been split into the new package `dialrjars`.
This is a breaking change, and `dialrjars` will need to be installed for
`dialr` to continue to work.

## Addition of the `phone` class

In addition to the one-shot phone processing functions, dialr now has a
`phone` S3 class to reduce the processing load from re-parsing phone numbers
for every operation. See `?phone` for details.

# dialr 0.1.0

Initial release.
