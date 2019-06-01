context("example phone numbers")

test_that("all regions return valid example numbers", {
  expect_true(all(is_possible(get_example(regions))))
  expect_true(all(is_valid(get_example(regions))))
  # expect_true(all(is_possible(get_example(regions, valid = FALSE))))
  expect_false(any(is_valid(get_example(regions, valid = FALSE))))
})

test_that("all regions return valid example numbers for valid types", {
  region_type_valid <-
    lapply(regions, function(x) {
      data.frame(region = x,
                 type = get_types_for_region(x)[[1]],
                 stringsAsFactors = FALSE)
    })
  region_type_valid <- Reduce(rbind, region_type_valid)
  region_type_valid <- get_example(region_type_valid$region,
                                   region_type_valid$type)
  
  expect_true(all(is_possible(region_type_valid)))
  expect_true(all(is_valid(region_type_valid)))
})

test_that("invalid types for region return NA", {
  types <- get_supported_types()
  region_type_invalid <-
    lapply(regions, function(x) {
      data.frame(region = x,
                 type = types[!types %in% get_types_for_region(x)[[1]]],
                 stringsAsFactors = FALSE)
    })
  region_type_invalid <- Reduce(rbind, region_type_invalid)
  # remove FIXED_LINE_OR_MOBILE, covered by FIXED_LINE and MOBILE
  region_type_invalid <-
    region_type_invalid[region_type_invalid$type != "FIXED_LINE_OR_MOBILE", ]
  region_type_invalid <- get_example(region_type_invalid$region,
                                     region_type_invalid$type)
  
  expect_false(any(is_valid(region_type_invalid)))
  expect_false(any(is_possible(region_type_invalid)))
  expect_true(all(is.na(as.character(region_type_invalid))))
})
