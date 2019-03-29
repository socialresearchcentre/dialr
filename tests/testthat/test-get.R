context("get functions")

test_that("get_supported_regions", {
  expect_is(get_supported_regions(), "character")
  expect_match(get_supported_regions(), "^[A-Z]{2}$")
})

test_that("get_region", {
  expect_error(get_region(FALSE))
  expect_is(get_region(phone(ph_raw, "AU")), "character")
  
  expect_equal(get_region(phone(ph_raw, "AU")), ph_raw_regions)
  expect_equal(get_region(get_example(region = regions)), regions)
})

cc <- c(1, 61, 84)
  
test_that("get_region_for_calling_code", {
  expect_error(get_region_for_calling_code(FALSE))
  expect_is(get_region_for_calling_code(cc), "character")
  expect_match(get_region_for_calling_code(cc), "^[A-Z]{2}$")
})

test_that("get_regions_for_calling_code", {
  expect_error(get_regions_for_calling_code(FALSE))
  expect_is(get_regions_for_calling_code(cc), "list")
  expect_equal(length(get_regions_for_calling_code(cc)), length(cc))
  expect_true(all(sapply(get_regions_for_calling_code(cc),
                         function(x) { all(grepl("^[A-Z]{2}$", x)) })))
})

test_that("get_supported_types", {
  expect_is(get_supported_types(), "character")
  expect_match(get_supported_types(), "^[A-Z_]+$")
})

test_that("get_types_for_region", {
  expect_is(get_types_for_region("AU"), "list")
  expect_is(get_types_for_region("AU")[[1]], "character")
  
  expect_equal(get_types_for_region("AU")[[1]],
               c("FIXED_LINE", "MOBILE", "TOLL_FREE", "PREMIUM_RATE",
                 "SHARED_COST", "VOIP", "PAGER"))
})

test_that("get_type", {
  expect_error(get_type(FALSE))
  expect_is(get_type(phone(ph_raw, "AU")), "character")
  
  expect_equal(get_type(phone(ph_raw, "AU")), ph_raw_types)
  expect_equal(get_type(get_example(region = c("AU"), type = types$AU)),
               types$AU)
})


