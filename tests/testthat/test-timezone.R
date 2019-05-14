context("timezone")

test_that("get_timezone", {
  expect_error(get_timezone(FALSE))
  expect_is(get_timezone(phone(ph_raw, "AU")), "character")
  
  expect_equal(get_timezone(phone(ph_raw, "AU")), ph_raw_timezone)
  expect_equal(get_timezone(phone(ph_raw, "AU"), strict = TRUE), ph_raw_timezone_strict)
})
