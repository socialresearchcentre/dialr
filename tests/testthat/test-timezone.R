context("timezone")

test_that("get_timezone", {
  expect_error(get_timezone(FALSE))
  expect_is(get_timezone(phone(ph_raw, "AU")), "character")
  
  expect_true(is.na(get_timezone(phone("0", "AU"))))
  expect_true(is.na(get_timezone(phone("0", "AU"), strict = TRUE)))
  expect_false(is.na(get_timezone(phone("1234", "AU"))))
  expect_true(is.na(get_timezone(phone("1234", "AU"), strict = TRUE)))
})
