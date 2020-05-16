context("carrier functions")

test_that("get_carrier", {
  expect_error(get_carrier(FALSE))
  expect_is(get_carrier(phone(ph_raw, "AU")), "character")
  
  expect_equal(get_carrier(phone(ph_raw, "AU")),
               c(NA, "", "Optus", "Optus", "", ""))
  # TODO - find a better example for `safe`
  expect_equal(get_carrier(phone(ph_raw, "AU"), safe = TRUE),
               c(NA, NA, "", "", "", ""))
  
  expect_true(is.na(get_carrier(phone("0", "AU"))))
  expect_true(is.na(get_carrier(phone("0", "AU"), strict = TRUE)))
  expect_false(is.na(get_carrier(phone("1234", "AU"))))
  expect_true(is.na(get_carrier(phone("1234", "AU"), strict = TRUE)))
})
