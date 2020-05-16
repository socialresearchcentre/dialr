context("geocoder")

test_that("get_geocode", {
  expect_error(get_geocode(FALSE))
  expect_is(get_geocode(phone(ph_raw, "AU")), "character")
  
  expect_equal(get_geocode(phone(ph_raw, "AU")) %in% c(NA, ""),
               c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE))
  expect_equal(get_geocode(phone(ph_raw, "AU"), home = "AU") %in% c(NA, ""),
               c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE))
  expect_equal(get_geocode(phone(ph_raw, "AU"), home = "US") %in% c(NA, ""),
               c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(get_geocode(get_example("US"), locale = "in", home = "IN"),
               "Amerika Serikat")
  
  expect_true(is.na(get_geocode(phone("0", "AU"))))
  expect_true(is.na(get_geocode(phone("0", "AU"), strict = TRUE)))
  expect_false(is.na(get_geocode(phone("1234", "AU"))))
  expect_true(is.na(get_geocode(phone("1234", "AU"), strict = TRUE)))
})
