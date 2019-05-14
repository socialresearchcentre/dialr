context("geocoder")

test_that("get_geocode", {
  expect_error(get_geocode(FALSE))
  expect_is(get_geocode(phone(ph_raw, "AU")), "character")
  
  expect_equal(get_geocode(phone(ph_raw, "AU")),
               c(NA, "", "", "", "Melbourne", "New Jersey"))
  expect_equal(get_geocode(phone(ph_raw, "AU"), home = "AU"),
               c(NA, "", "", "", "Melbourne", "United States"))
  expect_equal(get_geocode(phone(ph_raw, "AU"), home = "US"),
               c(NA, "", "Australia", "Australia", "Australia", "New Jersey"))
  expect_equal(get_geocode(phone(ph_raw, "AU"), locale = "de", home = "DE"),
               c(NA, "", "Australien", "Australien", "Australien", "Vereinigte Staaten von Amerika"))
  # TODO - find a better example for `check`
  expect_equal(get_geocode(phone(ph_raw, "AU"), check = TRUE),
               c(NA, "", "", "", "Melbourne", "New Jersey"))
})
