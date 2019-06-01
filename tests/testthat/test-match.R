context("phone equality checks")

test_that("matches correctly", {
  expect_equal(is_match(phone("0412345678", "AU"),
                        phone("+61 412 345 678", "AU")),
               TRUE)
  expect_equal(is_match(phone("0412345678", "AU"),
                        phone("0412345678", "US")),
               FALSE)
  expect_equal(is_match(phone("+61 412 345 678", "AU"),
                        phone("+61 412 345 678", "US")),
               TRUE)
})

test_that("correct return type for all inputs", {
  expect_identical(is_match(NA, NA, detailed = FALSE), NA)
  expect_identical(is_match(NA, NA, detailed = TRUE), NA_character_)
})

test_that("successfully runs for all combinations of input type", {
  expect_identical(is_match(phone("0412345678", "AU"), phone("0412345678", "AU")), TRUE)
  expect_identical(is_match("+61412345678", phone("0412345678", "AU")), TRUE)
  expect_identical(is_match(phone("0412345678", "AU"), "+61412345678"), TRUE)
  expect_identical(is_match("+61412345678", "+61412345678"), TRUE)
  expect_identical(is_match("0412345678", "0412345678", strict = TRUE), FALSE)
  expect_identical(is_match("0412345678", "0412345678", strict = FALSE), TRUE)
})

test_that("binary operators use is_match", {
  ph_match <- phone(rep(c("0412345678", NA), 10), "AU")
  expect_identical(ph_match == ph_match, is_match(ph_match, ph_match))
  expect_identical(ph_match != ph_match, !is_match(ph_match, ph_match))
  expect_error(ph_match > ph_match)
})

test_that("errors", {
  expect_error(is_match(list(), ""))
  expect_error(is_match("", list()))
})
