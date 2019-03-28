context("valid functions")

ph <- phone(ph_raw, "AU")

test_that("is_parsed", {
  expect_error(is_parsed(FALSE))
  expect_equal(is_parsed(ph), c(F, T, T, T, T, T))
})

test_that("is_valid", {
  expect_error(is_valid(FALSE))
  expect_equal(is_valid(get_example(region = regions)),
               rep(TRUE, times = length(regions)))
  
  expect_equal(is_valid(get_example(region = regions, valid = FALSE)),
               rep(FALSE, times = length(regions)))
})

test_that("is_possible", {
  expect_error(is_possible(FALSE))
  
  expect_equal(is_possible(ph), c(F, F, T, T, T, T))
  expect_equal(is_possible(ph, detailed = TRUE),
               c(NA, "TOO_SHORT", "IS_POSSIBLE", "IS_POSSIBLE",
                 "IS_POSSIBLE", "IS_POSSIBLE"))
  
  expect_equal(is_possible(ph, type = "MOBILE"), c(F, F, T, T, T, T))
  expect_equal(is_possible(ph, detailed = TRUE, type = "MOBILE"),
               c(NA, "TOO_SHORT", "IS_POSSIBLE", "IS_POSSIBLE",
                 "IS_POSSIBLE", "IS_POSSIBLE"))
  expect_equal(is_possible(ph, type = "TOLL_FREE"), c(F, F, F, F, F, T))
  expect_equal(is_possible(ph, detailed = TRUE, type = "TOLL_FREE"),
               c(NA, "TOO_SHORT", "INVALID_LENGTH", "INVALID_LENGTH",
                 "INVALID_LENGTH", "IS_POSSIBLE"))
})