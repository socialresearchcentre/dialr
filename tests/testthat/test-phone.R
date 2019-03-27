context("phone class")

x <- c(0, 0123, "0412 345 678", "61412987654", "03 9123 4567", "+12015550123")
ph <- phone(x, "AU")

test_that("phone class objects are created correctly", {
  expect_true(is.phone(ph))
  expect_equal(length(x), length(ph))
})

test_that("as.character", {
  expect_equal(x, as.character(ph))
  expect_equal(format(ph), as.character(ph, raw = FALSE))
})
