context("tibble checks")

library(dplyr)

ph <- phone(ph_raw, "AU")

test_that("phone vectors work in tibbles", {
  expect_s3_class(tibble(ph), "tbl_df")
})

test_that("subsetting works in tibbles", {
  ph_tbl <- tibble(ph)
  
  expect_equal(pull(ph_tbl, ph), ph)
  expect_equal(pull(filter(ph_tbl, row_number() %in% 1:2), ph), ph[1:2])
})

test_that("tibbles print successfully", {
  ph_tbl <- tibble(ph)
  
  expect_known_output(print(ph_tbl), "phone_print_tibble.txt")
})
