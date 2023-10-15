context("cc functions")

# remove non-ASCII
cc_lkp <- cc_lookup[!grepl("[^ -~]", names(cc_lookup))]

cc <- data.frame(country = names(cc_lkp),
                 region  = cc_lkp,
                 stringsAsFactors = FALSE)

test_that("get_cc", {
  expect_equal(get_cc(cc$country), cc$region)
  expect_equal(get_cc(tolower(cc$country)), cc$region)
  expect_equal(get_cc("BLAH"), NA_character_)
})

test_that("check_cc", {
  expect_equal(check_cc(cc$region), rep(TRUE, nrow(cc)))
  expect_equal(check_cc("BLAH"), FALSE)
})

test_that("validate_cc", {
  expect_equal(dialr:::validate_cc(cc$region), NULL)
  expect_error(dialr:::validate_cc("BLAH"))
})
