context("phone class")

ph <- phone(ph_raw, "AU")

test_that("phone vector created successfully", {
  expect_error(phone(as.list(ph_raw), "AU"))
  expect_error(phone(character(0), "AU"))
  expect_error(phone(ph_raw, c("AU", "AU")))
  expect_error(phone(ph_raw, "ERROR"))
  expect_equal(phone(ph_raw, "AU"), ph)
  expect_equal(dialr:::validate_phone(ph), ph)
})

test_that("phone class objects are created correctly", {
  expect_true(is.phone(ph))
  expect_false(is.phone(ph_raw))
  expect_equal(length(ph_raw), length(ph))
})

test_that("as.character", {
  expect_equal(ph_raw, as.character(ph))
})

test_that("format", {
  ph_fmt <- phone("0412 345 678", "AU")
  
  expect_equal(format(ph), as.character(ph, raw = FALSE))
  
  expect_equal(format(ph_fmt, "E164", clean = TRUE), "+61412345678")
  expect_equal(format(ph_fmt, "NATIONAL", clean = TRUE), "0412345678")
  expect_equal(format(ph_fmt, "INTERNATIONAL", clean = TRUE), "+61412345678")
  expect_equal(format(ph_fmt, "RFC3966", clean = TRUE), "+61412345678")
  
  expect_equal(format(ph_fmt, "E164", clean = FALSE), "+61412345678")
  expect_equal(format(ph_fmt, "NATIONAL", clean = FALSE), "0412 345 678")
  expect_equal(format(ph_fmt, "INTERNATIONAL", clean = FALSE), "+61 412 345 678")
  expect_equal(format(ph_fmt, "RFC3966", clean = FALSE), "tel:+61-412-345-678")
  
  expect_equal(format(ph_fmt, home = "US"), "01161412345678")
  expect_equal(format(ph_fmt, home = "GB"), "0061412345678")
  expect_equal(format(ph_fmt, home = "US", clean = FALSE), "011 61 412 345 678")
  expect_equal(format(ph_fmt, home = "GB", clean = FALSE), "00 61 412 345 678")
  
  expect_equal(format(phone("123", "AU")), "+61123")
  expect_equal(format(phone("123", "AU"), strict = TRUE), NA_character_)
})

test_that("summary", {
  expect_is(summary(ph), "table")
})

test_that("phone subset", {
  expect_equal(ph[1], phone(ph_raw[1], "AU"))
  expect_equal(ph[1:3], phone(ph_raw[1:3], "AU"))
  
  expect_equal(ph[1], ph[[1]])
  expect_equal(ph[1:3], ph[[1:3]])
  
  expect_error(ph$a)
})

test_that("phone subset assignments", {
  expect_equal({ph[1] <- phone(ph_raw[1], "AU"); ph}, ph)
  expect_equal({ph[1:3] <- phone(ph_raw[1:3], "AU"); ph}, ph)
  
  expect_warning(ph[1] <- ph_raw[1])
  expect_equal({suppressWarnings(ph[1] <- ph_raw[1]); ph},
               {ph[1] <- phone(ph_raw[1], getOption("dialr.home")); ph})
  expect_equal({suppressWarnings(ph[1:3] <- ph_raw[1:3]); ph},
               {ph[1:3] <- phone(ph_raw[1:3], getOption("dialr.home")); ph})
  expect_error(ph[1] <- list(ph_raw[1]))
  
  expect_equal({ph[[1]] <- phone(ph_raw[1], "AU"); ph}, ph)
  expect_equal({ph[[1:3]] <- phone(ph_raw[1:3], "AU"); ph}, ph)
  
  expect_warning(ph[[1]] <- ph_raw[1])
  expect_equal({suppressWarnings(ph[[1]] <- ph_raw[1]); ph},
               {ph[[1]] <- phone(ph_raw[1], getOption("dialr.home")); ph})
  expect_equal({suppressWarnings(ph[[1:3]] <- ph_raw[1:3]); ph},
               {ph[[1:3]] <- phone(ph_raw[1:3], getOption("dialr.home")); ph})
  expect_error(ph[[1]] <- as.list(ph_raw[1]))
  
  expect_error(ph$a <- 1)
})

test_that("phone concatenation", {
  expect_equal(c(ph, ph), phone(c(ph_raw, ph_raw), "AU"))
  
  expect_warning(c(ph, ph_raw))
  expect_equal(suppressWarnings(c(ph, ph_raw)),
               c(ph, phone(ph_raw, getOption("dialr.home"))))
  
  expect_warning(c(ph, as.list(ph_raw)))
  expect_equal(suppressWarnings(c(ph, as.list(ph_raw))),
               ph)
  
  expect_equal(rep(ph, 5), c(ph, ph, ph, ph, ph))
})
