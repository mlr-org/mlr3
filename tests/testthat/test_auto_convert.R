test_that("logical", {
  x = c(TRUE, NA, FALSE)

  expect_identical(
    auto_convert(x, "x", "logical", character()),
    c(TRUE, NA, FALSE)
  )

  expect_identical(
    auto_convert(x, "x", "integer", character()),
    c(1L, NA, 0L)
  )

  expect_identical(
    auto_convert(x, "x", "numeric", character()),
    c(1, NA, 0)
  )

  expect_identical(
    auto_convert(x, "x", "character", character()),
    c("TRUE", NA, "FALSE")
  )

  expect_identical(
    auto_convert(x, "x", "factor", c("TRUE", "FALSE")),
    factor(c("TRUE", NA, "FALSE"), levels = c("TRUE", "FALSE"))
  )

  expect_identical(
    auto_convert(x, "x", "ordered", c("TRUE", "FALSE")),
    ordered(c("TRUE", NA, "FALSE"), levels = c("TRUE", "FALSE"))
  )
})


test_that("integer", {
  x = c(1L, NA, 0L)

  expect_identical(
    auto_convert(x, "x", "logical", character()),
    c(TRUE, NA, FALSE)
  )

  expect_identical(
    auto_convert(x, "x", "integer", character()),
    c(1L, NA, 0L)
  )

  expect_identical(
    auto_convert(x, "x", "numeric", character()),
    c(1, NA, 0)
  )

  expect_identical(
    auto_convert(x, "x", "character", character()),
    c("1", NA, "0")
  )

  expect_identical(
    auto_convert(x, "x", "factor", c("1", "0")),
    factor(c("1", NA, "0"), levels = c("1", "0"))
  )

  expect_identical(
    auto_convert(x, "x", "ordered", c("1", "0")),
    ordered(c("1", NA, "0"), levels = c("1", "0"))
  )
})

test_that("numeric", {
  x = c(1, NA, 0)

  expect_identical(
    auto_convert(x, "x", "logical", character()),
    c(TRUE, NA, FALSE)
  )

  expect_identical(
    auto_convert(x, "x", "integer", character()),
    c(1L, NA, 0L)
  )

  expect_identical(
    auto_convert(x, "x", "numeric", character()),
    c(1, NA, 0)
  )

  expect_identical(
    auto_convert(x, "x", "character", character()),
    c("1", NA, "0")
  )

  expect_identical(
    auto_convert(x, "x", "factor", c("1", "0")),
    factor(c("1", NA, "0"), levels = c("1", "0"))
  )

  expect_identical(
    auto_convert(x, "x", "ordered", c("1", "0")),
    ordered(c("1", NA, "0"), levels = c("1", "0"))
  )
})

test_that("character", {
  x = c("1", NA, "0")

  expect_error(
    auto_convert(x, "x", "logical", character()),
    "[Ii]ncompatible type"
  )

  expect_error(
    auto_convert(x, "x", "integer", character()),
    "[Ii]ncompatible type"
  )

  expect_error(
    auto_convert(x, "x", "numeric", character()),
    "[Ii]ncompatible type"
  )

  expect_identical(
    auto_convert(as.character(x == "1"), "x", "logical", character()),
    c(TRUE, NA, FALSE)
  )

  expect_identical(
    auto_convert(x, "x", "character", character()),
    c("1", NA, "0")
  )

  expect_identical(
    auto_convert(x, "x", "factor", c("1", "0")),
    factor(c("1", NA, "0"), levels = c("1", "0"))
  )

  expect_identical(
    auto_convert(x, "x", "ordered", c("1", "0")),
    ordered(c("1", NA, "0"), levels = c("1", "0"))
  )
})

test_that("factor", {
  x = factor(c("1", NA, "0"), levels = c("1", "0"))

  expect_error(
    auto_convert(x, "x", "logical", character()),
    "[Ii]ncompatible type"
  )

  expect_error(
    auto_convert(x, "x", "integer", character()),
    "[Ii]ncompatible type"
  )

  expect_error(
    auto_convert(x, "x", "numeric", character()),
    "[Ii]ncompatible type"
  )
  expect_identical(
    auto_convert(x, "x", "character", character()),
    c("1", NA, "0")
  )

  expect_identical(
    auto_convert(x, "x", "factor", c("1", "0")),
    factor(c("1", NA, "0"), levels = c("1", "0"))
  )

  expect_identical(
    auto_convert(x, "x", "ordered", c("1", "0")),
    ordered(c("1", NA, "0"), levels = c("1", "0"))
  )
})

test_that("ordered", {
  x = ordered(c("1", NA, "0"), levels = c("1", "0"))

  expect_error(
    auto_convert(x, "x", "logical", character()),
    "[Ii]ncompatible type"
  )

  expect_error(
    auto_convert(x, "x", "integer", character()),
    "[Ii]ncompatible type"
  )

  expect_error(
    auto_convert(x, "x", "numeric", character()),
    "[Ii]ncompatible type"
  )

  expect_identical(
    auto_convert(x, "x", "character", character()),
    c("1", NA, "0")
  )

  expect_identical(
    auto_convert(x, "x", "factor", c("1", "0")),
    factor(c("1", NA, "0"), levels = c("1", "0"))
  )

  expect_identical(
    auto_convert(x, "x", "ordered", c("1", "0")),
    ordered(c("1", NA, "0"), levels = c("1", "0"))
  )
})

test_that("POSIXct", {
  expect_identical(
    auto_convert(NA, "x", "POSIXct", character()),
    .POSIXct(NA, "")
  )

  expect_identical(
    auto_convert(NA_integer_, "x", "POSIXct", character()),
    .POSIXct(NA, "")
  )

  expect_identical(
    auto_convert(NA_real_, "x", "POSIXct", character()),
    .POSIXct(NA_real_, "")
  )

  expect_identical(
    auto_convert(factor(NA_character_, levels = "a"), "x", "POSIXct", "a"),
    .POSIXct(NA, "")
  )

  expect_identical(
    auto_convert(ordered(NA_character_, levels = "a"), "x", "POSIXct", "a"),
    .POSIXct(NA, "")
  )

  expect_identical(
    auto_convert("2020-01-20 10:00:00", "x", "POSIXct", character()),
    as.POSIXct("2020-01-20 10:00:00", "")
  )
})
