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

test_that("character and factor to Date and POSIXct", {
  x = c("2020-01-01", NA, "2020-01-02")
  expect_identical(auto_convert(x, "x", "Date", character()), as.Date(x))
  expect_identical(auto_convert(factor(x), "x", "Date", character()), as.Date(x))
  expect_identical(auto_convert(x, "x", "POSIXct", character()), as.POSIXct(x, ""))

  # values which cannot be parsed must not be converted to NA
  x = c("2020-01-01", "foo", "01.02.2020")
  expect_error(auto_convert(x, "x", "Date", character()), "[Ii]ncompatible type")
  expect_error(auto_convert(factor(x), "x", "Date", character()), "[Ii]ncompatible type")
  expect_error(auto_convert(x, "x", "POSIXct", character()), "[Ii]ncompatible type")
  expect_error(auto_convert(c("2020-01-01 10:00:00", "foo"), "x", "POSIXct", character()), "[Ii]ncompatible type")

  task = as_task_regr(data.table(y = 1:2, d = as.Date(c("2020-01-01", "2020-01-02"))), target = "y")
  expect_error(task$rbind(data.table(y = 3:4, d = c("2020-01-03", "3 Jan 2020"))), "[Ii]ncompatible type")
  expect_equal(task$nrow, 2L)
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
  # these tests are disabled, R-devel is changing stuff too frequently
  # expect_identical(
  #   auto_convert(NA, "x", "POSIXct", character()),
  #   .POSIXct(NA, "")
  # )

  # expect_identical(
  #   auto_convert(NA_integer_, "x", "POSIXct", character()),
  #   .POSIXct(NA_integer_, "")
  # )

  # expect_identical(
  #   auto_convert(NA_real_, "x", "POSIXct", character()),
  #   .POSIXct(NA_real_, "")
  # )

  # expect_identical(
  #   auto_convert(NA_character_, "x", "POSIXct", character()),
  #   as.POSIXct(NA_character_)
  # )

  # expect_identical(
  #   auto_convert(factor(NA_character_, levels = "a"), "x", "POSIXct", "a"),
  #   as.POSIXct(NA_character_)
  # )

  # expect_identical(
  #   auto_convert(ordered(NA_character_, levels = "a"), "x", "POSIXct", "a"),
  #   as.POSIXct(NA_character_)
  # )

  expect_identical(
    auto_convert("2020-01-20 10:00:00", "x", "POSIXct", character()),
    as.POSIXct("2020-01-20 10:00:00", "")
  )

  expect_date(auto_convert(Sys.time(), "x", "Date", character()))
})

test_that("Date", {
  expect_date(auto_convert("2021-04-21", "x", "Date", character()))
})
