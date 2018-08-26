context("Backend")

test_that("BackendDataTable construction", {
  data = iris
  b = BackendDataTable$new(data = data)
  expect_backend(b)
  expect_integer(b$rownames, len = 150, any.missing = FALSE, sorted = TRUE, lower = 1, upper = 150, unique = TRUE)

  data$id = sprintf("r%02i", 1:150)
  b = BackendDataTable$new(data = data, primary.key = "id")
  expect_backend(b)
  expect_character(b$rownames, len = 150, any.missing = FALSE, unique = TRUE, pattern = "^r[0-9]+$")

  rownames(data) = data$id
  data$id = NULL
  b = BackendDataTable$new(data = data)
  expect_backend(b)
  expect_character(b$rownames, len = 150, any.missing = FALSE, unique = TRUE, pattern = "^r[0-9]+$")
})
