context("DataBackendDataTable")

test_that("DataBackendDataTable construction", {
  b = as_data_backend(iris)

  expect_backend(b)
  expect_iris_backend(b)

  data = iris
  data$id = sprintf("r%02i", 1:150)
  b = as_data_backend(data, primary_key = "id")
  expect_backend(b)
  expect_character(b$rownames, len = 150, any.missing = FALSE, unique = TRUE, pattern = "^r[0-9]+$")

  rownames(data) = data$id
  data$id = NULL
  b = as_data_backend(data)
  expect_backend(b)
  expect_character(b$rownames, len = 150, any.missing = FALSE, unique = TRUE, pattern = "^r[0-9]+$")

  data$Petal.Length[21:50] = NA
  b = as_data_backend(data)
  x = b$missings(b$rownames, c("Petal.Width", "Petal.Length"))
  expect_equal(x, set_names(c(0L, 30L), c("Petal.Width", "Petal.Length")))
})


test_that("DataBackendDataTable with 0 rows", {
  b = as_data_backend(iris[integer(), ])
  expect_backend(b)
})
