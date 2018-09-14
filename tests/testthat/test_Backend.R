context("Backend")

test_that("BackendDataTable construction", {
  data = iris
  b = BackendDataTable$new(data = data)
  expect_backend(b)
  expect_integer(b$rownames, len = 150, any.missing = FALSE, sorted = TRUE, lower = 1, upper = 150, unique = TRUE)

  data$id = sprintf("r%02i", 1:150)
  b = BackendDataTable$new(data = data, primary_key = "id")
  expect_backend(b)
  expect_character(b$rownames, len = 150, any.missing = FALSE, unique = TRUE, pattern = "^r[0-9]+$")

  rownames(data) = data$id
  data$id = NULL
  b = BackendDataTable$new(data = data)
  expect_backend(b)
  expect_character(b$rownames, len = 150, any.missing = FALSE, unique = TRUE, pattern = "^r[0-9]+$")
})

test_that("BackendRbind", {
  data = as.data.table(iris)
  data$id = 1:150

  backend = BackendDataTable$new(data[1:100, ], primary_key = "id")
  b = backend_rbind(backend,data[101:150, ])
  expect_backend(b)

  expect_set_equal(b$rownames, 1:150)
  expect_set_equal(b$colnames, names(data))
  expect_data_table(b$data(b$rownames, b$colnames), nrow = 150, ncol = 6)
})

test_that("BackendCbind", {
  data = as.data.table(iris)
  data$id = 1:150

  backend = BackendDataTable$new(data[, -"Sepal.Length"], primary_key = "id")
  b = backend_cbind(backend, data[, c("id", "Sepal.Length")])
  expect_backend(b)

  expect_set_equal(b$rownames, 1:150)
  expect_set_equal(b$colnames, names(data))
  expect_data_table(b$data(b$rownames, b$colnames), nrow = 150, ncol = 6)
})

test_that("Nested backends", {
  data = as.data.table(iris)
  data$id = 1:150

  backend = BackendDataTable$new(data[1:100, -"Sepal.Length"], primary_key = "id")
  b1 = backend_rbind(backend,data[101:130, -"Sepal.Length"])
  expect_backend(b1)

  b2 = backend_rbind(b1, data[131:150, -"Sepal.Length"])
  expect_backend(b2)

  b3 = backend_cbind(b2, data[, c("id", "Sepal.Length")])
  expect_backend(b3)

  expect_set_equal(b3$rownames, 1:150)
  expect_set_equal(b3$colnames, names(data))
  expect_data_table(b3$data(b3$rownames, b3$colnames), nrow = 150, ncol = 6)
})
