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

  b1 = BackendDataTable$new(data[1:100, ], primary_key = "id")
  b2 = BackendDataTable$new(data[101:150, ], primary_key = "id")
  b = BackendRbind$new(b1, b2)
  expect_backend(b)

  expect_set_equal(b$rownames, 1:150)
  expect_set_equal(b$colnames, names(data))
  expect_data_table(b$data(b$rownames, b$colnames), nrow = 150, ncol = 6)
  expect_set_equal(b$distinct("Species")$Species, distinct(iris$Species))
})

test_that("BackendCbind", {
  data = as.data.table(iris)
  data$id = 1:150

  b1 = BackendDataTable$new(data[, -"Sepal.Length"], primary_key = "id")
  b2 = BackendDataTable$new(data[, c("id", "Sepal.Length")], primary_key = "id")
  b = BackendCbind$new(b1, b2)
  expect_backend(b)

  expect_set_equal(b$rownames, 1:150)
  expect_set_equal(b$colnames, names(data))
  expect_data_table(b$data(b$rownames, b$colnames), nrow = 150, ncol = 6)
  expect_set_equal(b$distinct("Species")$Species, distinct(iris$Species))
})

test_that("Nested backends", {
  data = as.data.table(iris)
  data$id = 1:150

  b1 = BackendDataTable$new(data[1:100, -"Sepal.Length"], primary_key = "id")
  b2 = BackendDataTable$new(data[101:130, -"Sepal.Length"], primary_key = "id")
  b3 = BackendRbind$new(b1, b2)
  expect_backend(b3)

  b4 = BackendDataTable$new(data[131:150, -"Sepal.Length"], primary_key = "id")
  b5 = BackendRbind$new(b3, b4)
  expect_backend(b5)

  b6 = BackendDataTable$new(data[, c("id", "Sepal.Length")], primary_key = "id")
  b7 = BackendCbind$new(b5, b6)
  expect_backend(b7)

  expect_set_equal(b7$rownames, 1:150)
  expect_set_equal(b7$colnames, names(data))
  expect_data_table(b7$data(b7$rownames, b7$colnames), nrow = 150, ncol = 6)
  expect_set_equal(b7$distinct("Species")$Species, distinct(iris$Species))
})
