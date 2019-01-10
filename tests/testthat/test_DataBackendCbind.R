context("DataBackendCbind")

test_that("DataBackendCbind", {
  data = as.data.table(iris)
  data$Petal.Length[91:120] = NA
  data$id = 1:150

  b1 = as_data_backend(data[, -"Sepal.Length"], primary_key = "id")
  b2 = as_data_backend(data[, c("id", "Sepal.Length")], primary_key = "id")
  b = DataBackendCbind$new(b1, b2, b1$colnames, b2$colnames)
  # self = b
  expect_backend(b)
  expect_iris_backend(b, n_missing = 30L)

  expect_set_equal(b$rownames, 1:150)
  expect_set_equal(b$colnames, names(data))
  expect_data_table(b$data(b$rownames, b$colnames), nrow = 150, ncol = 6)
  expect_set_equal(b$distinct("Species")$Species, distinct(iris$Species))

  x = b$missing(b$rownames, c("Petal.Width", "Petal.Length"))
  expect_equal(x, set_names(c(0L, 30L), c("Petal.Width", "Petal.Length")))
})

test_that("issue #124", {
  task = mlr_tasks$get("iris")
  newcols = cbind(data.table(col = 1:150), task$row_ids)
  task$select(character(0))$cbind(newcols)
  expect_data_table(task$data(cols = "col"), ncol = 1L, nrow = 150L)
})

test_that("cbind backends with same columns", {
  data = as.data.table(iris)
  data$id = 1:150

  format = "data.table"
  b1 = as_data_backend(data[, -"Sepal.Length"], primary_key = "id")
  b2 = as_data_backend(data[, c("id", "Sepal.Length", "Sepal.Width", "Petal.Width")], primary_key = "id")
  cols_b1 = c("Sepal.Width")
  cols_b2 = c("Sepal.Length", "Petal.Width")

  rows = 1:10
  cols = b2$colnames

  b = self = DataBackendCbind$new(b1, b2, cols_b1, cols_b2)
  expect_backend(b)
})

test_that("Backends with different rows", {
  data = as.data.table(iris)
  data$id = 1:150

  b1 = as_data_backend(data[1:20, -"Sepal.Length"], primary_key = "id")
  b2 = as_data_backend(data[1:10, c("id", "Sepal.Length")], primary_key = "id")

  b = DataBackendCbind$new(b1, b2, b1$colnames, b2$colnames)

  expect_set_equal(b$colnames, c(names(iris), "id"))
  expect_set_equal(b$rownames, 1:10)

  expect_data_table(b$head(12), nrow = 10, ncol = 6)
})
