context("DataBackendCbind")

test_that("DataBackendCbind", {
  data = as.data.table(iris)
  data$Petal.Length[91:120] = NA
  data$id = 1:150

  b1 = as_data_backend(data[, -"Sepal.Length"], primary_key = "id")
  b2 = as_data_backend(data[, c("id", "Sepal.Length")], primary_key = "id")
  b = DataBackendCbind$new(b1, b2)
  expect_backend(b)
  expect_iris_backend(b, n_missing = 30L)

  expect_set_equal(b$rownames, 1:150)
  expect_set_equal(b$colnames, names(data))
  expect_data_table(b$data(b$rownames, b$colnames), nrows = 150, ncols = 6)
  expect_set_equal(b$distinct(b$rownames, "Species")$Species, distinct_values(iris$Species, drop = FALSE))

  x = b$missings(b$rownames, c("Petal.Width", "Petal.Length"))
  expect_equal(x, set_names(c(0L, 30L), c("Petal.Width", "Petal.Length")))
})

test_that("issue #124", {
  task = mlr_tasks$get("iris")
  newcols = cbind(data.table(col = 1:150))
  task$select(character(0))$cbind(newcols)
  expect_data_table(task$data(cols = "col"), ncols = 1L, nrows = 150L)
})

test_that("cbind backends with same columns", {
  data = as.data.table(iris)
  data$id = 1:150
  data1 = copy(data)
  data2 = copy(data)

  data1$Petal.Width = NA
  b1 = as_data_backend(data1[, -"Sepal.Length"], primary_key = "id")

  data2$Sepal.Width = NA
  b2 = as_data_backend(data2[, c("id", "Sepal.Length", "Sepal.Width", "Petal.Width")], primary_key = "id")

  rows = 1:10
  cols = b2$colnames

  b = DataBackendCbind$new(b1, b2)
  expect_backend(b)
  data = b$head(Inf)
  expect_false(anyMissing(data$Petal.Width))
  expect_true(allMissing(data$Sepal.Width))
})

test_that("Backends with different rows", {
  data = as.data.table(iris)
  data$id = 1:150

  b1 = as_data_backend(data[1:20, -"Sepal.Length"], primary_key = "id")
  b2 = as_data_backend(data[1:10, c("id", "Sepal.Length")], primary_key = "id")

  b = DataBackendCbind$new(b1, b2)

  expect_set_equal(b$colnames, c(names(iris), "id"))
  expect_set_equal(b$rownames, 1:20)

  expect_data_table(b$head(Inf), nrows = 20, ncols = 6)
})

test_that("Backends with mixed data_formats", {
  requireNamespace("Matrix")
  i = c(1, 3:8, 20)
  j = c(2, 9, 6:10, 5)
  x = 7 * (1:8)
  A = Matrix::sparseMatrix(i, j, x = x)
  colnames(A) = letters[1:10]
  X = cbind(A, Y = rnorm(nrow(A)))
  task = TaskRegr$new("sparse", X, "Y")

  rr = resample(task, "regr.rpart", "holdout")
  expect_number(rr$aggregate())

  task$cbind(data.frame(z = runif(task$nrow)))
  expect_backend(task$backend)
  expect_set_equal(task$backend$data_formats, "data.table")

  rr = resample(task, "regr.rpart", "holdout")
  expect_number(rr$aggregate())
})
