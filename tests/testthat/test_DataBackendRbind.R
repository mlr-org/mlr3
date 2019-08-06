context("DataBackendRbind")

test_that("DataBackendRbind", {
  data = as.data.table(iris)
  data$Petal.Length[91:120] = NA
  data$id = 1:150

  b1 = as_data_backend(data[1:100, ], primary_key = "id")
  b2 = as_data_backend(data[101:150, ], primary_key = "id")
  b = DataBackendRbind$new(b1, b2)
  expect_backend(b)
  expect_iris_backend(b, n_missing = 30L)

  expect_set_equal(b$rownames, 1:150)
  expect_set_equal(b$colnames, names(data))
  expect_data_table(b$data(b$rownames, b$colnames), nrows = 150, ncols = 6)
  expect_set_equal(b$distinct(b$rownames, "Species")$Species, distinct(iris$Species, drop = FALSE))

  x = b$missings(b$rownames, c("Petal.Width", "Petal.Length"))
  expect_equal(x, set_names(c(0L, 30L), c("Petal.Width", "Petal.Length")))
})

test_that("Backends with different cols", {
  data = as.data.table(iris)
  data$id = 1:150
  fn = c("Sepal.Width", "Species", "id")
  b1 = as_data_backend(data[1:100, c("Sepal.Length", "Sepal.Width", "Species", "id"), with = FALSE], primary_key = "id")
  b2 = as_data_backend(data[101:150, fn, with = FALSE], primary_key = "id")
  b = DataBackendRbind$new(b1, b2)

  expect_equal(b$ncol, 4)

  h = b$head(60)
  expect_data_table(h, ncols = 4, nrows = 60)

  h = b$head(120)
  expect_data_table(h, ncols = 4, nrows = 120)

  expect_data_table(b$data(rows = 1:120, cols = b$colnames), nrows = 120, ncols = 4)
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

  newdata = as.data.frame(as.list(set_names(rep(0, ncol(X)), colnames(X))))
  task$rbind(newdata)
  expect_backend(task$backend)
  expect_set_equal(task$backend$data_formats, "data.table")

  rr = resample(task, mlr_learners$get("regr.rpart"), mlr_resamplings$get("holdout"))
  expect_number(rr$aggregate())
})

test_that("Backends with same rows", {
  data = as.data.table(iris)
  data$id = 1:150
  data1 = data[1:20]
  data2 = data[6:10][, Petal.Length := NA]
  data2 = rbind(data2, data[101])

  b1 = as_data_backend(data1, primary_key = "id")
  b2 = as_data_backend(data2, primary_key = "id")

  b = DataBackendRbind$new(b1, b2)
  expect_backend(b)
  expect_set_equal(b$rownames, c(1:20, 101))

  data = b$data(b$rownames[1:20], b$colnames)
  expect_false(anyMissing(data$Petal.Length[1:5]))
  expect_false(anyMissing(data$Petal.Length[11:20]))
  expect_true(allMissing(data$Petal.Length[6:10]))
})
