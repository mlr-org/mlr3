context("DataBackend")

test_that("Nested backends", {
  data = as.data.table(iris)
  data$Petal.Length[91:120] = NA
  data$id = 1:150

  b1 = as_data_backend(data[1:100, -"Sepal.Length"], primary_key = "id")
  b2 = as_data_backend(data[101:130, -"Sepal.Length"], primary_key = "id")
  b3 = DataBackendRbind$new(b1, b2, b1$rownames, b2$rownames)
  expect_backend(b3)

  b4 = as_data_backend(data[131:150, -"Sepal.Length"], primary_key = "id")
  b5 = DataBackendRbind$new(b3, b4, b3$rownames, b4$rownames)
  expect_backend(b5)

  b6 = as_data_backend(data[, c("id", "Sepal.Length")], primary_key = "id")
  b7 = DataBackendCbind$new(b5, b6, b5$colnames, b6$colnames)
  expect_backend(b7)

  expect_iris_backend(b7, n_missing = 30L)

  x = b7$missing(b7$rownames, c("Petal.Width", "Petal.Length"))
  expect_equal(x, set_names(c(0L, 30L), c("Petal.Width", "Petal.Length")))

  task = mlr_tasks$get("iris")
  task$backend$data(rows = 1:150, cols = character(0L))
})
