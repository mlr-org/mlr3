context("DataBackendReplaceColumns")

test_that("DataBackenReplaceColumns", {
  data = as.data.table(iris)
  data$id = 1:150
  data[91:120, Petal.Length := NA]
  newdata = data[, list(id = id, Petal.Length = 1)]

  b1 = as_data_backend(data, primary_key = "id")
  b2 = as_data_backend(newdata, primary_key = "id")

  b = DataBackendReplaceColumns$new(b1, b2)
  expect_backend(b)
  expect_iris_backend(b, n_missing = 0L)
})
