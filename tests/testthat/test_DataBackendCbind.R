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
  expect_data_table(b$data(b$rownames, b$colnames), nrow = 150, ncol = 6)
  expect_set_equal(b$distinct("Species")$Species, distinct(iris$Species))

  x = b$missing(b$rownames, c("Petal.Width", "Petal.Length"))
  expect_equal(x, set_names(c(0L, 30L), c("Petal.Width", "Petal.Length")))
})
