context("DataBackendRbind")

test_that("DataBackendRbind", {
  data = as.data.table(iris)
  data$Petal.Length[91:120] = NA
  data$id = 1:150

  b1 = as_data_backend(data[1:100, ], primary_key = "id")
  b2 = as_data_backend(data[101:150, ], primary_key = "id")
  b = DataBackendRbind$new(b1, b2, b1$rownames, b2$rownames)
  expect_backend(b)
  expect_iris_backend(b, n_missing = 30L)

  expect_set_equal(b$rownames, 1:150)
  expect_set_equal(b$colnames, names(data))
  expect_data_table(b$data(b$rownames, b$colnames), nrow = 150, ncol = 6)
  expect_set_equal(b$distinct("Species")$Species, distinct(iris$Species))

  x = b$missing(b$rownames, c("Petal.Width", "Petal.Length"))
  expect_equal(x, set_names(c(0L, 30L), c("Petal.Width", "Petal.Length")))
})

test_that("Backends with different cols", {
  data = as.data.table(iris)
  data$id = 1:150
  fn = c("Sepal.Width", "Species", "id")
  b1 = as_data_backend(data[1:100, c("Sepal.Length", "Sepal.Width", "Species", "id"), with = FALSE], primary_key = "id")
  b2 = as_data_backend(data[101:150, fn, with = FALSE], primary_key = "id")
  b = DataBackendRbind$new(b1, b2, b1$rownames, b2$rownames)

  expect_set_equal(b$colnames, fn)
  expect_equal(b$ncol, length(fn))

  h = b$head(60)
  expect_data_table(h, ncol = 3, nrow = 60)

  h = b$head(120)
  expect_data_table(h, ncol = 3, nrow = 120)

  expect_data_table(b$data(rows = 1:120, cols = b$colnames), nrow = 120, ncol = length(fn))
})
