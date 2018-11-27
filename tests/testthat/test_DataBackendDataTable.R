context("DataBackend")

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
  x = b$missing(b$rownames, c("Petal.Width", "Petal.Length"))
  expect_equal(x, setNames(c(0L, 30L), c("Petal.Width", "Petal.Length")))
})

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
  expect_data_table(b$data(b$rownames, b$colnames), nrow = 150, ncol = 6)
  expect_set_equal(b$distinct("Species")$Species, distinct(iris$Species))

  x = b$missing(b$rownames, c("Petal.Width", "Petal.Length"))
  expect_equal(x, setNames(c(0L, 30L), c("Petal.Width", "Petal.Length")))
})

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
  expect_equal(x, setNames(c(0L, 30L), c("Petal.Width", "Petal.Length")))
})

test_that("DataBackendOverwrite", {
  data = as.data.table(iris)
  data$Petal.Length[91:120] = NA
  data$id = 1:150
  newdata = data[1:30, c("id", "Sepal.Length")][, Sepal.Length := 0]

  b1 = as_data_backend(data, primary_key = "id")
  b2 = as_data_backend(newdata, primary_key = "id")

  b = DataBackendOverwrite$new(b1, b2)
  expect_backend(b)
  expect_iris_backend(b, n_missing = 30L) # we do not test on Sepal.Length

  expect_equal(b$nrow, 150)
  expect_equal(b$ncol, 6)

  tab = b$data(c(1:2, 101:102), c("Sepal.Length", "Species"))
  expect_equal(tab$Sepal.Length[1:2], c(0, 0))
  expect_true(all(tab$Sepal.Length[3:4] > 0))

  tab = b$head()
  expect_equal(tab$Sepal.Length, rep(0, 6))

  expect_true(0 %in% b$distinct("Sepal.Length")[[1L]])

  x = b$missing(b$rownames, c("Petal.Width", "Petal.Length"))
  expect_equal(x, setNames(c(0L, 30L), c("Petal.Width", "Petal.Length")))
})

test_that("Nested backends", {
  data = as.data.table(iris)
  data$Petal.Length[91:120] = NA
  data$id = 1:150

  b1 = as_data_backend(data[1:100, -"Sepal.Length"], primary_key = "id")
  b2 = as_data_backend(data[101:130, -"Sepal.Length"], primary_key = "id")
  b3 = DataBackendRbind$new(b1, b2)
  expect_backend(b3)

  b4 = as_data_backend(data[131:150, -"Sepal.Length"], primary_key = "id")
  b5 = DataBackendRbind$new(b3, b4)
  expect_backend(b5)

  b6 = as_data_backend(data[, c("id", "Sepal.Length")], primary_key = "id")
  b7 = DataBackendCbind$new(b5, b6)
  expect_backend(b7)

  expect_iris_backend(b7, n_missing = 30L)

  x = b7$missing(b7$rownames, c("Petal.Width", "Petal.Length"))
  expect_equal(x, setNames(c(0L, 30L), c("Petal.Width", "Petal.Length")))
})
