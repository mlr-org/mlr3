context("DataBackendOverwrite")

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
  expect_equal(x, set_names(c(0L, 30L), c("Petal.Width", "Petal.Length")))
})
