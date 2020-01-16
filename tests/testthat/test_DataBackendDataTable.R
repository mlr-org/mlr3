context("DataBackendDataTable")

test_that("DataBackendDataTable construction", {
  b = as_data_backend(iris)

  expect_backend(b)
  expect_iris_backend(b)

  i = 1:30
  data = iris[i, ]
  data$id = i
  b = as_data_backend(data, primary_key = "id")
  expect_backend(b)
  expect_set_equal(b$rownames, i)

  rownames(data) = sprintf("rn_%i", data$id)
  data$id = NULL
  b = as_data_backend(data, keep_rownames = TRUE)
  expect_backend(b)
  expect_set_equal(b$data(i, "..rownames")[[1]], sprintf("rn_%i", i))

  data$Petal.Length[21:30] = NA
  b = as_data_backend(data)
  x = b$missings(b$rownames, c("Petal.Width", "Petal.Length"))
  expect_equal(x, set_names(c(0L, 10L), c("Petal.Width", "Petal.Length")))
})

test_that("DataBackendDataTable with 0 rows", {
  b = as_data_backend(iris[integer(), ])
  expect_backend(b)
})
