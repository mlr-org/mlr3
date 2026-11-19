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

  b = as_data_backend(iris, primary_key = 151:300)
  expect_equal(b$rownames, 151:300)
})

test_that("DataBackendDataTable with 0 rows", {
  b = as_data_backend(iris[integer(), ])
  expect_backend(b)
})

test_that("DataBackendDataTable validates the primary key", {
  expect_error(DataBackendDataTable$new(data.table(x = 1:2), "id"), "not in 'data'")
  expect_error(DataBackendDataTable$new(data.table(id = c(1L, 1L), x = 1:2), "id"), "duplicated")
  expect_error(DataBackendDataTable$new(data.table(id = c(1L, NA), x = 1:2), "id"), "missing")
  expect_error(DataBackendDataTable$new(data.table(id = c("a", "b"), x = 1:2), "id"), "integerish")

  b = DataBackendDataTable$new(data.table(id = c(2, 1), x = 1:2), "id")
  expect_backend(b)
  expect_equal(b$data(1:2, "x")$x, 2:1)
})
