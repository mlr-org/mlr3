context("DataBackendMatrix")

requireNamespace("Matrix")
data = Matrix::Matrix(0, nrow = 10, ncol = 12, sparse = TRUE)
colnames(data) = sprintf("cn%04i", seq_len(ncol(data)))

get_row_id = function(x) {
  attr(x, "..row_id")
}

expect_Matrix = function(x, ...) {
  expect_class(x, "Matrix")
  xm = as.matrix(x)
  expect_matrix(xm, ...)
}

test_that("DataBackendMatrix construction", {
  b = as_data_backend(data)
  expect_backend(b)
})

test_that("DataBackendMatrix sparse output", {
  b = as_data_backend(data)

  expect_class(b$head(format = "sparse"), class(data))

  rn = b$rownames
  cn = b$colnames

  # extra cols are ignored
  x = b$data(rows = rn[1L], cols = c(cn[2L], "_not_existing_"), format = "sparse")
  expect_Matrix(x, nrow = 1L, ncol = 1L)
  expect_equal(get_row_id(x), rn[1L])

  # zero cols matching
  x = b$data(rows = rn[1L], cols = "_not_existing_", format = "sparse")
  expect_Matrix(x, nrow = 1L, ncol = 0L)
  expect_equal(get_row_id(x), rn[1L])

  # extra rows are ignored
  query_rows = c(rn[1L], if (is.integer(rn)) -1L else "_not_existing_")
  x = b$data(query_rows, cols = cn[2L], format = "sparse")
  expect_Matrix(x, nrow = 1L, ncol = 1L)

  # zero rows matching
  query_rows = if (is.integer(rn)) -1L else "_not_existing_"
  x = b$data(rows = query_rows, cols = cn[2L], format = "sparse")
  expect_Matrix(x, nrow = 0L, ncol = 1L)

  # rows are duplicated
  x = b$data(rows = rep(rn[1L], 2L), cols = b$colnames, format = "sparse")
  expect_Matrix(x, nrow = 2L, ncol = b$ncol - 1L)

  # rows are returned in the right order
  i = sample(rn, min(b$nrow, 10L))
  x = b$data(rows = i, cols = b$primary_key, format = "sparse")
  testthat::expect_equal(i, get_row_id(x))

  # duplicated cols raise exception
  testthat::expect_error(b$data(rows = rn[1L], cols = rep(cn[1L], 2L, format = "sparse")), "duplicated")

  # argument n of head
  expect_Matrix(b$head(3, format = "sparse"), nrow = 3, ncol = b$ncol - 1L)
})

test_that("$missing", {
  M = data
  M[2:3, "cn0005"] = NA
  b = as_data_backend(M)
  rows = b$rownames
  cols = b$colnames
  x = b$missing(b$rownames, b$colnames)
  expect_identical(sum(x), 2L)
  expect_identical(x[["cn0005"]], 2L)
})

test_that("task argument 'format' is passed down", {
  td = cbind(y = 1:10, data)
  b = as_data_backend(td)
  b$colnames
  task = TaskRegr$new("regr_task", b, target = "y")
  expect_data_table(task$data(format = "data.table"))
  expect_Matrix(task$data(format = "sparse"))
})
