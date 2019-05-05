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

  expect_data_table(b$head())

  rn = b$rownames
  cn = b$colnames

  # extra cols are ignored
  x = b$data(rows = rn[1L], cols = c(cn[2L], "_not_existing_"), data_format = "Matrix")
  expect_Matrix(x, nrow = 1L, ncol = 1L)
  expect_equal(get_row_id(x), rn[1L])

  # zero cols matching
  x = b$data(rows = rn[1L], cols = "_not_existing_", data_format = "Matrix")
  expect_Matrix(x, nrow = 1L, ncol = 0L)
  expect_equal(get_row_id(x), rn[1L])

  # extra rows are ignored
  query_rows = c(rn[1L], if (is.integer(rn)) -1L else "_not_existing_")
  x = b$data(query_rows, cols = cn[2L], data_format = "Matrix")
  expect_Matrix(x, nrow = 1L, ncol = 1L)

  # zero rows matching
  query_rows = if (is.integer(rn)) -1L else "_not_existing_"
  x = b$data(rows = query_rows, cols = cn[2L], data_format = "Matrix")
  expect_Matrix(x, nrow = 0L, ncol = 1L)

  # rows are duplicated
  x = b$data(rows = rep(rn[1L], 2L), cols = b$colnames, data_format = "Matrix")
  expect_Matrix(x, nrow = 2L, ncol = b$ncol - 1L)

  # rows are returned in the right order
  i = sample(rn, min(b$nrow, 10L))
  x = b$data(rows = i, cols = b$primary_key, data_format = "Matrix")
  testthat::expect_equal(i, get_row_id(x))

  # duplicated cols raise exception
  testthat::expect_error(b$data(rows = rn[1L], cols = rep(cn[1L], 2L, data_format = "Matrix")), "unique")

  # argument n of head
  expect_data_table(b$head(3), nrow = 3, ncol = b$ncol)
})

test_that("$missings", {
  M = data
  M[2:3, "cn0005"] = NA
  b = as_data_backend(M)
  rows = b$rownames
  cols = b$colnames
  x = b$missings(b$rownames, b$colnames)
  expect_identical(sum(x), 2L)
  expect_identical(x[["cn0005"]], 2L)
})

test_that("task argument 'format' is passed down", {
  td = cbind(y = 1:10, data)
  b = as_data_backend(td)
  b$colnames
  task = TaskRegr$new("regr_task", b, target = "y")
  expect_data_table(task$data(data_format = "data.table"))
  expect_Matrix(task$data(data_format = "Matrix"))
})

test_that("learners can request sparse data format", {
  LearnerSparseTest = R6Class("LearnerRegrRpart", inherit = LearnerRegr,
    public = list(
      initialize = function(id = "regr.sparsetest") {
        super$initialize(
          id = id,
          feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
          predict_types = "response",
          properties = c("weights", "missings", "importance", "selected_features"),
          data_formats = c("Matrix", "data.table")
        )
      },

      train = function(task) {
        self$model = task$data(data_format = "Matrix")
        self
      },

      predict = function(task) {
        PredictionRegr$new(response = rep(task$class_names[1L], task$nrow))
      }
    )
  )

  td = cbind(y = 1:10, data)
  b = as_data_backend(td)
  task = TaskRegr$new("regr_task", b, target = "y")
  lrn = LearnerSparseTest$new()

  e = Experiment$new(task = task, learner = lrn)
  e$train()
  expect_is(e$model, "Matrix")
})
