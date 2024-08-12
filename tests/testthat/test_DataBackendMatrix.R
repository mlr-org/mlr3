requireNamespace("Matrix")
data = Matrix::Matrix(0, nrow = 10, ncol = 12, sparse = TRUE)
colnames(data) = sprintf("cn%04i", seq_len(ncol(data)))

expect_Matrix = function(x, ...) {
  expect_class(x, "Matrix")
  xm = as.matrix(x)
  expect_matrix(xm, ...)
}

test_that("DataBackendMatrix construction", {
  b = as_data_backend(data)
  expect_backend(b)
  expect_equal(b$rownames, 1:10)

  b = as_data_backend(data, primary_key = 11:20)
  expect_equal(b$rownames, 11:20)

  b = as_data_backend(data, primary_key = "rid", dense = data.table(rid = 11:20))
  expect_equal(b$rownames, 11:20)
})

test_that("DataBackendMatrix sparse output", {
  b = as_data_backend(data)

  expect_data_table(b$head())

  rn = b$rownames
  cn = b$colnames

  # extra cols are ignored
  x = b$data(rows = rn[1L], cols = c(cn[2L], "_not_existing_"))
  expect_data_table(x, nrows = 1L, ncols = 1L)

  # zero cols matching
  x = b$data(rows = rn[1L], cols = "_not_existing_")
  expect_data_table(x, ncols = 0L)

  # extra rows are ignored
  query_rows = c(rn[4L], if (is.integer(rn)) -1L else "_not_existing_")
  x = b$data(query_rows, cols = b$primary_key)
  expect_equal(unname(x[[b$primary_key]]), rn[4])

  # zero rows matching
  query_rows = if (is.integer(rn)) -1L else "_not_existing_"
  x = b$data(rows = query_rows, cols = cn[2L])
  expect_data_table(x, nrows = 0L, ncols = 1L)

  # rows are duplicated
  x = b$data(rows = rep(rn[1L], 2L), cols = b$colnames)
  expect_data_table(x, nrows = 2L, ncols = b$ncol)

  # rows are returned in the right order
  i = sample(rn, min(b$nrow, 10L))
  x = b$data(rows = i, cols = b$primary_key)
  testthat::expect_equal(i, x[[1]])

  # duplicated cols raise exception
  testthat::expect_error(b$data(rows = rn[1L], cols = rep(cn[1L], 2L)), "unique")

  # argument n of head
  expect_data_table(b$head(3), nrows = 3, ncols = b$ncol)
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
  expect_data_table(task$data())
})

test_that("learners can request sparse data format", {
  LearnerSparseTest = R6Class("LearnerRegrRpart", inherit = LearnerRegr,
    public = list(
      initialize = function(id = "regr.sparsetest") {
        super$initialize(
          id = id,
          feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
          predict_types = "response",
          properties = c("weights", "missings", "importance", "selected_features")
        )
      }
    ),

    private = list(
      .train = function(task) {
        task$data()
      },

      .predict = function(task) {
        list(response = rep(task$class_names[1L], task$nrow))
      }
    )
  )

  td = cbind(y = 1:10, data)
  b = as_data_backend(td)
  task = TaskRegr$new("regr_task", b, target = "y")
  lrn = LearnerSparseTest$new()
  expect_learner(lrn)

  lrn$train(task)
  expect_class(lrn$model, "data.table")
})
