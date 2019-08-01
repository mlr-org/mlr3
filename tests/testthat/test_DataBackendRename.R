context("DataBackendRename")

test_that("DataBackendRename", {
  names_orig = names(iris)
  names_new = paste0("xx_", names_orig)
  b = DataBackendRename$new(as_data_backend(iris), set_names(names_new, names_orig))
  expect_backend(b)

  expect_set_equal(b$colnames, c(names_new, "..row_id"))
  expect_set_equal(names(b$data(b$rownames, b$colnames)), c(names_new, "..row_id"))
  expect_set_equal(names(b$head()), c(names_new, "..row_id"))

  expect_data_table(b$data(rows = b$rownames, cols = names_orig), ncols = 0)
  expect_data_table(b$data(rows = b$rownames, cols = names_new), ncols = 5)
})

test_that("nested backends", {
  data = as.data.table(iris)
  b1 = as_data_backend(data)
  b2 = DataBackendRename$new(as_data_backend(data), set_names(paste0("new_", names(iris)), names(iris)))
  b = DataBackendCbind$new(b1, b2, b1$colnames, b2$colnames)
  expect_backend(b)

  expect_set_equal(b$rownames, 1:150)
  expect_set_equal(b$colnames, c(b1$colnames, b2$colnames))
})
