test_that("DataBackendRename", {
  old = names(iris)
  new = paste0("xx_", old)
  b = DataBackendRename$new(as_data_backend(iris), old, new)
  expect_backend(b)

  expect_set_equal(b$colnames, c(new, "..row_id"))
  expect_set_equal(names(b$data(b$rownames, b$colnames)), c(new, "..row_id"))
  expect_set_equal(names(b$head()), c(new, "..row_id"))

  expect_data_table(b$data(rows = b$rownames, cols = old), ncols = 0)
  expect_data_table(b$data(rows = b$rownames, cols = new), ncols = 5)
})

test_that("DataBackendRename / partial rename", {
  old = names(iris)[1:2]
  new = paste0("xx_", old)
  b = DataBackendRename$new(as_data_backend(iris), old, new)
  expect_backend(b)

  expected = c(map_values(names(iris), old, new), "..row_id")
  expect_set_equal(b$colnames, expected)

  expect_set_equal(names(b$data(b$rownames, b$colnames)), expected)
  expect_set_equal(names(b$head()), expected)

  expect_data_table(b$data(rows = b$rownames, cols = expected), ncols = 6)
})

test_that("nested backends", {
  data = as.data.table(iris)
  b1 = as_data_backend(data)
  b2 = DataBackendRename$new(as_data_backend(data), old = names(iris), new = paste0("new_", names(iris)))
  b = DataBackendCbind$new(b1, b2)
  expect_backend(b1)
  expect_backend(b2)
  expect_backend(b)

  expect_set_equal(b$rownames, 1:150)
  expect_set_equal(b$colnames, c(b1$colnames, b2$colnames))
})
