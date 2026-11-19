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
  expect_data_table(b$data(b$rownames, b$colnames), nrows = 150, ncols = 6)
  expect_set_equal(b$distinct(b$rownames, "Species")$Species, distinct_values(iris$Species, drop = FALSE))

  x = b$missings(b$rownames, c("Petal.Width", "Petal.Length"))
  expect_equal(x, set_names(c(0L, 30L), c("Petal.Width", "Petal.Length")))

  # all col-hashes are mutually disjoint
  expect_length(intersect(b1$col_hashes, b2$col_hashes), 0)
  expect_length(intersect(b$col_hashes, b1$col_hashes), 0)
  expect_length(intersect(b$col_hashes, b2$col_hashes), 0)
})

test_that("Backends with different cols", {
  data = as.data.table(iris)
  data$id = 1:150
  fn = c("Sepal.Width", "Species", "id")
  b1 = as_data_backend(data[1:100, c("Sepal.Length", "Sepal.Width", "Species", "id"), with = FALSE], primary_key = "id")
  b2 = as_data_backend(data[101:150, fn, with = FALSE], primary_key = "id")
  b = DataBackendRbind$new(b1, b2)

  expect_equal(b$ncol, 4)

  h = b$head(60)
  expect_data_table(h, ncols = 4, nrows = 60)

  h = b$head(120)
  expect_data_table(h, ncols = 4, nrows = 120)

  expect_data_table(b$data(rows = 1:120, cols = b$colnames), nrows = 120, ncols = 4)
})

test_that("Backends with same rows", {
  data = as.data.table(iris)
  data$id = 1:150
  data1 = data[1:20]
  data2 = data[6:10][, Petal.Length := NA]
  data2 = rbind(data2, data[101])

  b1 = as_data_backend(data1, primary_key = "id")
  b2 = as_data_backend(data2, primary_key = "id")

  b = DataBackendRbind$new(b1, b2)
  expect_backend(b)
  expect_set_equal(b$rownames, c(1:20, 101))

  data = b$data(b$rownames[1:20], b$colnames)
  expect_false(anyMissing(data$Petal.Length[1:5]))
  expect_false(anyMissing(data$Petal.Length[11:20]))
  expect_true(allMissing(data$Petal.Length[6:10]))
})

test_that("missings and distinct count cells filled with NA", {
  b1 = as_data_backend(data.table(id = 1:2, x = 1:2, extra = c("a", "b")), primary_key = "id")
  b2 = as_data_backend(data.table(id = 3:5, x = c(3L, NA, 5L), other = 1:3), primary_key = "id")
  b = DataBackendRbind$new(b1, b2)

  expect_equal(b$missings(1:5, c("x", "extra", "other", "id")), c(x = 1L, extra = 3L, other = 2L, id = 0L))
  expect_equal(b$missings(1:2, c("extra", "other")), c(extra = 0L, other = 2L))
  expect_equal(b$missings(c(3L, 3L), "extra"), c(extra = 2L))
  expect_equal(b$missings(integer(), "extra"), c(extra = 0L))

  expect_equal(b$distinct(1:5, "extra", na_rm = FALSE), list(extra = c("a", "b", NA)))
  expect_equal(b$distinct(NULL, "extra", na_rm = FALSE), list(extra = c("a", "b", NA)))
  expect_equal(b$distinct(1:2, "extra", na_rm = FALSE), list(extra = c("a", "b")))
  expect_equal(b$distinct(1:5, "extra", na_rm = TRUE), list(extra = c("a", "b")))

  task = as_task_regr(data.table(y = c(1, 2), x = c(1, 2), extra = c(1, 2)), target = "y")
  task$select("x")
  task$rbind(data.table(y = c(3, 4), x = c(3, 4)))
  task$col_roles$feature = c("x", "extra")
  expect_equal(task$missings(), c(y = 0L, x = 0L, extra = 2L))
})
