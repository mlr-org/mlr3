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
  expect_data_table(b$data(b$rownames, b$colnames), nrows = 150, ncols = 6)
  expect_set_equal(b$distinct(b$rownames, "Species")$Species, distinct_values(iris$Species, drop = FALSE))

  x = b$missings(b$rownames, c("Petal.Width", "Petal.Length"))
  expect_equal(x, set_names(c(0L, 30L), c("Petal.Width", "Petal.Length")))

  expect_equal(b$col_hashes, c(b1$col_hashes, b2$col_hashes)[setdiff(b$colnames, b$primary_key)])
})

test_that("issue #124", {
  task = tsk("iris")
  newcols = cbind(data.table(col = 1:150))
  task$select(character())$cbind(newcols)
  expect_data_table(task$data(cols = "col"), ncols = 1L, nrows = 150L)
})

test_that("cbind backends with same columns", {
  data = as.data.table(iris)
  data$id = 1:150
  data1 = copy(data)
  data2 = copy(data)

  data1$Petal.Width = NA
  b1 = as_data_backend(data1[, -"Sepal.Length"], primary_key = "id")

  data2$Sepal.Width = NA
  b2 = as_data_backend(data2[, c("id", "Sepal.Length", "Sepal.Width", "Petal.Width")], primary_key = "id")

  rows = 1:10
  cols = b2$colnames

  b = DataBackendCbind$new(b1, b2)
  expect_backend(b)
  data = b$head(Inf)
  expect_false(anyMissing(data$Petal.Width))
  expect_true(allMissing(data$Sepal.Width))

  expect_equal(b$col_hashes, insert_named(b1$col_hashes, b2$col_hashes)[setdiff(b$colnames, b$primary_key)])
})

test_that("Backends with different rows", {
  data = as.data.table(iris)
  data$id = 1:150

  b1 = as_data_backend(data[1:20, -"Sepal.Length"], primary_key = "id")
  b2 = as_data_backend(data[1:10, c("id", "Sepal.Length")], primary_key = "id")

  b = DataBackendCbind$new(b1, b2)

  expect_set_equal(b$colnames, c(names(iris), "id"))
  expect_set_equal(b$rownames, 1:20)

  expect_data_table(b$head(Inf), nrows = 20, ncols = 6)

  expect_equal(b$col_hashes, c(b1$col_hashes, b2$col_hashes)[setdiff(b$colnames, b$primary_key)])
})

test_that("missings and distinct count cells filled with NA", {
  b1 = as_data_backend(data.table(id = 1:3, a = 1:3), primary_key = "id")
  b2 = as_data_backend(data.table(id = 2:4, b = c(NA, 3L, 4L)), primary_key = "id")
  b = DataBackendCbind$new(b1, b2)

  expect_equal(b$missings(1:4, c("a", "b", "id")), c(a = 1L, b = 2L, id = 0L))
  expect_equal(b$missings(2:3, c("a", "b")), c(a = 0L, b = 1L))
  expect_equal(b$missings(c(1L, 1L), "b"), c(b = 2L))

  expect_equal(b$distinct(1:4, c("a", "b"), na_rm = FALSE), list(a = c(1:3, NA), b = c(NA, 3L, 4L)))
  expect_equal(b$distinct(NULL, "a", na_rm = FALSE), list(a = c(1:3, NA)))
  expect_equal(b$distinct(3L, c("a", "b"), na_rm = FALSE), list(a = 3L, b = 3L))
  expect_equal(b$distinct(1:4, c("a", "b"), na_rm = TRUE), list(a = 1:3, b = 3:4))

  task = as_task_regr(data.table(y = c(1, 2, 3, 4), x = c(1, 2, 3, 4)), target = "y")
  task$filter(1:2)
  task$cbind(data.table(z = c(10, 20)))
  task$row_roles$use = 1:4
  expect_equal(task$missings(), c(y = 0L, x = 0L, z = 2L))
})
