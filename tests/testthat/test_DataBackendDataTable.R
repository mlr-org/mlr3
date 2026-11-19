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

test_that("compact_seq", {
  b1 = as_data_backend(data.table(x = 1:4))
  b2 = as_data_backend(data.table(x = 1:4, id = 1:4), primary_key = "id")
  expect_true(b1$compact_seq)
  expect_false(b2$compact_seq)

  # non-positive and unknown row ids do not match a row
  for (b in list(b1, b2)) {
    expect_data_table(b$data(-1L, "x"), nrows = 0L, ncols = 1L)
    expect_data_table(b$data(0L, "x"), nrows = 0L, ncols = 1L)
    expect_equal(b$data(c(3L, -1L, NA, 0L, 9L, 3L, 1L), "x")$x, c(3L, 3L, 1L))
    expect_equal(b$distinct(-(1:3), "x"), list(x = integer()))
    expect_equal(b$missings(-1L, "x"), c(x = 0L))
  }

  # setter resets the hash and rejects backends without sequential ids
  hash = b1$hash
  b1$compact_seq = FALSE
  expect_false(b1$compact_seq)
  expect_false(b1$hash == hash)
  b1$compact_seq = TRUE
  expect_equal(b1$hash, hash)
  expect_error({b1$compact_seq = NA}, "NA") # nolint

  b3 = as_data_backend(data.table(x = 1:4), primary_key = 2:5)
  expect_error({b3$compact_seq = TRUE}, "sequence") # nolint
  expect_false(b3$compact_seq)
})
