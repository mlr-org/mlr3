# fget_key ---------------------------------------------------------------------

test_that("fget_key works with character keys", {
  # small table
  tab = data.table(key_col = c("k0001", "k0002", "k0003"), val = c(1L, 2L, 3L))
  res = fget_key(tab, i = "k0002", j = "val", key = "key_col")
  expect_equal(res, 2L)

  # large table
  n = 1200L
  tab = data.table(key_col = sprintf("k%04d", 1:n), val = 1:n)
  res = fget_key(tab, i = "k1000", j = "val", key = "key_col")
  expect_equal(res, 1000L)
})

test_that("fget_key returns all matches on non-unique key column", {
  # small table
  tab = data.table(key_col = c("k0001", "k0002", "k0002"), val = c(1L, 2L, 3L))
  res = fget_key(tab, i = "k0002", j = "val", key = "key_col")
  expect_equal(res, c(2L, 3L))

  # large table
  n = 1200L
  tab = data.table(key_col = sprintf("k%04d", 1:n), val = 1:n)
  tab[1001, key_col := "k1000"]
  res = fget_key(tab, i = "k1000", j = "val", key = "key_col")
  expect_equal(res, c(1000L, 1001L))
})

test_that("fget_key works with non-character keys", {
  # small table
  tab = data.table(key_col = c(1L, 2L, 3L), val = c(1L, 2L, 3L))
  res = fget_key(tab, i = 2L, j = "val", key = "key_col")
  expect_equal(res, 2L)

  # large table
  n = 1200L
  tab = data.table(key_col = 1:n, val = 1:n)
  res = fget_key(tab, i = 1000L, j = "val", key = "key_col")
  expect_equal(res, 1000L)
})

test_that("fget returns empty vector if no match", {
  # small table
  tab = data.table(key_col = c("k0001", "k0002", "k0003"), val = c(1L, 2L, 3L))
  res = fget_key(tab, i = "k0004", j = "val", key = "key_col")
  expect_equal(res, integer())

  # large table
  n = 1200L
  tab = data.table(key_col = 1:n, val = 1:n)
  res = fget_key(tab, i = 1201L, j = "val", key = "key_col")
  expect_equal(res, integer())
})

# fget_keys ---------------------------------------------------------------------

test_that("fget_keys returns values in order of i", {
  # small table
  tab = data.table(key_col = c("k0001", "k0002", "k0003"), val = c(1L, 2L, 3L))
  res = fget_keys(tab, i = c("k0002", "k0001"), j = "val", key = "key_col")
  expect_equal(res, c(2L, 1L))

  # large table
  n = 1200L
  tab = data.table(key_col = sprintf("k%04d", 1:n), val = 1:n)
  res = fget_keys(tab, i = c("k1000", "k0201"), j = "val", key = "key_col")
  expect_equal(res, c(1000L, 201L))
})

test_that("non-matching elements return NA", {
  # small table
  tab = data.table(key_col = c("k0001", "k0002", "k0003"), val = c(1L, 2L, 3L))
  res = fget_keys(tab, i = c("k0002", "k0004"), j = "val", key = "key_col")
  expect_equal(res, c(2L, NA))

  # large table
  n = 1200L
  tab = data.table(key_col = sprintf("k%04d", 1:n), val = 1:n)
  res = fget_keys(tab, i = c("k1000", "k1201"), j = "val", key = "key_col")
  expect_equal(res, c(1000L, NA))
})

test_that("fget_keys repeats matches", {
  # small table
  tab = data.table(key_col = c("k0001", "k0002", "k0003"), val = c(1L, 2L, 3L))
  res = fget_keys(tab, i = c("k0002", "k0002", "k0003"), j = "val", key = "key_col")
  expect_equal(res, c(2L, 2L, 3L))

  # large table
  n = 1200L
  tab = data.table(key_col = sprintf("k%04d", 1:n), val = 1:n)
  res = fget_keys(tab, i = tab$key_col[c(1000, 200, 700, 1000)], j = "val", key = "key_col")
  expect_equal(res, c(1000L, 200L, 700L, 1000L))
})

test_that("fget_keys on non-unique key column", {
  tab = data.table(key_col = c("k0001", "k0002", "k0002"), val = c(1L, 2L, 3L))
  res = fget_keys(tab, i = c("k0001", "k0002"), j = "val", key = "key_col")
  expect_equal(res, c(1L, 2L))

  n = 1200L
  tab = data.table(key_col = sprintf("k%04d", 1:n), val = 1:n)
  tab[3, key_col := "k0002"]
  res = fget_keys(tab, i = c("k0001", "k0002"), j = "val", key = "key_col")
  expect_equal(res, c(1L, 2L))
})

