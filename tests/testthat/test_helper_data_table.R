test_that("fget returns values in order of i for small tables", {
  tab = data.table(key_col = c("a", "b", "c"), val = c(1L, 2L, 3L))
  res = fget(tab, i = c("b", "a"), j = "val", key = "key_col")
  expect_equal(res, c(2L, 1L))
})


test_that("fget repeats matches for small tables", {
  tab = data.table(key_col = c("a", "b", "c"), val = c(1L, 2L, 3L))
  res = fget(tab, i = c("b", "b", "c"), j = "val", key = "key_col")
  expect_equal(res, c(2L, 2L, 3L))
})

test_that("fget returns values in order of i for large tables", {
  n = 1200
  tab = data.table(key_col = sprintf("k%04d", 1:n), val = 1:n)
  i_vec = tab$key_col[c(1000, 200, 700)]
  res = fget(tab, i = i_vec, j = "val", key = "key_col")
  expect_equal(res, c(1000, 200, 700))
})

test_that("fget repeats matches for large tables", {
  n = 1200
  tab = data.table(key_col = sprintf("k%04d", 1:n), val = 1:n)
  i_vec = tab$key_col[c(1000, 200, 700, 1000)]
  res = fget(tab, i = i_vec, j = "val", key = "key_col")
  expect_equal(res, c(1000, 200, 700, 1000))
})
