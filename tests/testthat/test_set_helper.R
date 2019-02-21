context("set helper")

test_that("funique", {
  res = funique(sample(1:3, 10, replace = TRUE))
  expect_equal(anyDuplicated(res), 0L)
  expect_true(attr(res, ".unique"))
})

test_that("set_union", {
  x = rep(1:4, 2)
  y = rep(2:5, 2)
  res = set_union(x, y)
  expect_equal(anyDuplicated(res), 0L)
  expect_true(attr(res, ".unique"))
  expect_set_equal(res, 1:5)
})


test_that("set_intersect", {
  x = rep(1:4, 2)
  y = rep(2:5, 2)
  res = set_intersect(x, y)
  expect_equal(anyDuplicated(res), 0L)
  expect_true(attr(res, ".unique"))
  expect_set_equal(res, 2:4)


  x = funique(1:3)
  y = c(1L, 1L)
  res = set_intersect(x, y)
  expect_equal(anyDuplicated(res), 0L)
  expect_true(attr(res, ".unique"))
  expect_set_equal(res, 1L)

  y = funique(1:3)
  x = c(1L, 1L)
  res = set_intersect(x, y)
  expect_equal(anyDuplicated(res), 0L)
  expect_true(attr(res, ".unique"))
  expect_set_equal(res, 1L)
})

test_that("set_diff", {
  x = rep(1:4, 2)
  y = rep(2:5, 2)

  res = set_diff(x, y)
  expect_equal(anyDuplicated(res), 0L)
  expect_true(attr(res, ".unique"))
  expect_set_equal(res, 1L)

  res = set_diff(y, x)
  expect_equal(anyDuplicated(res), 0L)
  expect_true(attr(res, ".unique"))
  expect_set_equal(res, 5L)
})

test_that("set_equal", {
  x = rep(1:4, 2)
  y = rep(2:5, 2)

  expect_false(set_equal(x, y))
  expect_true(set_equal(x, rev(x)))
})
