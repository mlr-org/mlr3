test_that("Dictionary: clone works", {
  t1 = tsk("iris")
  expect_task(t1)
  t2 = tsk("iris")
  expect_task(t2)
  expect_different_address(t1, t2)
})

test_that("$keys(pattern) works", {
  expect_subset(mlr_learners$keys("classif"), mlr_learners$keys(), empty.ok = FALSE)
})
