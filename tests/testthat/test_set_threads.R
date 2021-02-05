test_that("set_threads", {
  l1 = lrn("classif.featureless")
  expect_learner(set_threads(l1))

  l2 = lrn("classif.debug")
  expect_null(l2$param_set$values$threads)
  expect_learner(set_threads(l2, 1))
  expect_equal(l2$param_set$values$threads, 1)

  x = list(l1, l2)
  expect_list(set_threads(x, 2))
  expect_equal(l2$param_set$values$threads, 2)
})
