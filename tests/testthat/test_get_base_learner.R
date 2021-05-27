test_that("get_base_learner", {
  l = lrn("classif.featureless")
  expect_equal(get_base_learner(l)$hash, l$hash)
  expect_null(get_base_learner(rsmp("holdout")))
})
