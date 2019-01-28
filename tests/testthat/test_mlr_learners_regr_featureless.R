context("mlr_learners_regr_featureless")

test_that("autotest", {
  learner = mlr_learners$get("regr.featureless")
  expect_autotest(learner)
})
