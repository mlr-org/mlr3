context("mlr_learners_regr_featureless")

test_that("autotest", {
  learner = mlr_learners$get("regr.featureless")
  expect_learner(learner)
  result = run_autotest(learner)
  expect_true(result, info = result$error)
})
