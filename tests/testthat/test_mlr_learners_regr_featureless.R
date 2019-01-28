context("mlr_learners_regr_featureless")

test_that("autotest", {
  learner = mlr_learners$get("regr.featureless")
  result = run_autotest(learner)
  expect_true(result, info = result$error)
})
