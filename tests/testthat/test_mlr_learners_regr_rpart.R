context("mlr_learners_regr_rpart")

test_that("autotest", {
  learner = mlr_learners$get("regr.rpart")
  expect_autotest(learner)
})
