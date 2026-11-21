test_that("fallback = default_fallback() works", {
  learner = lrn("classif.rpart")
  fallback = default_fallback(learner)

  expect_class(fallback, "LearnerClassifFeatureless")
  expect_equal(fallback$predict_type, "response")

  learner = lrn("classif.rpart", predict_type = "prob")
  fallback = default_fallback(learner)

  expect_class(fallback, "LearnerClassifFeatureless")
  expect_equal(fallback$predict_type, "prob")

  learner = lrn("regr.rpart")
  fallback = default_fallback(learner)

  expect_class(fallback, "LearnerRegrFeatureless")
  expect_equal(fallback$predict_type, "response")

  learner = lrn("regr.debug", predict_type = "se")
  fallback = default_fallback(learner)

  expect_class(fallback, "LearnerRegrFeatureless")
  expect_equal(fallback$predict_type, "se")

  learner = lrn("regr.debug", predict_type = "quantiles", quantiles = c(0.1, 0.9), quantile_response = 0.1)
  fallback = default_fallback(learner)

  expect_class(fallback, "LearnerRegrFeatureless")
  expect_equal(fallback$predict_type, "quantiles")
  expect_equal(fallback$quantiles, c(0.1, 0.9))
  expect_equal(fallback$quantile_response, 0.1)
})

test_that("default_fallback() works for learners inheriting directly from Learner", {
  LearnerGeneric = R6Class("LearnerGeneric", inherit = Learner)

  learner = LearnerGeneric$new("generic", task_type = "classif", predict_types = c("response", "prob"))
  learner$predict_type = "prob"
  fallback = default_fallback(learner)

  expect_class(fallback, "LearnerClassifFeatureless")
  expect_equal(fallback$predict_type, "prob")

  learner = LearnerGeneric$new("generic", task_type = "regr", predict_types = c("response", "se"))
  learner$predict_type = "se"
  fallback = default_fallback(learner)

  expect_class(fallback, "LearnerRegrFeatureless")
  expect_equal(fallback$predict_type, "se")
})

test_that("default_fallback() errors if no fallback learner is available", {
  LearnerGeneric = R6Class("LearnerGeneric", inherit = Learner)
  learner = LearnerGeneric$new("generic", task_type = "classif", predict_types = "response")

  learner$task_type = "unsupervised"
  expect_error(default_fallback(learner), "Could not find default fallback learner", class = "Mlr3ErrorConfig")

  old_task_types = mlr_reflections$task_types
  on.exit({
    mlr_reflections$task_types = old_task_types
  })
  mlr_reflections$task_types = setkeyv(
    rbind(old_task_types, old_task_types["regr"][, `:=`(type = "test", learner = "LearnerTest")]),
    "type"
  )
  learner$task_type = "test"
  expect_error(default_fallback(learner), "Could not find default fallback learner", class = "Mlr3ErrorConfig")
})
