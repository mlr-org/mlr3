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

  learner = lrn("regr.debug",
    predict_type = "quantiles",
    quantiles = c(0.1, 0.9),
    quantile_response = 0.1)
  fallback = default_fallback(learner)

  expect_class(fallback, "LearnerRegrFeatureless")
  expect_equal(fallback$predict_type, "quantiles")
  expect_equal(fallback$quantiles, c(0.1, 0.9))
  expect_equal(fallback$quantile_response, 0.1)
})
