test_that("set_fallback() works", {
  learner = lrn("classif.rpart")
  set_fallback(learner)

  expect_class(learner, "LearnerClassifRpart")
  expect_class(learner$fallback, "LearnerClassifFeatureless")
  expect_equal(learner$fallback$predict_type, "response")

  learner = lrn("classif.rpart", predict_type = "prob")
  set_fallback(learner)

  expect_class(learner, "LearnerClassifRpart")
  expect_class(learner$fallback, "LearnerClassifFeatureless")
  expect_equal(learner$fallback$predict_type, "prob")

  learner = lrn("regr.rpart")
  set_fallback(learner)

  expect_class(learner, "LearnerRegrRpart")
  expect_class(learner$fallback, "LearnerRegrFeatureless")
  expect_equal(learner$fallback$predict_type, "response")

  learner = lrn("regr.debug", predict_type = "se")
  set_fallback(learner)

  expect_class(learner, "LearnerRegrDebug")
  expect_class(learner$fallback, "LearnerRegrFeatureless")
  expect_equal(learner$fallback$predict_type, "se")

  learner = lrn("regr.debug",
    predict_type = "quantiles",
    quantiles = c(0.1, 0.9),
    quantile_response = 0.1)
  set_fallback(learner)

  expect_class(learner, "LearnerRegrDebug")
  expect_class(learner$fallback, "LearnerRegrFeatureless")
  expect_equal(learner$fallback$predict_type, "quantiles")
  expect_equal(learner$fallback$quantiles, c(0.1, 0.9))
  expect_equal(learner$fallback$quantile_response, 0.1)
})
