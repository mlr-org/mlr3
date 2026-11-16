make_learner_lm = function() {
  LearnerRegrLm = R6Class(
    "LearnerRegrLm",
    inherit = LearnerRegr,
    public = list(
      initialize = function() {
        super$initialize(id = "regr.lm", feature_types = c("integer", "numeric"), predict_types = "response")
      }
    ),
    private = list(
      .train = function(task) stats::lm(task$formula(), data = task$data()),
      .predict = function(task) list(response = unname(stats::predict(self$model, newdata = task$data())))
    )
  )
  LearnerRegrLm$new()
}

test_that("aic", {
  task = tsk("mtcars")
  learner = make_learner_lm()$train(task)
  expect_measure(msr("aic"))
  expect_equal(msr("aic")$score(NULL, learner = learner), stats::AIC(learner$model))
  expect_equal(msr("aic", k = 3)$score(NULL, learner = learner), stats::AIC(learner$model, k = 3))

  rr = resample(task, make_learner_lm(), rsmp("cv", folds = 3), store_models = TRUE)
  expect_equal(rr$score(msr("aic"))$aic, map_dbl(rr$learners, function(l) stats::AIC(l$model)))
})

test_that("aic is NA for models without a logLik method", {
  learner = lrn("regr.rpart")$train(tsk("mtcars"))
  expect_warning({score = msr("aic")$score(NULL, learner = learner)}, "does not support AIC")
  expect_equal(score, NA_real_)
})

test_that("aic does not hide errors of the logLik method", {
  registerS3method("logLik", "broken_model", function(object, ...) stop("logLik failed"))
  learner = lrn("regr.featureless")$train(tsk("mtcars"))
  learner$state$model = structure(list(), class = "broken_model")
  expect_error(msr("aic")$score(NULL, learner = learner), "logLik failed")
})
