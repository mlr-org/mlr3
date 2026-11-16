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

test_that("bic", {
  task = tsk("mtcars")
  learner = make_learner_lm()$train(task)
  expect_measure(msr("bic"))
  expect_equal(msr("bic")$score(NULL, learner = learner), stats::BIC(learner$model))
})

test_that("bic is NA for models without a logLik method", {
  learner = lrn("regr.rpart")$train(tsk("mtcars"))
  expect_warning({score = msr("bic")$score(NULL, learner = learner)}, "does not support BIC")
  expect_equal(score, NA_real_)
})
