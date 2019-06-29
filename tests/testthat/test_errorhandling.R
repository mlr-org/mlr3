context("error handling")

skip("fallback not implemented")

test_that("no encapsulation", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.debug")
  ctrl = mlr_control(encapsulate_train = "none", encapsulate_predict = "none")

  learner$param_set$values = list(error_train = TRUE)
  e = Experiment$new(task = task, learner = learner)
  # expect_error(e$train(ctrl = ctrl), "classif.debug->train", class = "trainError")
  expect_error(e$train(ctrl = ctrl), "classif.debug->train")

  learner$param_set$values = list(error_predict = TRUE)
  e = Experiment$new(task = task, learner = learner)
  e$train(ctrl = ctrl)
  # expect_error(e$predict(ctrl = ctrl), "classif.debug->predict", class = "predictError")
  expect_error(e$predict(ctrl = ctrl), "classif.debug->predict")
})

test_that("encapsulation + no fallback", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.debug")
  ctrl = mlr_control(encapsulate_train = "evaluate", encapsulate_predict = "evaluate")

  learner$param_set$values = list(error_train = TRUE)
  learner$train(task, ctrl = ctrl)
  learner$log

  e = Experiment$new(task = task, learner = learner)
  e$train(ctrl = ctrl)
  expect_true(e$state == "trained")

  expect_error(e$predict())
  expect_true(e$state == "trained")

  expect_error(e$score(), "predicted")
})


test_that("fallback learner", {
  # no fail
  learner = mlr_learners$get("classif.debug")
  learner$fallback = "classif.featureless"
  e = Experiment$new("sonar", learner)

  e$train()
  expect_is(e$model, "classif.debug_model")
  expect_is(e$learner$model, "classif.debug_model")
  expect_false(any(e$has_errors))

  e$predict()
  expect_prediction(e$prediction)
  expect_false(any(e$has_errors))
  expect_character(e$log()$errors, len = 0L)


  # fail during train
  learner = mlr_learners$get("classif.debug", param_vals = list(error_train = TRUE))
  learner$fallback = "classif.featureless"
  e = Experiment$new("sonar", learner, ctrl = mlr_control(encapsulate_train = "evaluate"))

  e$train()
  expect_null(e$model)
  expect_null(e$learner$model)
  expect_true(e$has_errors[["train"]])
  expect_string(e$log("train")$errors, fixed = "classif.debug")

  e$predict()
  expect_prediction(e$prediction)
  expect_false(e$has_errors[["predict"]])
  expect_string(e$log("predict")$warnings, fixed = "fallback")

  # fail during predict
  learner = mlr_learners$get("classif.debug", param_vals = list(error_predict = TRUE))
  learner$fallback = "classif.featureless"
  e = Experiment$new("sonar", learner, ctrl = mlr_control(encapsulate_predict = "evaluate"))

  e$train()
  expect_is(e$model, "classif.debug_model")
  expect_is(e$learner$model, "classif.debug_model")
  expect_false(e$has_errors[["train"]])
  expect_false(e$has_errors[["predict"]])
  expect_character(e$log("train")$errors, len = 0L)

  e$predict()
  expect_prediction(e$prediction)
  expect_false(e$has_errors[["train"]])
  expect_true(e$has_errors[["predict"]])
  expect_string(e$log("predict")$errors, fixed = "classif.debug")
  expect_string(e$log("predict")$warnings, fixed = "fallback")

  # fail during train+predict
  learner = mlr_learners$get("classif.debug", param_vals = list(error_train = TRUE, error_predict = TRUE))
  learner$fallback = "classif.featureless"
  e = Experiment$new("sonar", learner, ctrl = mlr_control(encapsulate_train = "evaluate", encapsulate_predict = "evaluate"))

  e$train()
  expect_null(e$learner$model)
  expect_true(e$has_errors[["train"]])
  expect_false(e$has_errors[["predict"]])
  expect_string(e$log("train")$errors, fixed = "classif.debug")

  e$predict()
  expect_prediction(e$prediction)
  expect_true(e$has_errors[["train"]])
  expect_false(e$has_errors[["predict"]]) # classif.debug is not called here
  expect_string(e$log("train")$errors, fixed = "classif.debug")
  expect_string(e$log("predict")$warnings, fixed = "fallback")

  # NA predictions
  learner = mlr_learners$get("classif.debug", param_vals = list(predict_missing = 0.5))
  learner$fallback = "classif.featureless"
  e = Experiment$new("sonar", learner)$train()$predict()

  expect_string(e$log("predict")$warnings, fixed = "augment")
  expect_false(anyMissing(e$prediction$response))
})
