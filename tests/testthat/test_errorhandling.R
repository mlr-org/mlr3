context("error handling")

test_that("no encapsulation", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.debug")
  ctrl = mlr_control(encapsulate_train = "none", encapsulate_predict = "none")

  learner$param_set$values = list(error_train = TRUE)
  e = Experiment$new(task = task, learner = learner)
  expect_error(e$train(ctrl = ctrl), "classif.debug->train", class = "trainError")

  learner$param_set$values = list(error_predict = TRUE)
  e = Experiment$new(task = task, learner = learner)
  e$train(ctrl = ctrl)
  expect_error(e$predict(ctrl = ctrl), "classif.debug->predict", class = "predictError")
})
