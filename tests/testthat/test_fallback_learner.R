context("fallback learner")

test_that("no fallback_learner", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.crashtest")
  ctrl = mlr_control(use_evaluate = FALSE)

  e = Experiment$new(task = task, learner = learner)
  expect_error(e$train(ctrl = ctrl), "crashtest")

  ctrl = mlr_control(use_evaluate = TRUE)
  e = Experiment$new(task = task, learner = learner)
  e$train(ctrl = ctrl)
  e$predict(ctrl = ctrl)
  e$prediction
  e$score(ctrl = ctrl)
  e$performance
})

test_that("fallback_learner", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.crashtest")
  ctrl = mlr_control(fallback_learner = mlr_learners$get("classif.featureless"), use_evaluate = TRUE)

  e = Experiment$new(task = task, learner = learner)
  e$train(ctrl = ctrl)
  e$predict(ctrl = ctrl)
  e$prediction
  e$score(ctr = ctrl)
  e$performance

})
