context("worker")

test_that("Handling of training errors", {
  learner = LearnerClassifDebug$new()
  e = Experiment$new(task = mlr_tasks$get("sonar"), learner = learner)
  r = ResamplingCustom$new()$instantiate(e$task, train_sets = list(1:150))
  e$data$resampling = r
  e$data$iteration = 1L

  res = train_worker(e$task, e$learner, e$train_set, mlr_control())
  expect_list(res, len = 3)
  expect_names(names(res), permutation.of = c("learner", "train_log", "train_time"))
  expect_class(res$learner$model, "unittest")
  expect_null(res$train_log)
  expect_number(res$train_time, lower = 0)

  e$data$learner$param_set$values = list(error_train = TRUE)
  # expect_error(train_worker(e$task, e$learner, e$train_set, mlr_control()), class = "trainError")

  ctrl = mlr_control(encapsulate_train = "evaluate")
  res = train_worker(e$task, e$learner, e$train_set, ctrl)
  expect_list(res, len = 3)
  expect_names(names(res), permutation.of = c("learner", "train_log", "train_time"))
  expect_data_table(res$train_log, min.rows = 1)
})

test_that("experiment_worker", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.featureless")
  resampling = mlr_resamplings$get("cv3")$instantiate(task)
  iteration = 1L
  measures = task$measures

  res = experiment_worker(iteration, task, learner, resampling, measures, mlr_control())
  expect_list(res)
  expect_names(names(res), permutation.of =
    c("iteration", "learner", "train_log", "train_time", "predict_log", "predict_time", "predicted", "performance", "score_time"))

  expect_is(res$predicted, "PredictionData")
  expect_is(res$predicted, "PredictionDataClassif")
})
