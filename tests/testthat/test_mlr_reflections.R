old_mlr_reflections = as.environment(as.list(mlr_reflections, all.names = TRUE))

on.exit({
  mlr_reflections = as.environment(as.list(old_mlr_reflections, all.names = TRUE))
})

mlr_reflections$task_types = setkeyv(rbind(mlr_reflections$task_types, rowwise_table(
  ~type,  ~package, ~task,              ~learner,        ~prediction,         ~prediction_data,        ~measure,
  "test", "mlr3",   "TaskClassifTest", "LearnerClassif", "PredictionClassif", "PredictionDataClassif", "MeasureClassif"
)), "type")

mlr_reflections$task_col_roles$test = c(mlr_reflections$task_col_roles$classif, "test")
mlr_reflections$task_properties$test = mlr_reflections$task_properties$classif
mlr_reflections$default_measures$test = "classif.ce"

TaskClassifTest = R6Class("TaskClassifTest",
  inherit = TaskClassif,
  public = list(
    initialize = function(id, backend, target, positive = NULL, label = NA_character_, extra_args = list()) {
      super$initialize(id = id, backend = backend, target = target, label = label, extra_args = extra_args)

      self$task_type = "test"
      new_col_roles = named_list(setdiff(mlr_reflections$task_col_roles[["test"]], names(private$.col_roles)), character(0))
      private$.col_roles = insert_named(private$.col_roles, new_col_roles)

      update_classif_property(self, private)
    }
  )
)

b = as_data_backend(load_dataset("PimaIndiansDiabetes2", "mlbench"))
task = TaskClassifTest$new("test", b, target = "diabetes", positive = "pos", label = "Pima Indian Diabetes")
learner = lrn("classif.rpart")
measure = msr("classif.ce")

test_that("assertions work", {
  expect_learner(assert_learner(learner, task))
  expect_error(assert_learner(learner, tsk("california_housing")), "must have task type")
  expect_null(assert_task_learner(task, learner))
  expect_error(assert_task_learner(tsk("california_housing"), learner), "not match type")
  expect_measure(assert_measure(measure, task, learner))
  expect_error(assert_measure(measure, tsk("california_housing"), learner), "is not compatible")
  expect_error(assert_measure(measure, task, lrn("regr.rpart")), "is not compatible")

  at = learner
  class(at) = c("AutoTuner", "Learner", "R6")
  expect_learner(assert_learner(at, task))
  expect_error(assert_learner(at, tsk("california_housing")), "must have task type")
  expect_null(assert_task_learner(task, at))
  expect_error(assert_task_learner(tsk("california_housing"), at))
})

test_that("train and predict works", {
  expect_learner(learner$train(task))
  expect_prediction(learner$predict(task))
})

test_that("resampling works", {
  rr = resample(task, learner, rsmp("cv", folds = 3))
  expect_equal(rr$task_type, "test")

  scores = rr$score(msr("classif.ce"), predictions = TRUE)
  expect_list(scores$prediction_test, "Prediction")
  expect_numeric(scores$classif.ce, any.missing = FALSE)
  expect_number(rr$aggregate(msr("classif.ce")))

  scores = rr$score(predictions = TRUE)
  expect_list(scores$prediction_test, "Prediction")
  expect_numeric(scores$classif.ce, any.missing = FALSE)
  expect_number(rr$aggregate(msr("classif.ce")))
})

test_that("benchmark works", {
  grid = benchmark_grid(task, learner, rsmp("cv", folds = 3))
  bmr = benchmark(grid)
  expect_benchmark_result(bmr)
  expect_equal(bmr$task_type, "test")
  tab = bmr$aggregate(measure)
  expect_data_table(tab, nrows = 1L)
  expect_names(names(tab), type = "unique", identical.to = c("nr", "resample_result", "task_id", "learner_id", "resampling_id", "iters", "classif.ce"))

  grid = benchmark_grid(list(task, tsk("mtcars"), tsk("california_housing")), learner, rsmp("cv", folds = 3))
  expect_error(benchmark(grid), "Multiple task types detected")
})

test_that("set column roles works", {
  expect_task(task$set_col_roles("age", "test"))
  expect_equal(task$col_roles$test, "age")
})

test_that("external packages can set column roles", {
  x = utils::getFromNamespace("mlr_reflections", ns = "mlr3")
  old_col_roles = x$task_col_roles$classif

  on.exit({
    x$col_roles$classif = old_col_roles
  }, add = TRUE)

  x$task_col_roles$classif = c(x$task_col_roles$classif, "extra_role")

  task = tsk("pima")

  with_future(future::multisession, {
    rr = resample(task, lrn("classif.rpart"), rsmp("cv", folds = 3))
  })
  expect_resample_result(rr)
})
