context("TaskClassif")

test_that("Basic ops on iris task", {
  task = mlr_tasks$get("iris")
  expect_task(task)
  expect_task_supervised(task)
  expect_task_classif(task)
  expect_equal(task$target_names, "Species")
  expect_set_equal(task$class_names, levels(iris$Species))
  expect_identical(task$class_n, nlevels(iris$Species))

  f = task$formula()
  expect_class(f, "formula")
  expect_set_equal(attr(terms(f), "term.labels"), setdiff(names(iris), "Species"))
})

test_that("$class_names and $class_n consider also inactive rows", {
  task = mlr_tasks$get("iris")
  task$set_row_role(1:100, character(0L))

  expect_set_equal(task$class_names, levels(iris$Species))
  expect_identical(task$class_n, 3L)
})

test_that("Factor levels are preserved in prediction", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.featureless")
  learner$predict_type = "prob"
  e = Experiment$new(task, learner)
  e$train(row_ids = 1:100)

  e$predict(1:10)
  pred = as.data.table(e$prediction)
  expect_factor(pred$truth, levels = levels(iris$Species), any.missing = FALSE)
  expect_factor(pred$response, levels = levels(iris$Species), any.missing = FALSE)
  expect_equal(levels(pred$truth), levels(pred$response))
  expect_numeric(pred$prob.virginica, lower = 0, upper = 0, any.missing = FALSE)

  e$predict(101:150)
  pred = as.data.table(e$prediction)
  expect_factor(pred$truth, levels = levels(iris$Species), any.missing = FALSE)
  expect_factor(pred$response, levels = levels(iris$Species), any.missing = FALSE)
  expect_equal(levels(pred$truth), levels(pred$response))
  expect_numeric(pred$prob.virginica, lower = 0, upper = 0, any.missing = FALSE)
})

test_that("Target is character/factor", {
  b = as_data_backend(iris)
  expect_error(TaskClassif$new("iris", backend = b, target = "Sepal.Length"), "Target column")
})

test_that("Replace features", {
  task = mlr_tasks$get("iris")
  data = task$data()[, c("Sepal.Length", "Petal.Length")]
  task$replace_features(data)
  expect_task(task)
  expect_task_classif(task)
  expect_equal(task$nrow, 150)
  expect_equal(task$ncol, 3)
})

test_that("TaskClassif: 0 feature task", {
  b = as_data_backend(iris[, 5L, drop = FALSE])
  task = TaskClassif$new(id = "zero_feat_task", b, target = "Species")
  expect_output(print(task))
  b = task$backend
  expect_backend(b)
  expect_task(task)
  expect_task_supervised(task)
  expect_task_classif(task)
  expect_data_table(task$data(), ncol = 1L)

  lrn = mlr_learners$get("classif.featureless")
  e = Experiment$new(task, lrn)
  e$train()$predict()$score()
  expect_experiment(e)
  expect_number(e$performance, lower = 0.6, upper = 0.7)
})
