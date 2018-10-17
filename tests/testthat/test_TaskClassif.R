context("TaskClassif")

test_that("Basic ops on iris task", {
  task = mlr_tasks$get("iris")
  expect_task(task)
  expect_task_supervised(task)
  expect_task_classif(task)
  expect_equal(task$target_names, "Species")
  expect_set_equal(task$class_names, levels(iris$Species))
  expect_identical(task$class_n, nlevels(iris$Species))

  f = task$formula
  expect_class(f, "formula")
  expect_set_equal(attr(terms(f), "term.labels"), setdiff(names(iris), "Species"))
})

test_that("$class_names and $class_n only consider active rows", {
  task = mlr_tasks$get("iris")
  task$set_row_role(1:100, character(0L))

  expect_identical(task$class_names, "virginica")
  expect_identical(task$class_n, 1L)
})

test_that("Factor levels are preserved in prediction", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.dummy")
  learner$predict_type = "prob"
  e = Experiment$new(task, learner)
  e$train(subset = 1:100)

  e$predict(1:10)
  pred = e$prediction
  expect_factor(pred$truth, levels = levels(iris$Species), any.missing = FALSE)
  expect_factor(pred$response, levels = levels(iris$Species), any.missing = FALSE)
  expect_equal(levels(pred$truth), levels(pred$response))
  expect_numeric(pred$prob.virginica, lower = 0, upper = 0, any.missing = FALSE)

  e$predict(101:150)
  pred = e$prediction
  expect_factor(pred$truth, levels = levels(iris$Species), any.missing = FALSE)
  expect_factor(pred$response, levels = levels(iris$Species), any.missing = FALSE)
  expect_equal(levels(pred$truth), levels(pred$response))
  expect_numeric(pred$prob.virginica, lower = 0, upper = 0, any.missing = FALSE)
})
