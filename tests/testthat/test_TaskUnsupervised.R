test_that("regression task", {
  data("mtcars", package = "datasets")
  task_train = as_task_regr(mtcars, "mpg")
  learner = lrn("regr.rpart")
  learner$train(task_train)

  mtcars$mpg = NULL
  task_predict = as_task_unsupervised(mtcars, id = "Test")
  expect_task_unsupervised(task_predict)
  expect_prediction(learner$predict(task_predict))
})


test_that("classification task", {
  data("iris", package = "datasets")
  task_train = as_task_classif(iris, "Species")
  learner = lrn("classif.rpart")
  learner$train(task_train)

  iris$Species = NULL
  task_predict = as_task_unsupervised(iris, id = "Test")
  expect_task_unsupervised(task_predict)
  expect_prediction(learner$predict(task_predict))
})

test_that("empty classif task", {
  data("iris", package = "datasets")
  task_train = as_task_classif(iris, "Species")
  learner = lrn("classif.rpart")
  learner$train(task_train)

  iris$Species = NULL
  iris = iris[NULL, ]
  task_predict = as_task_unsupervised(iris, id = "Test")
  expect_task_unsupervised(task_predict)
  expect_prediction(learner$predict(task_predict))
})

test_that("empty regr task", {
  data("mtcars", package = "datasets")
  task_train = as_task_regr(mtcars, "mpg")
  learner = lrn("regr.rpart")
  learner$train(task_train)

  mtcars$mpg = NULL
  mtcars = mtcars[NULL, ]
  task_predict = as_task_unsupervised(mtcars, id = "Test")
  expect_task_unsupervised(task_predict)
  expect_prediction(learner$predict(task_predict))
})

test_that("fallback", {
  data("iris", package = "datasets")
  task_train = as_task_classif(iris, "Species")
  learner = lrn("classif.debug", error_predict = 1)
  learner$encapsulate = c(predict = "evaluate")
  learner$fallback = lrn("classif.featureless")
  learner$train(task_train)

  iris$Species = NULL
  task_predict = as_task_unsupervised(iris, id = "Test")
  expect_task_unsupervised(task_predict)
  expect_prediction(learner$predict(task_predict))
})

test_that("assertions work", {
  data("iris", package = "datasets")
  iris$Species = NULL
  task = as_task_unsupervised(iris, id = "Test")
  learner = lrn("classif.rpart")

  expect_error(assert_learnable(task, learner), "cannot be trained")
  expect_null(assert_predictable(task, learner))

  at = learner
  class(at) = c("AutoTuner", "Learner", "R6")
  expect_error(assert_learnable(task, at), "cannot be trained")
  expect_null(assert_predictable(task, at))
})

test_that("train with unsupervised task fails", {
  data("iris", package = "datasets")
  iris$Species = NULL
  task_train = as_task_unsupervised(iris, id = "Test")
  learner = lrn("classif.rpart")
  expect_error(learner$train(task_train), "cannot be trained")
})
