test_that("regression task", {
  data("mtcars", package = "datasets")
  task_train = as_task_regr(mtcars, "mpg")
  learner = lrn("regr.rpart")
  learner$train(task_train)

  mtcars$mpg = NULL
  task_predict = as_task_unsupervised(mtcars, id = "Test")
  expect_prediction_regr(learner$predict(task_predict))
})


test_that("classification task", {
  data("iris", package = "datasets")
  task_train = as_task_classif(iris, "Species")
  learner = lrn("classif.rpart")
  learner$train(task_train)

  iris$Species = NULL
  task_predict = as_task_unsupervised(iris, id = "Test")
  expect_prediction_classif(learner$predict(task_predict))
})

test_that("empty task", {
  data("iris", package = "datasets")
  task_train = as_task_classif(iris, "Species")
  learner = lrn("classif.rpart")
  learner$train(task_train)

  iris$Species = NULL
  iris = iris[NULL, ]
  task_predict = as_task_unsupervised(iris, id = "Test")
  expect_prediction_classif(learner$predict(task_predict))
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
  expect_prediction_classif(learner$predict(task_predict))
})
