library(mlr3learners)

test_that("continue method works", {
  task = tsk("iris")
  learner = lrn("classif.xgboost", nrounds = 10)
  learner$train(task)
  m1 = learner$model

  learner$param_set$values$nrounds = 20
  learner$continue(task)
  m2 = learner$model

  expect_equal(learner$param_set$values$nrounds, learner$model$niter)
  expect_false(identical(m1, m2))

  learner = lrn("classif.xgboost", nrounds = 10)
  learner$train(task)

  expect_error(learner$continue(task),
    regexp = "No additional boosting iterations provided")

  learner = lrn("classif.xgboost", nrounds = 10)

  expect_error(learner$continue(task),
    regexp = "Learner does not contain a model")

  learner = lrn("classif.rpart")
  learner$train(task)

  expect_error(learner$continue(task),
    regexp = "Learner 'classif.rpart' does not support continue")

  task = tsk("iris")
  learner = lrn("classif.xgboost", nrounds = 10)
  learner$train(task)
  task$select("Petal.Length")

  expect_error(learner$continue(task),
    regexp = "Supplied task does not allow to continue training")
})
