context("Learner")

test_that("construction", {
  l = Learner$new("test-learner", task_type = "classif", predict_types = "prob")
  expect_class(l, "Learner")
})

test_that("Learners are called with invoke / small footprint of call", {
  task = mlr_tasks$get("boston_housing")
  learner = mlr_learners$get("regr.rpart")
  learner$train(task)
  call = as.character(learner$model$call)
  expect_character(call, min.len = 1L, any.missing = FALSE)
  expect_true(any(grepl("task$formula()", call, fixed = TRUE)))
  expect_true(any(grepl("task$data", call, fixed = TRUE)))
  expect_lt(sum(nchar(call)), 1000)
})


test_that("Extra data slots of learners are kept", {
  task = mlr_tasks$get("boston_housing")
  learner = mlr_learners$get("regr.rpart")
  learner$data$foo = "bar"
  learner$train(task)
  expect_equal(learner$data$foo, "bar")
  learner$predict(task)
  expect_equal(learner$data$foo, "bar")
})
