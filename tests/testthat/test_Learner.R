context("Learner")

test_that("construction", {
  l = Learner$new("test-learner", task_type = "classif", predict_types = "prob")
  expect_class(l, "Learner")
})

test_that("Learners are called with invoke / small footprint of call", {
  task = tsk("boston_housing")
  learner = lrn("regr.rpart")
  learner$train(task)
  call = as.character(learner$model$call)
  expect_character(call, min.len = 1L, any.missing = FALSE)
  expect_true(any(grepl("task$formula()", call, fixed = TRUE)))
  expect_true(any(grepl("task$data", call, fixed = TRUE)))
  expect_lt(sum(nchar(call)), 1000)
})


test_that("Extra data slots of learners are kept / reset", {
  task = tsk("boston_housing")
  learner = lrn("regr.rpart")
  learner$train(task)
  learner$state$foo = "bar"
  expect_equal(learner$state$foo, "bar")
  learner$predict(task)
  expect_equal(learner$state$foo, "bar")

  learner$train(task)
  expect_null(learner$state$foo)
})

test_that("task is checked in train() / predict()", {
  learner = lrn("regr.rpart")
  expect_error(learner$train(tsk("pima")), "type")
  expect_error(learner$predict(tsk("pima")), "type")
})

test_that("learner timings", {
  learner = lrn("regr.rpart")
  t = learner$timings
  expect_equal(unname(t), as.double(c(NA, NA)))
  expect_equal(names(t), c("train", "predict"))


  learner$train(tsk("mtcars"))
  t = learner$timings
  expect_number(t[["train"]])
  expect_equal(t[["predict"]], NA_real_)

  learner$predict(tsk("mtcars"))
  t = learner$timings
  expect_number(t[["train"]])
  expect_number(t[["predict"]])
})
