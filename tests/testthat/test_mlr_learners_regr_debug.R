test_that("validation works", {
  task = tsk("mtcars")
  learner = lrn("regr.debug")
  expect_error(learner$inner_valid_scores(), "No model")
  learner$train(task)
  expect_error(learner$inner_valid_scores(), "No inner validation")
  learner$param_set$set_values(
    validate = 0.2
  )
  learner$train(task)
  expect_list(learner$inner_valid_scores(), types = "numeric")
  expect_permutation(names(learner$inner_valid_scores()), c("mse", "mae"))
})

test_that("inner tuning works", {
  task = tsk("mtcars")
  learner = lrn("regr.debug", save_tasks = TRUE, response = "tune")
  expect_error(learner$train(task), "Can only tune if")
  learner$param_set$set_values(validate = 0.2)
  learner$train(task)
  # when we tune, the response is the mean over all data
  expect_equal(learner$model$response, mean(task$truth()))

  # otherwise the response does not use the validation data
  learner$param_set$set_values(response = NULL, validate = 0.2)
  learner$train(task)
  expect_equal(learner$model$response, mean(task$truth(learner$model$task_train$row_ids)))
})

test_that("set_inner_tuning works", {
  learner = lrn("regr.debug")
  expect_error(set_inner_tuning(learner), "Parameter 'validate'")
  set_inner_tuning(learner, validate = 0.2)
})


