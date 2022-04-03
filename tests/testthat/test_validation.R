test_that("valid_ids are passed correctly to the .train method of the learner", {
  l = lrn("regr.featureless")
  task = tsk("mtcars")
  task$filter(1:3)
  task$set_row_roles(1, "holdout")
  l$train(task, valid_ids = 2)
  # 2 is validation id and 1 is holdout --> the mean should be the value of 3
  expect_equal(mean(task$truth(3)), l$model$location)
  # and the validation error should be the mean squared errror between y_2 and the mean
  expect_equal(mean((l$model$location - task$truth(2))^2), l$model$valid_error)
  l$param_set$values$robust = TRUE
  l$train(task, valid_ids = 2)
  # 2 is validation id and 1 is holdout --> the mean should be the value of 3
  expect_equal(mean(task$truth(3)), l$model$location)
  # and the validation error should be the mean absolute errror between y_2 and the median
  expect_equal(stats::mad(task$truth(2), center = l$model$location), l$model$valid_error)

  # error when valid_ids is not a subset of row_ids
  expect_error(l$train(task, row_ids = 1, valid_ids = 1))
})

test_that("Cannot pass valid_ids if learner does not have property 'validation'", {
  l = lrn("classif.rpart")
  task = tsk("iris")
  expect_error(l$train(task, valid_ids = 10),
    regexp = "Learner does not have property 'validation' but valid_ids is not NULL."
  )
})

test_that("validation works in resample()", {
  l = lrn("regr.featureless")
  task = tsk("mtcars")
  task$set_row_roles(31:32, "holdout")
  resampling = rsmp("custom")
  resampling$instantiate(task, train_sets = list(1:20), test_sets = list(21:30))
  rr = resample(task, l, resampling, store_models = TRUE)
  y_hat = mean(mtcars[["mpg"]][1:20])
  expect_equal(rr$learners[[1]]$model$location, y_hat)
})
