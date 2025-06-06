test_that("predict method works", {
  task = tsk("sonar")
  lrn = lrn("classif.featureless")$train(task)

  newdata = task$data(1:3)
  expect_factor(predict(lrn, newdata = newdata), len = 3)
  expect_factor(predict(lrn, newdata = newdata, predict_type = "response"), len = 3)
  expect_error(predict(lrn, newdata = newdata, predict_type = "prob"), "'prob'")

  lrn = lrn("classif.featureless", predict_type = "prob")$train(task)
  expect_factor(predict(lrn, newdata = newdata), len = 3)
  expect_factor(predict(lrn, newdata = newdata, predict_type = "response"), len = 3)
  expect_matrix(predict(lrn, newdata = newdata, predict_type = "prob"), nrows = 3, ncols = 2)

  expect_true(uniqueN(predict(lrn, newdata, method = "mode")) == 1L)
})

test_that("missing predictions are handled gracefully / classif", {
  task = tsk("sonar")
  learner = lrn("classif.debug", predict_missing = 1, predict_missing_type = "na", predict_type = "prob")
  learner$train(task)

  p = learner$predict(task)
  expect_factor(p$response, levels = task$class_names)
  expect_true(all(is.na(p$response)))
  expect_true(all(is.na(p$prob)))
  expect_error(p$score(), "missing")

  learner = lrn("classif.debug", predict_missing = 0.5, predict_missing_type = "omit", predict_type = "prob")
  learner$train(task)
  expect_error(learner$predict(task), "observations")
})

test_that("missing predictions are handled gracefully / regr", {
  task = tsk("mtcars")
  learner = lrn("regr.debug", predict_missing = 1, predict_missing_type = "na", predict_type = "se")
  learner$train(task)

  p = learner$predict(task)
  expect_numeric(p$response)
  expect_numeric(p$se)
  expect_true(all(is.na(p$response)))
  expect_true(all(is.na(p$se)))
  expect_error(p$score(), "missing")

  learner = lrn("regr.debug", predict_missing = 0.5, predict_missing_type = "omit", predict_type = "se")
  learner$train(task)
  expect_error(learner$predict(task), "observations")
})


test_that("predict_newdata with weights (#519)", {
  # we had a problem where predict did not work if weights were present in the task
  # especially with the "predict_newdata" function

  task = tsk("california_housing")
  task$set_col_roles("households", "weights_learner")
  task$set_col_roles("total_rooms", "weights_measure")
  learner = lrn("regr.featureless")
  learner$train(task)

  # predict with different API calls
  # normal predict on the task
  pred = learner$predict(task)
  expect_prediction(pred)
  expect_equal(pred$weights, tsk("california_housing")$data()$total_rooms)
  # w/o weights in the new-df
  pred = learner$predict_newdata(task$data())
  expect_prediction(pred)
  expect_null(pred$weights)
  # w weights in the new-df
  pred = learner$predict_newdata(task$data(cols = c(task$target_names, task$feature_names, "households")))
  expect_prediction(pred)
  expect_null(pred$weights)

  # now measure-weights are present
  pred = learner$predict_newdata(task$data(cols = c(task$target_names, task$feature_names, "total_rooms")))
  expect_prediction(pred)
  expect_equal(pred$weights, tsk("california_housing")$data()$total_rooms)
  # now both are present
  pred = learner$predict_newdata(task$data(cols = c(task$target_names, task$feature_names, "households", "total_rooms")))
  expect_prediction(pred)
  expect_equal(pred$weights, tsk("california_housing")$data()$total_rooms)

})

test_that("parallel predict works", {
  skip_if_not_installed("future")
  task = tsk("sonar")
  lrn = lrn("classif.featureless")$train(task)

  lrn$parallel_predict = FALSE
  p1 = lrn$predict(task, row_ids = 20:1)

  lrn$parallel_predict = TRUE
  p2 = with_future(future::multisession,
    lrn$predict(task, row_ids = 20:1)
  )

  expect_equal(as.data.table(p1), as.data.table(p2))
})



