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

test_that("missing predictions are handled gracefully", {
  task = tsk("sonar")
  learner = lrn("classif.debug", predict_missing = 1, predict_missing_type = "na", predict_type = "prob")

  learner$train(task)
  p = learner$predict(task)
  expect_factor(p$response, levels = task$class_names)
  expect_true(all(is.na(p$response)))

  expect_true(all(is.na(p$prob)))

  learner = lrn("classif.debug", predict_missing = 0.5, predict_missing_type = "omit", predict_type = "prob")
  learner$train(task)
  expect_error(learner$predict(task), "observations")
})

test_that("predict_newdata with weights (#519)", {
  task = tsk("boston_housing")
  task$set_col_roles("nox", "weight")
  learner = lrn("regr.featureless")
  learner$train(task)
  expect_prediction(learner$predict(task))

  # w/o weights
  expect_prediction(learner$predict_newdata(task$data()))

  # w weights
  expect_prediction(learner$predict_newdata(task$data(cols = c(task$target_names, task$feature_names, "nox"))))
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
