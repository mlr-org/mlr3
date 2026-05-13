test_that("Construction", {
  task = tsk("california_housing")
  p = PredictionRegr$new(row_ids = task$row_ids, truth = task$truth(), response = task$truth())
  expect_prediction(p)
  expect_prediction_regr(p)
})

test_that("Internally constructed Prediction", {
  task = tsk("california_housing")
  lrn = lrn("regr.featureless")
  lrn$predict_type = "se"
  p = lrn$train(task)$predict(task)
  expect_prediction(p)
  expect_prediction_regr(p)
})


test_that("c", {
  task = tsk("california_housing")
  lrn = lrn("regr.featureless")
  lrn$predict_type = "se"
  rr = resample(task, lrn, rsmp("cv", folds = 3))

  pred = do.call(c, rr$predictions())
  expect_prediction(pred)
  expect_prediction_regr(pred)

  dt = as.data.table(pred)
  expect_data_table(dt, nrows = task$nrow, ncols = 4L, any.missing = FALSE)

  # duplicates are detected?
  p1 = get_private(rr)$.data$data$fact$prediction[[1L]]$test
  p2 = get_private(rr)$.data$data$fact$prediction[[1L]]$test
  p3 = c(p1, p2, keep_duplicates = FALSE)
  expect_equal(sort(p1$data$row_ids), sort(p2$data$row_ids))
  expect_equal(sort(p1$data$row_ids), sort(p3$data$row_ids))
  expect_numeric(p3$response, len = length(p1$response), any.missing = FALSE)
  expect_numeric(p3$se, len = length(p1$se), any.missing = FALSE)
})

test_that("c drops se (#250)", {
  task = tsk("california_housing")
  lrn = lrn("regr.featureless")
  rr = resample(task, lrn, rsmp("cv", folds = 3))

  pred = do.call(c, rr$predictions())
  expect_null(pred$data$se)
  expect_false("se" %chin% pred$predict_types)
  expect_true(allMissing(pred$se))
  expect_false("se" %chin% names(as.data.table(pred)))
})

test_that("distr", {
  skip_if_not_installed("distr6")

  task = tsk("mtcars")
  distr = distr6::VectorDistribution$new(
    distribution = "Binomial",
    params = replicate(task$nrow, list(prob = runif(1), size = 10), FALSE)
  )

  p = PredictionRegr$new(row_ids = task$row_ids, truth = task$truth(), distr = distr, check = TRUE)
  expect_output(print(p))
  expect_set_equal(p$predict_types, c("response", "se", "distr"))
  expect_numeric(p$response, len = task$nrow, any.missing = FALSE)
  expect_numeric(p$se, len = task$nrow, any.missing = FALSE, lower = 0)
  expect_integer(p$missing, len = 0)
  expect_prediction(p)

  expect_prediction(c(p, p))
  expect_output(print(p))
  expect_set_equal(p$predict_types, c("response", "se", "distr"))
  expect_numeric(p$response, len = task$nrow, any.missing = FALSE)
  expect_numeric(p$se, len = task$nrow, any.missing = FALSE, lower = 0)
  expect_integer(p$missing, len = 0)
})

test_that("as_prediction_regr", {
  task = tsk("mtcars")
  learner = lrn("regr.featureless")
  p = learner$train(task)$predict(task)

  tab = as.data.table(p)
  p2 = as_prediction_regr(tab)

  expect_equal(tab, as.data.table(p2))
})

test_that("filtering", {
  task = tsk("mtcars")
  p = PredictionRegr$new(row_ids = task$row_ids, truth = task$truth(), response = task$truth())

  p2 = p$clone()$filter(1:3)
  expect_set_equal(p$row_ids, 1:32)
  expect_set_equal(p2$row_ids, 1:3)
  expect_prediction(as_prediction_regr(as.data.table(p2)))
})

test_that("predictions with weights", {
  ll = lrn("regr.debug")$train(as_task_regr(cars, target = "dist"), row_ids = 1)

  pred_with_weights = ll$predict(cars_weights_measure)
  expect_equal(pred_with_weights$weights, rep(c(1, 10), each = 25))

  pred_without_weights = ll$predict(cars_weights_learner) # learner weights are ignored during predict
  expect_null(pred_without_weights$weights)

  expect_error(c(pred_with_weights, pred_without_weights), "Some predictions have weights, others do not")

  expect_equal(c(pred_with_weights, pred_with_weights)$weights, rep(c(1, 10, 1, 10), each = 25))

  expect_equal(
    c(pred_with_weights$clone(deep = TRUE)$filter(1:10), pred_with_weights)$weights,
    rep(c(1, 10, 1, 10), each = 25)[c(1:10, 51:100)]
  )
})

test_that("extra data is stored", {
  LearnerExtra = R6Class(
    "LearnerExtra",
    inherit = LearnerRegrDebug,
    private = list(
      .predict = function(task, ...) {
        pred = super$.predict(task, ...)
        pred$extra = list(extra_col = replicate(length(pred$response), "bar"))
        pred
      }
    )
  )

  learner = LearnerExtra$new()
  learner$train(tsk("mtcars"))
  pred = learner$predict(tsk("mtcars"))
  expect_list(pred$extra, len = 1)
  expect_equal(pred$extra[[1]], replicate(length(pred$response), "bar"))
  expect_prediction(pred)

  LearnerExtra = R6Class(
    "LearnerExtra",
    inherit = LearnerRegrDebug,
    private = list(
      .predict = function(task, ...) {
        pred = super$.predict(task, ...)
        pred$extra = list(extra_col = replicate(2, "bar"))
        pred
      }
    )
  )

  learner = LearnerExtra$new()
  learner$train(tsk("mtcars"))
  expect_error(learner$predict(tsk("mtcars")), "Extra data must have the same length as the number of predictions")
})

test_that("raw data is stored", {
  task = tsk("mtcars")
  raw_obj = list(a = 1, b = "hello")
  p = PredictionRegr$new(row_ids = task$row_ids, truth = task$truth(), response = task$truth(), raw = raw_obj)
  expect_equal(p$raw, raw_obj)

  # raw survives filtering
  p2 = p$clone()$filter(1:3)
  expect_equal(p2$raw, raw_obj)

  # raw is NULL when not provided
  p3 = PredictionRegr$new(row_ids = task$row_ids, truth = task$truth(), response = task$truth())
  expect_null(p3$raw)
})

test_that("raw data is combined into list", {
  task = tsk("mtcars")
  p1 = PredictionRegr$new(row_ids = 1:3, truth = task$truth(1:3), response = task$truth(1:3), raw = list(x = 1))
  p2 = PredictionRegr$new(row_ids = 4:6, truth = task$truth(4:6), response = task$truth(4:6), raw = list(x = 2))
  combined = c(p1, p2)
  expect_list(combined$raw, len = 2)
  expect_equal(combined$raw[[1]], list(x = 1))
  expect_equal(combined$raw[[2]], list(x = 2))

  # combining with and without raw
  p3 = PredictionRegr$new(row_ids = 7:9, truth = task$truth(7:9), response = task$truth(7:9))
  combined2 = c(p1, p3)
  expect_list(combined2$raw, len = 1)
  expect_equal(combined2$raw[[1]], list(x = 1))

  # combining without any raw
  p4 = PredictionRegr$new(row_ids = 10:12, truth = task$truth(10:12), response = task$truth(10:12))
  combined3 = c(p3, p4)
  expect_null(combined3$raw)
})

test_that("raw data via learner predict", {
  LearnerRaw = R6Class(
    "LearnerRaw",
    inherit = LearnerRegrDebug,
    private = list(
      .predict = function(task, ...) {
        pred = super$.predict(task, ...)
        pred$raw = list(upstream_output = "raw_value")
        pred
      }
    )
  )

  learner = LearnerRaw$new()
  learner$train(tsk("mtcars"))
  pred = learner$predict(tsk("mtcars"))
  expect_equal(pred$raw, list(upstream_output = "raw_value"))
})

test_that("predict_raw with regr.rpart", {
  task = tsk("mtcars")
  learner = lrn("regr.rpart", predict_raw = TRUE)
  learner$train(task)
  pred = learner$predict(task)
  expect_numeric(pred$raw, len = task$nrow)

  learner = lrn("regr.rpart")
  learner$train(task)
  pred = learner$predict(task)
  expect_null(pred$raw)
})

test_that("obs_loss works", {
  task = tsk("mtcars")
  learner = lrn("regr.rpart")
  learner$train(task)
  prediction = learner$predict(task)
  obs_loss = prediction$obs_loss(msr("regr.mse"))
  expect_data_table(obs_loss, nrows = task$nrow)
  expect_numeric(obs_loss$regr.mse, len = task$nrow, any.missing = FALSE)

  obs_loss = prediction$obs_loss(msrs(c("regr.mse", "regr.rmse", "regr.ktau")))
  expect_data_table(obs_loss, nrows = task$nrow)
  expect_numeric(obs_loss$regr.mse, len = task$nrow, any.missing = FALSE)
  expect_numeric(obs_loss$regr.rmse, len = task$nrow, any.missing = FALSE)
  expect_true(all(is.na(obs_loss$regr.ktau)))
})
