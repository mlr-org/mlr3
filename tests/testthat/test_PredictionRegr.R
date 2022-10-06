test_that("Construction", {
  task = tsk("boston_housing")
  p = PredictionRegr$new(row_ids = task$row_ids, truth = task$truth(), response = task$truth())
  expect_prediction(p)
  expect_prediction_regr(p)
})

test_that("Internally constructed Prediction", {
  task = tsk("boston_housing")
  lrn = lrn("regr.featureless")
  lrn$predict_type = "se"
  p = lrn$train(task)$predict(task)
  expect_prediction(p)
  expect_prediction_regr(p)
})


test_that("c", {
  task = tsk("boston_housing")
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
  task = tsk("boston_housing")
  lrn = lrn("regr.featureless")
  rr = resample(task, lrn, rsmp("cv", folds = 3))

  pred = do.call(c, rr$predictions())
  expect_null(pred$data$se)
  expect_false("se" %in% pred$predict_types)
  expect_true(allMissing(pred$se))
  expect_false("se" %in% names(as.data.table(pred)))
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
