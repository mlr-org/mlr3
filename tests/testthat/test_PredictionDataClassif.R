test_that("PredictionDataClassif", {
  task = tsk("iris")
  learner = lrn("classif.featureless", predict_type = "prob")
  p = learner$train(task)$predict(task)
  pdata = p$data

  expect_s3_class(pdata, "PredictionDataClassif")
  expect_integer(pdata$row_ids, any.missing = FALSE)
  expect_factor(pdata$truth, levels = task$class_names, any.missing = FALSE)
  expect_factor(pdata$response, levels = task$class_names, any.missing = FALSE)
  expect_matrix(pdata$prob, nrows = task$nrow, ncols = length(task$class_names), any.missing = FALSE)

  expect_s3_class(c(pdata, pdata), "PredictionDataClassif")
  expect_prediction(as_prediction(pdata))
  expect_equal(as.data.table(p), as.data.table(as_prediction(pdata)))

  pdata = filter_prediction_data(pdata, row_ids = 1:3)
  expect_set_equal(pdata$row_ids, 1:3)
  expect_factor(pdata$truth, len = 3)
  expect_factor(pdata$response, len = 3)
  expect_matrix(pdata$prob, nrows = 3)
})

test_that("row sums of prob sums up to 1 ", {
  pdata = new_prediction_data(list(row_ids = 1:2, truth = factor(c("a", "b")),
    response = c("a", "b"), prob = matrix(c(0.5, 0.5, 0.5, 1), 2)), "classif")
  expect_error(check_prediction_data(pdata), "sum up")
})

test_that("construction of empty PredictionDataClassif", {
  task = tsk("iris")

  learner = lrn("classif.featureless")
  learner$train(task)
  pred = learner$predict(task, row_ids = integer())
  expect_prediction(pred)
  expect_set_equal(pred$predict_types, "response")
  expect_integer(pred$row_ids, len = 0L)
  expect_factor(pred$truth, len = 0L, levels = task$class_names)
  expect_null(pred$prob)
  expect_data_table(as.data.table(pred), nrows = 0L, ncols = 3L)

  learner = lrn("classif.featureless", predict_type = "prob")
  learner$train(task)
  pred = learner$predict(task, row_ids = integer())
  expect_prediction(pred)
  expect_set_equal(pred$predict_types, c("response", "prob"))
  expect_integer(pred$row_ids, len = 0L)
  expect_factor(pred$truth, len = 0L, levels = task$class_names)
  expect_matrix(pred$prob, nrows = 0L, ncols = length(task$class_names))
  expect_data_table(as.data.table(pred), nrows = 0L, ncols = 6L)
})

test_that("combine with extra data", {
  pdata = new_prediction_data(list(
    row_ids = 1:2,
    truth = factor(c("a", "b")),
    response = c("a", "b"),
    prob = matrix(c(0.5, 0.5, 0.5, 1), 2),
    extra = list(extra_col = c("a", "b"), extra_col2 = c(1, 2))),
    "classif")

  pdata2 = new_prediction_data(list(
    row_ids = 1:2,
    truth = factor(c("a", "b")),
    response = c("a", "b"),
    prob = matrix(c(0.5, 0.5, 0.5, 1), 2),
    extra = list(extra_col = c("c", "d"), extra_col2 = c(3, 4))),
    "classif")

  expect_equal(c(pdata, pdata2)$extra, list(extra_col = c("a", "b", "c", "d"), extra_col2 = c(1, 2, 3, 4)))
  expect_equal(c(pdata, pdata2, keep_duplicates = FALSE)$extra, list(extra_col = c("c", "d"), extra_col2 = c(3, 4)))

  pdata2 = new_prediction_data(list(
    row_ids = 1:2,
    truth = factor(c("a", "b")),
    response = c("a", "b"),
    prob = matrix(c(0.5, 0.5, 0.5, 1), 2),
    extra = list(extra_col = c("c", "d"), extra_col3 = c(3, 4))),
    "classif")

  expect_equal(c(pdata, pdata2)$extra, list(extra_col = c("a", "b", "c", "d"), extra_col2 = c(1, 2, NA, NA), extra_col3 = c(NA, NA, 3, 4)))

  pdata2 = new_prediction_data(list(
    row_ids = 1:2,
    truth = factor(c("a", "b")),
    response = c("a", "b"),
    prob = matrix(c(0.5, 0.5, 0.5, 1), 2)),
    "classif")

  expect_error(c(pdata, pdata2), "Some predictions have extra data, others do not")
})

test_that("filtering with extra data", {
  pdata = new_prediction_data(list(
    row_ids = 1:2,
    truth = factor(c("a", "b")),
    response = c("a", "b"),
    prob = matrix(c(0.5, 0.5, 0.5, 1), 2),
    extra = list(extra_col = c("a", "b"), extra_col2 = c(1, 2))),
    "classif")

  expect_equal(filter_prediction_data(pdata, row_ids = 1)$extra, list(extra_col = "a", extra_col2 = 1))
  expect_equal(filter_prediction_data(pdata, row_ids = 2)$extra, list(extra_col = "b", extra_col2 = 2))
})
