context("PredictionClassif")

test_that("Construction", {
  task = mlr_tasks$get("iris")
  p = PredictionClassif$new(row_ids = task$row_ids, truth = task$truth(), response = task$truth())
  expect_prediction(p)
  expect_prediction_classif(p)
})

test_that("Internally constructed Prediction", {
  task = mlr_tasks$get("iris")
  lrn = mlr_learners$get("classif.featureless")
  lrn$predict_type = "prob"
  e = Experiment$new(task, lrn)$train()$predict()
  p = e$prediction
  expect_prediction(p)
  expect_prediction_classif(p, task = task)
})

test_that("setting threshold binaryclass", {
  task = mlr_tasks$get("sonar")
  lrn = mlr_learners$get("classif.rpart", predict_type = "prob")
  e = Experiment$new(task, lrn)$train()$predict()
  p = e$prediction
  expect_factor(p$response, levels = task$class_names)
  expect_equal(as.character(p$response), colnames(p$prob)[max.col(p$prob)])

  set_thresh = function(e, th) {
    p = e$prediction$set_threshold(th)
    e$prediction = p
  }

  response_before = p$response
  set_thresh(e, 0.5)
  expect_factor(e$prediction$response, levels = task$class_names, any.missing = FALSE)
  expect_equal(e$prediction$response, response_before)
  expect_lt(e$score()$performance, 0.25)

  set_thresh(e, 0)
  expect_factor(e$prediction$response, levels = task$class_names, any.missing = FALSE)
  expect_true(all(as.character(e$prediction$response) == task$positive | e$prediction$prob[, task$positive] == 0))
  expect_gt(e$score()$performance, 0.25)

  set_thresh(e, 1)
  expect_factor(e$prediction$response, levels = task$class_names, any.missing = FALSE)
  expect_true(all(as.character(e$prediction$response) == task$negative | e$prediction$prob[, task$negative] == 0))
  expect_gt(e$score()$performance, 0.25)

})

test_that("setting threshold multiclass", {
  task = mlr_tasks$get("zoo")
  lrn = mlr_learners$get("classif.rpart", predict_type = "prob")
  e = Experiment$new(task, lrn)$train()$predict()

  # a small fix for our tests ... Add a small number to all probabilities so that
  # we can knock off single labels
  e$data$predicted$prob = t(apply(e$data$predicted$prob, 1, function(x) {
    x = x + 0.01
    x / sum(x)
  }))

  p = e$prediction
  expect_factor(p$response, levels = task$class_names)
  expect_equal(as.character(p$response), colnames(p$prob)[max.col(p$prob)])

  prob_before = p$prob
  response_before = p$response

  expect_error({
    p$set_threshold(c(0.5, 0.5))
  }, "length") # check for correct length(threshold) = nclass

  x = p$set_threshold(set_names(c(1, 1, 1, 1, 1, 1, 1), task$class_names))
  expect_factor(x$response, levels = task$class_names, any.missing = FALSE)
  expect_equal(x$response, response_before)

  threshold = set_names(c(0, 1, 1, 1, 1, 1, 1), task$class_names)
  x = p$set_threshold(threshold)
  expect_factor(x$response, levels = task$class_names, any.missing = FALSE)
  expect_equal(as.character(unique(x$response)), task$class_names[1L])
})

test_that("confusion", {
  task = mlr_tasks$get("iris")
  lrn = mlr_learners$get("classif.featureless")
  lrn$predict_type = "prob"
  e = Experiment$new(task, lrn)$train()$predict()
  p = e$prediction
  cm = p$confusion

  expect_matrix(cm, nrow = 3, ncol = 3, any.missing = FALSE)
  expect_equal(colnames(p$confusion), task$class_names)
  expect_equal(rownames(p$confusion), task$class_names)
  expect_equal(names(dimnames(cm)), c("response", "truth"))
})


test_that("rbind", {
  task = mlr_tasks$get("iris")
  lrn = mlr_learners$get("classif.featureless")
  lrn$predict_type = "prob"
  rr = resample(task, lrn, "cv3")

  pred = do.call(rbind, map(rr$experiments(), "prediction"))
  expect_prediction(pred)
  expect_prediction_classif(pred)

  dt = as.data.table(pred)
  expect_data_table(dt, nrow = task$nrow, ncol = 6L, any.missing = FALSE)

  conf = pred$confusion
  expect_equal(sum(conf), 150L)
  expect_equal(rownames(conf), task$class_names)
  expect_equal(colnames(conf), task$class_names)
  expect_equal(conf, Reduce("+", map(rr$experiments(), function(x) x$prediction$confusion)))
})
