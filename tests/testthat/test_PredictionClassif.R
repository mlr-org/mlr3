context("PredictionClassif")

test_that("Construction", {
  task = mlr_tasks$get("iris")
  p = PredictionClassif$new(task = task, response = task$truth())
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

test_that("setting threshold", {
  # binary classification
  task = mlr_tasks$get("sonar")
  lrn = mlr_learners$get("classif.rpart", predict_type = "prob")
  e = Experiment$new(task, lrn)$train()$predict()
  p = e$prediction
  expect_factor(p$response, levels = task$class_names)
  expect_equal(as.character(p$response), colnames(p$prob)[max.col(p$prob)])

  response_before = p$response
  p$threshold = 0.5
  expect_factor(p$response, levels = task$class_names, any.missing = FALSE)
  expect_equal(p$response, response_before)
  expect_lt(e$score()$performance, 0.25)

  p$threshold = 0
  expect_factor(p$response, levels = task$class_names, any.missing = FALSE)
  expect_true(all(as.character(p$response) == task$positive | p$prob[, task$positive] == 0))
  expect_gt(e$score()$performance, 0.25)

  p$threshold = 1
  expect_factor(p$response, levels = task$class_names, any.missing = FALSE)
  expect_true(all(as.character(p$response) == task$negative | p$prob[, task$negative] == 0))
  expect_gt(e$score()$performance, 0.25)

  # multiclass classification
  task = mlr_tasks$get("zoo")
  lrn = mlr_learners$get("classif.rpart", predict_type = "prob")
  e = Experiment$new(task, lrn)$train()$predict()
  p = e$prediction
  expect_factor(p$response, levels = task$class_names)
  expect_equal(as.character(p$response), colnames(p$prob)[max.col(p$prob)])

  prob_before = p$prob
  response_before = p$response

  # a small fix for our tests ... Add a small number to all probabilities so that
  # we can knock off single labels
  p$prob = t(apply(p$prob, 1, function(x) {
    x = x + 0.01
    x / sum(x)
  }))
  expect_error({
    p$threshold = c(0.5, 0.5)
  }, "have length") # check for correct length(threshold) = nclass

  p$threshold = set_names(c(1, 1, 1, 1, 1, 1, 1), task$class_names)
  expect_factor(p$response, levels = task$class_names, any.missing = FALSE)
  expect_equal(p$response, response_before)

  p$threshold = set_names(c(0, 1, 1, 1, 1, 1, 1), task$class_names)
  expect_factor(p$response, levels = task$class_names, any.missing = FALSE)
  expect_equal(as.character(unique(p$response)), task$class_names[1L])
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
