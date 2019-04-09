context("PredictionClassif")

test_that("Construction", {
  p = PredictionClassif$new()
  expect_prediction(p)
})

test_that("partial results", {
  task = mlr_tasks$get("iris")
  lrn = mlr_learners$get("classif.featureless")
  lrn$predict_type = "prob"
  e = Experiment$new(task, lrn)$train()$predict()
  p = e$prediction
  expect_prediction(p)
  expect_prediction_classif(p, task = task)

  p = PredictionClassif$new()
  expect_prediction(p)
  expect_prediction_classif(p)
  p$row_ids = task$row_ids
  expect_error(as.data.table(p))

  p$truth = task$truth()
  p$response = rep(factor("setosa", levels = task$class_names), task$nrow)
  expect_prediction(p)
  expect_prediction_classif(p)
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
  p$prob = t(apply(p$prob, 1, function(x) { x = x + 0.01; x / sum(x) }))
  expect_error({ p$threshold = c(0.5, 0.5) }, "have length") # check for correct length(threshold) = nclass

  p$threshold = c(1, 1, 1, 1, 1, 1, 1)
  expect_factor(p$response, levels = task$class_names, any.missing = FALSE)
  expect_equal(p$response, response_before)

  p$threshold = c(0, 1, 1, 1, 1, 1, 1)
  expect_factor(p$response, levels = task$class_names, any.missing = FALSE)
  expect_false(task$class_names[1L] %in% p$response)

  p$threshold = c(1, 0, 1, 1, 1, 1, 1)
  expect_factor(p$response, levels = task$class_names, any.missing = FALSE)
  expect_false(task$class_names[2L] %in% p$response)
})
