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
  expect_true(all(as.character(p$response) == task$positive))
  expect_gt(e$score()$performance, 0.25)

  p$threshold = 1
  expect_factor(p$response, levels = task$class_names, any.missing = FALSE)
  expect_true(all(as.character(p$response) == task$negative|| p$prob[, task$negative] == 0))
  expect_gt(e$score()$performance, 0.25)

  # multiclass classification
  task = mlr_tasks$get("iris")
  lrn = mlr_learners$get("classif.rpart", predict_type = "prob")
  e = Experiment$new(task, lrn)$train()$predict()
  p = e$prediction
  expect_factor(p$response, levels = task$class_names)
  expect_equal(as.character(p$response), colnames(p$prob)[max.col(p$prob)])

  response_before = p$response

  expect_error({p$threshold = c(0.5, 0.5)}) # check for correct length(threshold) = nclass
  # expect_error({p$threshold = c(0.1, 0.5, 0.5)}) # FIXME: sum of elements is allowed to unequal 1

  p$threshold = c(0, 0.5, 0.5)
  expect_factor(p$response, levels = task$class_names, any.missing = FALSE)
  expect_equal(levels(droplevels(p$response)), task$class_names[1])
  expect_gt(e$score()$performance, .66)

  p$threshold = c(0.5, 0, 0.5)
  expect_factor(p$response, levels = task$class_names, any.missing = FALSE)
  expect_equal(levels(droplevels(p$response)), task$class_names[2])
  expect_gt(e$score()$performance, 0.66)

  p$threshold = c(0.5, 0.5, 0)
  expect_factor(p$response, levels = task$class_names, any.missing = FALSE)
  expect_equal(levels(droplevels(p$response)), task$class_names[3])
  expect_gt(e$score()$performance, 0.66)

})
