test_that("Construction", {
  task = tsk("iris")
  p = PredictionClassif$new(row_ids = task$row_ids, truth = task$truth(), response = task$truth())
  expect_prediction(p)
  expect_prediction_classif(p)
})

test_that("Internally constructed Prediction", {
  task = tsk("iris")
  lrn = lrn("classif.featureless")
  lrn$predict_type = "prob"
  p = lrn$train(task)$predict(task)
  expect_prediction(p)
  expect_prediction_classif(p, task = task)

  p = PredictionClassif$new(row_ids = task$row_ids, truth = task$truth(), prob = p$prob)
  expect_set_equal(p$predict_types, c("response", "prob"))
})

test_that("setting threshold binaryclass", {
  task = tsk("sonar")
  lrn = lrn("classif.rpart", predict_type = "prob")
  p = lrn$train(task)$predict(task)
  expect_factor(p$response, levels = task$class_names)
  expect_equal(as.character(p$response), colnames(p$prob)[max.col(p$prob)])

  response_before = p$response
  expect_false(withVisible(p$set_threshold(0.5))$visible) # 356
  expect_factor(p$response, levels = task$class_names, any.missing = FALSE)
  expect_equal(p$response, response_before)
  expect_lt(p$score(msr("classif.ce")), 0.25)

  p$set_threshold(0)
  expect_factor(p$response, levels = task$class_names, any.missing = FALSE)
  expect_true(all(as.character(p$response) == task$positive | p$prob[, task$positive] == 0))
  expect_gt(p$score(msr("classif.ce")), 0.25)

  p$set_threshold(1)
  expect_factor(p$response, levels = task$class_names, any.missing = FALSE)
  expect_true(all(as.character(p$response) == task$negative | p$prob[, task$negative] == 0))
  expect_gt(p$score(msr("classif.ce")), 0.25)
})

test_that("setting threshold multiclass", {
  task = tsk("zoo")
  lrn = lrn("classif.rpart", predict_type = "prob")
  p = lrn$train(task)$predict(task)

  # a small fix for our tests ... Add a small number to all probabilities so that
  # we can knock off single labels
  p$data$prob = t(apply(p$data$prob, 1, function(x) {
    x = x + 0.01
    x / sum(x)
  }))

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

test_that("setting threshold edge cases (#452)", {
  learner = lrn("classif.rpart", predict_type = "prob")
  t = tsk("iris")
  prd = learner$train(t)$predict(t$clone()$filter(c(1, 51, 101)))

  prd$set_threshold(c(setosa = 0, versicolor = 0, virginica = 0), ties_method = "first")
  expect_equal(as.character(prd$response), c("setosa", "versicolor", "versicolor"))
  prd$set_threshold(c(setosa = 0, versicolor = 0, virginica = 0), ties_method = "last")
  expect_equal(as.character(prd$response), c("setosa", "virginica", "virginica"))

  prd$set_threshold(c(setosa = 1, versicolor = 0, virginica = 0), ties_method = "first")
  expect_equal(as.character(prd$response), c("setosa", "versicolor", "versicolor"))
  prd$set_threshold(c(setosa = 1, versicolor = 0, virginica = 0), ties_method = "last")
  expect_equal(as.character(prd$response), c("setosa", "virginica", "virginica"))

  prd$set_threshold(c(setosa = 0, versicolor = 1, virginica = 0), ties_method = "first")
  expect_equal(as.character(prd$response), c("setosa", "virginica", "virginica"))
  prd$set_threshold(c(setosa = 0, versicolor = 1, virginica = 0), ties_method = "last")
  expect_equal(as.character(prd$response), c("setosa", "virginica", "virginica"))

  prd$set_threshold(c(setosa = 0, versicolor = 0, virginica = 1), ties_method = "first")
  expect_equal(as.character(prd$response), c("setosa", "versicolor", "versicolor"))
  prd$set_threshold(c(setosa = 0, versicolor = 0, virginica = 1), ties_method = "last")
  expect_equal(as.character(prd$response), c("setosa", "versicolor", "versicolor"))
})

test_that("confusion", {
  task = tsk("iris")
  lrn = lrn("classif.featureless")
  lrn$predict_type = "prob"
  p = lrn$train(task)$predict(task)
  cm = p$confusion

  expect_matrix(cm, nrows = 3, ncols = 3, any.missing = FALSE)
  expect_equal(colnames(p$confusion), task$class_names)
  expect_equal(rownames(p$confusion), task$class_names)
  expect_equal(names(dimnames(cm)), c("response", "truth"))
})

test_that("c", {
  task = tsk("iris")
  lrn = lrn("classif.featureless")
  lrn$predict_type = "prob"
  rr = resample(task, lrn, rsmp("cv", folds = 3))

  pred = do.call(c, rr$predictions())
  expect_prediction_classif(pred)

  dt = as.data.table(pred)
  expect_data_table(dt, nrows = task$nrow, ncols = 6L, any.missing = FALSE)

  conf = pred$confusion
  expect_equal(sum(conf), 150L)
  expect_equal(rownames(conf), task$class_names)
  expect_equal(colnames(conf), task$class_names)
  expect_equal(conf, Reduce("+", map(rr$predictions(), "confusion")))

  # duplicates are detected?
  p1 = get_private(rr)$.data$data$fact$prediction[[1]]$test
  p2 = get_private(rr)$.data$data$fact$prediction[[1]]$test
  p3 = c(p1, p2, keep_duplicates = FALSE)
  expect_equal(sort(p1$data$row_ids), sort(p2$data$row_ids))
  expect_equal(sort(p1$data$row_ids), sort(p3$data$row_ids))
  expect_factor(p3$response, len = length(p1$response), any.missing = FALSE)
  expect_matrix(p3$prob, nrows = nrow(p1$prob), ncols = ncol(p1$prob))
})

test_that("as_prediction_classif", {
  task = tsk("penguins")
  learner = lrn("classif.featureless", method = "weighted.sample")
  p = learner$train(task)$predict(task)

  tab = as.data.table(p)
  p2 = as_prediction_classif(tab)

  expect_equal(tab, as.data.table(p2))
})

test_that("#615", {
  task = tsk("iris")
  training <- task$clone()$filter(1:100)
  testing <- task$clone()$filter(101:150)

  l = lrn("classif.rpart")
  l$train(training)
  expect_equal(unname(l$predict(testing)$score()), 1)
})

test_that("filtering", {
  task = tsk("iris")
  p = PredictionClassif$new(row_ids = task$row_ids, truth = task$truth(), response = task$truth())

  p2 = p$clone()$filter(1:3)
  expect_set_equal(p$row_ids, 1:150)
  expect_set_equal(p2$row_ids, 1:3)
  expect_prediction(as_prediction_classif(as.data.table(p2)))
})
