test_that("score_roc_measure works", {
  learner = lrn("classif.rpart", predict_type = "prob")
  task = tsk("sonar")
  learner$train(task)
  pred = learner$predict(task)
  res = score_roc_measures(pred)

  expect_list(res, len = 2)
  expect_named(res, c("confusion_matrix", "measures"))
  expect_named(
    res$measures,
    c("tpr", "fpr", "fnr", "tnr", "ppv", "fdr", "npv", "fomr", "acc", "lr_plus", "lr_minus", "dor")
  )
})

test_that("score_roc_measures matches the confusion matrix", {
  task = tsk("german_credit")
  pred = lrn("classif.rpart")$train(task)$predict(task)
  res = score_roc_measures(pred)
  cm = pred$confusion

  expect_null(names(res$measures$fdr))
  expect_equal(res$measures$tpr, cm[1L, 1L] / sum(cm[, 1L]))
  expect_equal(res$measures$fpr, cm[1L, 2L] / sum(cm[, 2L]))
  expect_equal(res$measures$ppv, cm[1L, 1L] / sum(cm[1L, ]))
  expect_equal(res$measures$acc, sum(diag(cm)) / sum(cm))

  out = capture.output(print(res, abbreviations = FALSE))
  expect_match(out[grepl("^ +good ", out)], sprintf("good +%i +%i +tpr", cm["good", "good"], cm["bad", "good"]))
})

test_that("score_roc_measures ignores weights", {
  data = tsk("sonar")$data()
  data$w = ifelse(data$Class == "M", 1, 10)
  task = as_task_classif(data, target = "Class")
  task$set_col_roles("w", "weights_measure")
  pred = lrn("classif.rpart")$train(task)$predict(task)
  res = score_roc_measures(pred)
  cm = pred$confusion
  expect_equal(res$measures$acc, sum(diag(cm)) / sum(cm))
})

test_that("score_roc_measures requires a binary classification prediction", {
  task = tsk("iris")
  pred = lrn("classif.rpart")$train(task)$predict(task)
  expect_error(score_roc_measures(pred), "binary")
  task = tsk("mtcars")
  pred = lrn("regr.rpart")$train(task)$predict(task)
  expect_error(score_roc_measures(pred), "PredictionClassif")
})
