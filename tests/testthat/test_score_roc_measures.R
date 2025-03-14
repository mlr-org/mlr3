test_that("score_roc_measure works", {
  learner = lrn("classif.rpart", predict_type = "prob")
  splits = partition(task = tsk("pima"), ratio = 0.7)
  task = tsk("pima")
  learner$train(task)
  pred = learner$predict(task)
  res = score_roc_measures(pred)

  expect_list(res, len = 2)
  expect_named(res, c("confusion_matrix", "measures"))
  expect_named(res$measures, c("tpr", "fpr", "fnr", "tnr", "ppv", "fdr", "npv", "fomr", "acc", "lr_plus", "lr_minus", "dor"))
})
