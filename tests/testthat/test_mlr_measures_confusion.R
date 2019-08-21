context("mlr_measures_confusion")

test_that("precision +  recall", {
  task = tsk("pima")
  lrn = lrn("classif.featureless", method = "sample")
  measures = list(msr("classif.precision"), msr("classif.recall"))

  p = lrn$train(task)$predict(task)
  perf = p$score(measures)

  expect_equal(
    Metrics::precision(as.integer(p$truth == task$positive), as.integer(p$response == task$positive)),
    perf[["classif.precision"]]
  )

  expect_equal(
    Metrics::recall(as.integer(p$truth == task$positive), as.integer(p$response == task$positive)),
    perf[["classif.recall"]]
  )
})
