context("mlr_measures_confusion")

test_that("precision +  recall", {
  task = mlr_tasks$get("pima")
  lrn = mlr_learners$get("classif.featureless", param_vals = list(method = "sample"))
  measures = mlr_measures$mget(c("classif.precision", "classif.recall"))

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
