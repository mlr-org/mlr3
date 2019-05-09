context("mlr_measures_confusion")

test_that("precision +  recall", {
  task = mlr_tasks$get("pima")
  task$measures = mlr_measures$mget(c("classif.precision", "classif.recall"))
  lrn = mlr_learners$get("classif.featureless", param_vals = list(method = "sample"))

  e = Experiment$new(task, lrn)$train()$predict()$score()
  p = e$prediction
  perf = e$performance


  expect_equal(
    Metrics::precision(as.integer(p$truth == task$positive), as.integer(p$response == task$positive)),
    perf[["classif.precision"]]
  )

  expect_equal(
    Metrics::recall(as.integer(p$truth == task$positive), as.integer(p$response == task$positive)),
    perf[["classif.recall"]]
  )
}
)
