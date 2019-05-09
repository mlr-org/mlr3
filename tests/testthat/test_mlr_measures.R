context("mlr_measures")

test_that("mlr_measures", {
  keys = mlr_measures$keys()

  for (key in keys) {
    m = mlr_measures$get(key)
    expect_measure(m)
  }
}
)

test_that("custom aggregation", {
  task = mlr_tasks$get("wine")
  lrn = mlr_learners$get("classif.featureless")

  m = mlr_measures$get("classif.ce")
  m$id = "max_ce"
  m$aggregator = max
  task$measures = list(mlr_measures$get("classif.ce"), m)

  rr = resample(task, lrn, mlr_resamplings$get("cv"))
  rr$aggregated
  expect_equal(rr$aggregated[["max_ce"]], max(rr$performance("classif.ce")))
}
)
