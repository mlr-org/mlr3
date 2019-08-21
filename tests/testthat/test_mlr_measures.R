context("mlr_measures")

test_that("mlr_measures", {
  expect_dictionary(mlr_measures, min_items = 1L)
  keys = mlr_measures$keys()

  for (key in keys) {
    m = mlr_measures$get(key)
    expect_measure(m)
  }
})

test_that("as.data.table(mlr_measures)", {
  d = as.data.table(mlr_measures)
  expect_data_table(d)
  expect_character(d$key, unique = TRUE, any.missing = FALSE)
  expect_subset(d$task_type, c(mlr_reflections$task_types$type, NA))
  qexpectr(d$packages, "S")
  expect_subset(d$predict_type, unlist(mlr_reflections$learner_predict_types))
  qexpectr(d$task_properties, "S")
  expect_subset(unlist(d$task_properties), unlist(mlr_reflections$task_properties))
})

test_that("custom aggregation", {
  task = tsk("wine")
  lrn = lrn("classif.featureless")

  m = msr("classif.ce")
  m$id = "max_ce"
  m$aggregator = max
  measures = list(msr("classif.ce"), m)

  rr = resample(task, lrn, rsmp("cv", folds = 3))
  perf = rr$performance(measures)
  aggr = rr$aggregate(measures)
  expect_equal(aggr[["max_ce"]], max(perf$classif.ce))
})
