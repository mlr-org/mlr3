task = tsk("penguins")
learner = lrn("classif.rpart")
rr = resample(task, learner, rsmp("cv", folds = 3), store_models = TRUE)

test_that("similarity measures", {
  measures = mlr_measures$keys("^sim\\.")

  for (m in msrs(measures)) {
    expect_number(m$aggregate(rr))
    expect_true(is.na(m$score(rr$prediction(), learner = rr$learners[[1]])))
    expect_numeric(rr$aggregate(msrs(c("classif.acc", m$id))), len = 2, any.missing = FALSE)
    expect_true(allMissing(rr$score(m)[[m$id]]))
  }
})

test_that("similarity example", {
  task = tsk("penguins")
  learners = list(
    lrn("classif.rpart", maxdepth = 1, id = "r1"),
    lrn("classif.rpart", maxdepth = 2, id = "r2")
  )
  resampling = rsmp("cv", folds = 3)
  grid = benchmark_grid(task, learners, resampling)
  bmr = benchmark(grid, store_models = TRUE)
  x = bmr$aggregate(msr("sim.jaccard"))
  expect_numeric(x$sim.jaccard, any.missing = FALSE)
})
