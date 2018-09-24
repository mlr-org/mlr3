context("benchmark")

test_that("Basic benchmarking", {
  tasks = mlr_tasks$mget(c("iris", "sonar"))
  learners = mlr_learners$mget(c("classif.dummy", "classif.rpart"))
  resamplings = mlr_resamplings$mget("cv")
  resamplings$cv$par_vals = list(folds =  3)
  bmr = benchmark(tasks, learners, resamplings)

  expect_is(bmr, "BenchmarkResult")
  expect_names(names(bmr$data), permutation.of = c(reflections$experiment_slots$name, "hash"))

  hashes = bmr$hashes$hash
  rr = bmr$resampling(hashes[1])
  expect_resample_result(rr)
  expect_experiment(rr$experiment(1))

  expect_data_table(bmr$performance, nrow = 12L)
  expect_names(names(bmr$performance), permutation.of = c("task", "learner", ids(tasks[[1L]]$measures)))
})

test_that("Different performance measures", {
  tasks = mlr_tasks$mget(c("iris", "sonar"))
  tasks$iris$measures = mlr_measures$mget("acc")
  learners = mlr_learners$mget(c("classif.dummy", "classif.rpart"))
  resamplings = mlr_resamplings$mget("cv")
  resamplings$cv$par_vals = list(folds = 3)

  bmr = benchmark(tasks, learners, resamplings)
  perf = bmr$performance
  expect_data_table(perf, nrow = 12L)
  expect_names(names(perf), must.include = c("mmce", "acc"))
  expect_equal(perf[task == "sonar", sum(is.na(mmce))], 0)
  expect_equal(perf[task == "iris", sum(is.na(mmce))], 6)
  expect_equal(perf[task == "sonar", sum(is.na(acc))], 6)
  expect_equal(perf[task == "iris", sum(is.na(acc))], 0)
})
