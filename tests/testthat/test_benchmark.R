context("benchmark")

tasks = mlr_tasks$mget(c("iris", "sonar"))
tasks$iris$measures = mlr_measures$mget("acc")
learners = mlr_learners$mget(c("classif.dummy", "classif.rpart"))
resamplings = mlr_resamplings$mget("cv")
resamplings$cv$par_vals = list(folds =  3)
bmr = benchmark(tasks, learners, resamplings)

test_that("Basic benchmarking", {
  expect_is(bmr, "BenchmarkResult")
  expect_names(names(bmr$data), permutation.of = c(reflections$experiment_slots$name, "hash"))
  expect_data_table(bmr$performance, nrow = 12L)
  expect_names(names(bmr$performance), must.include = c("task", "learner", "resampling", ids(tasks[[1L]]$measures), ids(tasks[[2]]$measures)))

  perf = bmr$performance
  expect_data_table(perf, nrow = 12L)
  expect_names(names(perf), must.include = c("mmce", "acc"))
  expect_numeric(perf$mmce, lower = 0, upper = 1, any.missing = TRUE)
  expect_numeric(perf$acc, lower = 0, upper = 1, any.missing = TRUE)
  expect_equal(perf[task == "sonar", sum(is.na(mmce))], 0)
  expect_equal(perf[task == "iris", sum(is.na(mmce))], 6)
  expect_equal(perf[task == "sonar", sum(is.na(acc))], 6)
  expect_equal(perf[task == "iris", sum(is.na(acc))], 0)
})

test_that("ResampleResult getter", {
  hashes = bmr$hashes$hash
  expect_character(hashes, len = 4L, any.missing = FALSE, unique = TRUE)
  rr = bmr$resampling(hashes[1])
  expect_resample_result(rr)
  expect_experiment(rr$experiment(1))
})
