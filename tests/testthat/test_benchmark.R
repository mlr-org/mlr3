context("benchmark")

tasks = mlr_tasks$mget(c("iris", "sonar"))
tasks$iris$measures = mlr_measures$mget("acc")
learners = mlr_learners$mget(c("classif.dummy", "classif.rpart"))
resamplings = mlr_resamplings$mget("cv")
resamplings$cv$par_vals = list(folds =  3)
bmr = benchmark(tasks, learners, resamplings)

test_that("Basic benchmarking", {
  expect_is(bmr, "BenchmarkResult")
  expect_names(names(bmr$data), permutation.of = c(mlr_reflections$experiment_slots$name, "hash"))
  expect_data_table(bmr$performance, nrow = 12L)
  expect_names(names(bmr$performance), must.include = c("task_id", "learner_id", "resampling_id", ids(tasks[[1L]]$measures), ids(tasks[[2]]$measures)))

  perf = bmr$performance
  expect_data_table(perf, nrow = 12L)
  expect_names(names(perf), must.include = c("mmce", "acc"))
  expect_numeric(perf$mmce, lower = 0, upper = 1, any.missing = TRUE)
  expect_numeric(perf$acc, lower = 0, upper = 1, any.missing = TRUE)
  expect_equal(perf[task_id == "sonar", sum(is.na(mmce))], 0)
  expect_equal(perf[task_id == "iris", sum(is.na(mmce))], 6)
  expect_equal(perf[task_id == "sonar", sum(is.na(acc))], 6)
  expect_equal(perf[task_id == "iris", sum(is.na(acc))], 0)

  tab = bmr$tasks
  expect_data_table(tab, nrow = 2, any.missing = FALSE)
  expect_names(names(tab), permutation.of = c("task_hash", "task", "task_id"))
  expect_hash(tab$task_hash, len = 2L)

  tab = bmr$learners
  expect_data_table(tab, nrow = 2, any.missing = FALSE)
  expect_names(names(tab), permutation.of = c("learner_hash", "learner", "learner_id"))
  expect_hash(tab$learner_hash, len = 2L)

  tab = bmr$resamplings
  expect_data_table(tab, nrow = 2, any.missing = FALSE)
  expect_names(names(tab), permutation.of = c("resampling_hash", "resampling", "resampling_id"))
  expect_hash(tab$resampling_hash, len = 2L)

  tab = bmr$measures
  expect_data_table(tab, nrow = 2, any.missing = FALSE)
  expect_names(names(tab), permutation.of = c("measure_id", "measure"))
  expect_character(tab$measure_id, len = 2L, unique = TRUE, any.missing = FALSE)
})

test_that("ResampleResult getter", {
  hashes = bmr$resample_results$hash
  expect_character(hashes, len = 4L, any.missing = FALSE, unique = TRUE)
  rr = bmr$resample_result(hashes[1])
  expect_resample_result(rr)
  expect_experiment(rr$experiment(1))
})


test_that("discarding model", {
  bmr = benchmark(tasks[1L], learners[1L], resamplings, ctrl = exec_control(store_prediction = FALSE, store_model = FALSE))
  expect_true(all(map_lgl(bmr$data$prediction, is.null)))
  expect_true(all(map_lgl(bmr$data$model, is.null)))
})
