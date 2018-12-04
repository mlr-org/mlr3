context("benchmark")

tasks = mlr_tasks$mget(c("iris", "sonar"))
tasks$iris$measures = mlr_measures$mget("acc")
learners = mlr_learners$mget(c("classif.featureless", "classif.rpart"))
resamplings = mlr_resamplings$mget("cv")
resamplings$cv$param_vals = list(folds =  3)
bmr = benchmark(tasks, learners, resamplings)

test_that("Basic benchmarking", {
  expect_benchmark_result(bmr)
  expect_names(names(bmr$data), permutation.of = c(mlr_reflections$experiment_slots$name, "hash"))

  tab = as.data.table(bmr)
  expect_data_table(tab, nrow = 12L)
  expect_names(names(tab), must.include = c("task_id", "learner_id", "resampling_id", ids(tasks[[1L]]$measures), ids(tasks[[2]]$measures)))
  expect_numeric(tab$mmce, lower = 0, upper = 1, any.missing = TRUE)
  expect_numeric(tab$acc, lower = 0, upper = 1, any.missing = TRUE)
  expect_equal(tab[task_id == "sonar", sum(is.na(mmce))], 0)
  expect_equal(tab[task_id == "iris", sum(is.na(mmce))], 6)
  expect_equal(tab[task_id == "sonar", sum(is.na(acc))], 6)
  expect_equal(tab[task_id == "iris", sum(is.na(acc))], 0)

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

  tab = bmr$aggregated
  expect_data_table(tab, nrow = 4L)
  expect_names(names(tab), type = "unique", permutation.of = c("hash", "resample_result", "task_id", "learner_id", "resampling_id", "mmce", "acc"))
  expect_numeric(tab[task_id == "sonar", mmce], any.missing = FALSE)
  expect_numeric(tab[task_id == "iris", acc], any.missing = FALSE)
})

test_that("ResampleResult getter", {
  hashes = bmr$resample_results$hash
  expect_character(hashes, len = 4L, any.missing = FALSE, unique = TRUE)
  rr = bmr$resample_result(hashes[1])
  expect_resample_result(rr)
  expect_experiment(rr$experiment(1))
})


test_that("discarding model", {
  bmr = benchmark(tasks[1L], learners[1L], resamplings, ctrl = mlr_control(store_prediction = FALSE, store_model = FALSE))
  expect_true(every(bmr$data$prediction, is.null))
  expect_true(every(bmr$data$model, is.null))
})

test_that("bmr$combine()", {
  bmr_new = benchmark(mlr_tasks$mget("pima"), learners, resamplings)
  bmr_combined = bmr$clone(deep = TRUE)$combine(bmr_new)

  expect_benchmark_result(bmr)
  expect_benchmark_result(bmr_new)
  expect_benchmark_result(bmr_combined)

  expect_data_table(bmr$data, nrow = 12)
  expect_data_table(bmr_new$data, nrow = 6)
  expect_data_table(bmr_combined$data, nrow = 18)

  expect_false("pima_indians" %in% bmr$tasks$task_id)
  expect_true("pima_indians" %in% bmr_new$tasks$task_id)
  expect_true("pima_indians" %in% bmr_combined$tasks$task_id)
})


test_that("bmr$get_best()", {
  measure = tasks$iris$measures[[1L]]
  best = bmr$get_best(measure)
  expect_resample_result(best)
})
