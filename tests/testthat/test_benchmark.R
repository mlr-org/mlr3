context("benchmark")

tasks = mlr_tasks$mget(c("iris", "sonar"))
tasks$iris$measures = mlr_measures$mget("classif.acc")
learners = mlr_learners$mget(c("classif.featureless", "classif.rpart"))
resamplings = mlr_resamplings$mget("cv")
resamplings$cv$param_set$values = list(folds = 3)
design = expand_grid(tasks, learners, resamplings)
bmr = benchmark(design)

test_that("Basic benchmarking", {
  expect_benchmark_result(bmr)
  expect_names(names(bmr$data), permutation.of = c(mlr_reflections$experiment_slots$name, "hash"))

  tab = as.data.table(bmr)
  expect_data_table(tab, nrow = 12L)
  expect_names(names(tab), must.include = c("task_id", "learner_id", "resampling_id", ids(tasks[[1L]]$measures), ids(tasks[[2]]$measures)))
  expect_numeric(tab$classif.ce, lower = 0, upper = 1, any.missing = TRUE)
  expect_numeric(tab$classif.acc, lower = 0, upper = 1, any.missing = TRUE)
  expect_equal(tab[task_id == "sonar", sum(is.na(classif.ce))], 0)
  expect_equal(tab[task_id == "iris", sum(is.na(classif.ce))], 6)
  expect_equal(tab[task_id == "sonar", sum(is.na(classif.acc))], 6)
  expect_equal(tab[task_id == "iris", sum(is.na(classif.acc))], 0)

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
  expect_names(names(tab), permutation.of = c("measure_hash", "measure_id", "measure"))
  expect_character(tab$measure_id, len = 2L, unique = TRUE, any.missing = FALSE)

  tab = bmr$aggregated(objects = FALSE)
  expect_data_table(tab, nrow = 4L)
  expect_names(names(tab), type = "unique", permutation.of = c("hash", "task_id", "learner_id", "resampling_id", "classif.ce", "classif.acc"))
  expect_numeric(tab[task_id == "sonar", classif.ce], any.missing = FALSE)
  expect_numeric(tab[task_id == "iris", classif.acc], any.missing = FALSE)
})

test_that("ResampleResult getter", {
  hashes = bmr$resample_results$hash
  expect_character(hashes, len = 4L, any.missing = FALSE, unique = TRUE)
  rr = bmr$resample_result(hashes[1])
  expect_resample_result(rr)
  expect_experiment(rr$experiment(1))
})


test_that("discarding model", {
  bmr = benchmark(expand_grid(tasks[1L], learners[1L], resamplings), ctrl = mlr_control(store_prediction = FALSE, store_model = FALSE))
  expect_true(every(bmr$data$predicted, is.null))
  expect_true(every(bmr$data$model, is.null))
})

test_that("bmr$combine()", {
  bmr_new = benchmark(expand_grid(mlr_tasks$mget("pima"), learners, resamplings))
  bmr_combined = bmr$clone(deep = TRUE)$combine(bmr_new)

  expect_benchmark_result(bmr)
  expect_benchmark_result(bmr_new)
  expect_benchmark_result(bmr_combined)

  expect_data_table(bmr$data, nrow = 12)
  expect_data_table(bmr_new$data, nrow = 6)
  expect_data_table(bmr_combined$data, nrow = 18)

  expect_false("pima" %in% bmr$tasks$task_id)
  expect_true("pima" %in% bmr_new$tasks$task_id)
  expect_true("pima" %in% bmr_combined$tasks$task_id)
})


test_that("bmr$get_best()", {
  measure = tasks$iris$measures[[1L]]
  best = bmr$get_best(measure$id)
  expect_resample_result(best)
})

test_that("inputs are cloned", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.featureless")
  resampling = mlr_resamplings$get("holdout")

  bmr = benchmark(data.table(task = list(task), learner = list(learner), resampling = list(resampling)))
  e = bmr$resample_result(bmr$resample_results$hash)$experiment(1)

  expect_different_address(task, e$task)
  expect_different_address(learner, e$learner)
  expect_different_address(resampling, e$data$resampling)
})

test_that("memory footprint", {
  expect_equal(uniqueN(map_chr(design$task, address)), 2L)
  expect_equal(uniqueN(map_chr(design$learner, address)), 2L)
  expect_equal(uniqueN(map_chr(design$resampling, address)), 2L)

  x = bmr$data
  # FIXME: cloning!
  # expect_equal(uniqueN(map_chr(x$learner, address)), 2L)
  expect_equal(uniqueN(map_chr(x$task, address)), 2L)
  expect_equal(uniqueN(map_chr(x$resampling, address)), 2L)
  expect_equal(uniqueN(map_chr(x$measures, function(x) address(x[[1]]))), 2L)
})

test_that("resample with replacement measures", {
  tasks = mlr_tasks$mget(c("iris", "sonar"))
  learner = mlr_learners$get("classif.featureless")
  bmr = benchmark(design = expand_grid(tasks, learner, "cv3"), measures = mlr_measures$mget(c("classif.ce", "classif.acc")))
  expect_equal(bmr$measures$measure_id, c("classif.ce", "classif.acc"))
  expect_subset(c("classif.ce", "classif.acc"), names(bmr$aggregated()))
})
