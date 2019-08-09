context("benchmark")

tasks = mlr_tasks$mget(c("iris", "sonar"))
learners = mlr_learners$mget(c("classif.featureless", "classif.rpart"))
resamplings = mlr_resamplings$mget("cv3")
design = expand_grid(tasks, learners, resamplings)
bmr = benchmark(design)

test_that("Basic benchmarking", {
  expect_benchmark_result(bmr)
  expect_names(names(bmr$data), permutation.of = c(mlr_reflections$rr_names, "hash"))

  tab = as.data.table(bmr)
  expect_data_table(tab, nrows = 12L, ncols = 6L)
  expect_names(names(tab), permutation.of = c("hash", "task", "learner", "resampling", "iteration", "prediction"))

  tab = bmr$performance(ids = FALSE)
  expect_data_table(tab, nrows = 12L, ncols = 7L)
  expect_names(names(tab), permutation.of = c("nr", "task", "learner", "resampling", "iteration", "prediction", "classif.ce"))

  tab = bmr$tasks
  expect_data_table(tab, nrows = 2, any.missing = FALSE)
  expect_names(names(tab), permutation.of = c("task_hash", "task", "task_id"))
  expect_hash(tab$task_hash, len = 2L)

  tab = bmr$learners
  expect_data_table(tab, nrows = 2, any.missing = FALSE)
  expect_names(names(tab), permutation.of = c("learner_hash", "learner", "learner_id"))
  expect_hash(tab$learner_hash, len = 2L)

  tab = bmr$resamplings
  expect_data_table(tab, nrows = 2, any.missing = FALSE)
  expect_names(names(tab), permutation.of = c("resampling_hash", "resampling", "resampling_id"))
  expect_hash(tab$resampling_hash, len = 2L)

  tab = bmr$aggregate()
  expect_data_table(tab, nrows = 4L)
  expect_names(names(tab), type = "unique", permutation.of = c("nr", "resample_result", "task_id", "learner_id", "resampling_id", "classif.ce"))
})

test_that("ResampleResult / hash", {
  aggr = bmr$aggregate()
  nr = aggr$nr
  expect_integer(nr, len = 4L, any.missing = FALSE, unique = TRUE)

  for (i in nr) {
    rr = aggr$resample_result[[i]]
    expect_resample_result(rr)
    expect_equivalent(rr$aggregate(), aggr[["classif.ce"]][i])
    expect_equal(bmr$hashes[i], rr$hash)
  }
})


test_that("discarding model", {
  bmr = benchmark(expand_grid(tasks[1L], learners[1L], resamplings), ctrl = mlr_control(store_models = TRUE))
  expect_false(every(map(bmr$data$learner, "model"), is.null))
  bmr = benchmark(expand_grid(tasks[1L], learners[1L], resamplings), ctrl = mlr_control(store_models = FALSE))
  expect_true(every(map(bmr$data$learner, "model"), is.null))
})

test_that("bmr$combine()", {
  bmr_new = benchmark(expand_grid(mlr_tasks$mget("pima"), learners, resamplings))
  bmr_combined = bmr$clone(deep = TRUE)$combine(bmr_new)


  # new bmr gets pasted at the end of data so hashes do net get mixed up?
  pos_old = match(bmr$hashes, bmr_combined$hashes)
  pos_new = match(bmr_new$hashes, bmr_combined$hashes)
  expect_true(all(pos_old < min(pos_new)))

  expect_benchmark_result(bmr)
  expect_benchmark_result(bmr_new)
  expect_benchmark_result(bmr_combined)

  expect_data_table(bmr$data, nrows = 12)
  expect_data_table(bmr_new$data, nrows = 6)
  expect_data_table(bmr_combined$data, nrows = 18)

  expect_false("pima" %in% bmr$tasks$task_id)
  expect_true("pima" %in% bmr_new$tasks$task_id)
  expect_true("pima" %in% bmr_combined$tasks$task_id)
})

test_that("bmr$resample_result()", {
  expect_resample_result(bmr$resample_result(1L))
})

test_that("inputs are cloned", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.featureless")
  resampling = mlr_resamplings$get("holdout")

  expect_error(benchmark(data.table(task = list(task), learner = list(learner), resampling = list(resampling))), "instantiated")
  bmr = benchmark(design = data.table(task = list(task), learner = list(learner), resampling = list(resampling$instantiate(task))))
  rr = bmr$aggregate()$resample_result[[1L]]

  expect_different_address(task, rr$task)
  expect_different_address(learner, rr$data$learner[[1L]])
  expect_different_address(resampling, rr$resampling)
})

test_that("memory footprint", {
  expect_equal(uniqueN(map_chr(design$task, address)), 2L)
  expect_equal(uniqueN(map_chr(design$learner, address)), 2L)
  expect_equal(uniqueN(map_chr(design$resampling, address)), 2L)

  x = bmr$data
  expect_equal(uniqueN(map_chr(x$task, address)), 2L)
  expect_equal(uniqueN(map_chr(x$resampling, address)), 2L)
})

test_that("multiple measures", {
  tasks = mlr_tasks$mget(c("iris", "sonar"))
  learner = mlr_learners$get("classif.featureless")
  measures = mlr_measures$mget(c("classif.ce", "classif.acc"))
  bmr = benchmark(design = expand_grid(tasks, learner, "cv3"))
  expect_subset(c("classif.ce", "classif.acc"), names(bmr$aggregate(measures)))
})

test_that("predict_type is checked", {
  task = tsk("sonar")
  learner = lrn("classif.featureless")
  resampling = rsp("cv", folds = 3L)
  design = expand_grid(task, learner, resampling)
  bmr = benchmark(design)
  expect_error(bmr$aggregate("classif.auc", "predict_type"))
})

test_that("custom resampling (#245)", {
  task_boston = mlr_tasks$get("boston_housing")
  task_boston$set_col_role(c("chas", "town"), new_roles = "label")
  lrn = mlr_learners$get("regr.featureless")

  rdesc = mlr_resamplings$get("custom")
  train_sets = list((1:200), (1:300), (1:400))
  test_sets = list((201:301), (301:401), (401:501))
  rdesc$instantiate(task_boston, train_sets, test_sets)

  expect_resample_result(mlr3::resample(task_boston, lrn, rdesc))

  design = data.table(task = list(task_boston), learner = list(lrn), resampling = list(rdesc))
  bmr = benchmark(design)
  expect_benchmark_result(bmr)
})

test_that("extract params", {
  # some params, some not
  lrns = mlr_learners$mget(c("classif.rpart", "classif.rpart", "classif.rpart"))
  lrns[[1]]$param_set$values = list()
  lrns[[2]]$param_set$values = list(xval = 0, cp = 0.1)
  bmr = benchmark(expand_grid("wine", lrns, "cv3"))
  aggr = bmr$aggregate(params = TRUE)
  expect_list(aggr$params[[1]], names = "unique", len = 0L)
  expect_list(aggr$params[[2]], names = "unique", len = 2L)
  expect_list(aggr$params[[3]], names = "unique", len = 1L)


  # only one params
  lrns = mlr_learners$mget(c("classif.featureless"))
  bmr = benchmark(expand_grid("wine", lrns, "cv3"))
  aggr = bmr$aggregate(params = TRUE)
  expect_list(aggr$params[[1]], names = "unique", len = 1L)

  # no params
  lrns = mlr_learners$mget(c("classif.debug"))
  bmr = benchmark(expand_grid("wine", lrns, "cv3"))
  aggr = bmr$aggregate(params = TRUE)
  expect_list(aggr$params[[1]], names = "unique", len = 0L)
})
