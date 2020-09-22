context("benchmark")

tmp = tsk("iris", id = "iris_small")$select("Sepal.Length")
tasks = c(mlr_tasks$mget(c("iris", "sonar")), list(tmp))
learners = mlr_learners$mget(c("classif.featureless", "classif.rpart"))
resamplings = rsmp("cv", folds = 3)
design = benchmark_grid(tasks, learners, resamplings)
bmr = benchmark(design)

test_that("Basic benchmarking", {
  expect_benchmark_result(bmr)
  expect_names(names(as.data.table(bmr)), permutation.of = c(mlr_reflections$rr_names, "uhash"))

  tab = as.data.table(bmr)
  expect_data_table(tab, nrows = 18L, ncols = 6L)
  expect_names(names(tab), permutation.of = c("uhash", mlr_reflections$rr_names))
  measures = list(msr("classif.acc"))

  tab = bmr$score(measures, ids = FALSE)
  expect_data_table(tab, nrows = 18L, ncols = 6L + length(measures))
  expect_names(names(tab), must.include = c("nr", mlr_reflections$rr_names, ids(measures)))

  tab = bmr$tasks
  expect_data_table(tab, nrows = 3L, any.missing = FALSE)
  expect_names(names(tab), identical.to = c("task_hash", "task_id", "task"))
  expect_hash(tab$task_hash, len = 3L)

  tab = bmr$learners
  expect_data_table(tab, nrows = 2L, any.missing = FALSE)
  expect_names(names(tab), identical.to = c("learner_hash", "learner_id", "learner"))
  expect_hash(tab$learner_hash, len = 2L)
  qexpectr(map(tab$learner, "state"), "0")

  tab = bmr$resamplings
  expect_data_table(tab, nrows = 3L, any.missing = FALSE)
  expect_names(names(tab), permutation.of = c("resampling_hash", "resampling", "resampling_id"))
  expect_hash(tab$resampling_hash, len = 3L)

  tab = bmr$aggregate(measures)
  expect_data_table(tab, nrows = 6L)
  expect_names(names(tab), type = "unique",
    identical.to = c("nr", "resample_result", "task_id", "learner_id", "resampling_id", "iters", ids(measures)))
})

test_that("ResampleResult / hash", {
  m = msr("classif.ce")
  aggr = bmr$aggregate(m, uhashes = TRUE)
  nr = aggr$nr
  expect_integer(nr, len = 6L, any.missing = FALSE, unique = TRUE)

  for (i in nr) {
    rr = aggr$resample_result[[i]]
    expect_resample_result(rr)
    expect_equivalent(rr$aggregate(m), aggr[["classif.ce"]][i])
    expect_equal(bmr$uhashes[i], rr$uhash)
  }
})


test_that("discarding model", {
  bmr2 = benchmark(benchmark_grid(tasks[1L], learners[1L], resamplings), store_models = FALSE)
  expect_benchmark_result(bmr2)
  expect_true(every(map(as.data.table(bmr2)$learner, "model"), is.null))

  bmr2 = benchmark(benchmark_grid(tasks[1L], learners[1L], resamplings), store_models = TRUE)
  expect_benchmark_result(bmr2)
  expect_false(every(map(as.data.table(bmr2)$learner, "model"), is.null))
})

test_that("bmr$combine()", {
  bmr_new = benchmark(benchmark_grid(mlr_tasks$mget("pima"), learners, resamplings))

  combined = list(
    bmr$clone(deep = TRUE)$combine(bmr_new),
    c(bmr, bmr_new)
  )

  for (bmr_combined in combined) {
    expect_benchmark_result(bmr)
    expect_benchmark_result(bmr_new)
    expect_benchmark_result(bmr_combined)

    expect_data_table(bmr$data$fact, nrows = 18L)
    expect_data_table(bmr_new$data$fact, nrows = 6L)
    expect_data_table(bmr_combined$data$fact, nrows = 24L)

    expect_false("pima" %in% bmr$tasks$task_id)
    expect_true("pima" %in% bmr_new$tasks$task_id)
    expect_true("pima" %in% bmr_combined$tasks$task_id)
  }

  rr = resample(tsk("zoo"), lrn("classif.rpart"), rsmp("holdout"))
  bmr2 = c(combined[[1]], rr)
  expect_benchmark_result(bmr2)
  expect_data_table(bmr2$data$fact, nrows = 25L)
})

test_that("empty bmr", {
  bmr_new = BenchmarkResult$new()
  expect_benchmark_result(bmr_new)

  bmr_new$combine(NULL)
  expect_benchmark_result(bmr_new)

  bmr_new$combine(bmr)
  expect_benchmark_result(bmr_new)
  expect_data_table(bmr_new$data$fact, nrows = nrow(bmr$data))
})

test_that("bmr$resample_result()", {
  uhashes = bmr$uhashes
  expect_resample_result(bmr$resample_result(1L))
  expect_resample_result(bmr$resample_result(uhash = uhashes[1]))
  expect_error(bmr$resample_result(0))
  expect_error(bmr$resample_result(100))
  expect_error(bmr$resample_result(uhash = "a"))
  expect_error(bmr$resample_result(i = 1, uhash = uhashes[1]))
})

test_that("inputs are cloned", {
  task = tsk("iris")
  learner = lrn("classif.featureless")
  resampling = rsmp("holdout")

  expect_error(benchmark(data.table(task = list(task), learner = list(learner), resampling = list(resampling))), "instantiated")
  bmr = benchmark(design = data.table(task = list(task), learner = list(learner), resampling = list(resampling$instantiate(task))))
  rr = bmr$aggregate()$resample_result[[1L]]

  expect_different_address(task, rr$task)
  expect_different_address(learner, rr$data$learner_objs$learner[[1L]])
  expect_different_address(resampling, rr$resampling)
})

test_that("memory footprint", {
  expect_equal(uniqueN(map_chr(design$task, address)), 3L)
  expect_equal(uniqueN(map_chr(design$learner, address)), 2L)
  expect_equal(uniqueN(map_chr(design$resampling, address)), 3L)

  x = as.data.table(bmr)
  expect_equal(uniqueN(map_chr(x$task, address)), 3L)
  expect_equal(uniqueN(map_chr(x$resampling, address)), 3L)
})

test_that("multiple measures", {
  tasks = list(tsk("iris"), tsk("sonar"))
  learner = lrn("classif.featureless")
  measures = list(msr("classif.ce"), msr("classif.acc"))
  bmr = benchmark(design = benchmark_grid(tasks, learner, rsmp("cv", folds = 3)))
  expect_subset(c("classif.ce", "classif.acc"), names(bmr$aggregate(measures)))
})

test_that("predict_type is checked", {
  task = tsk("sonar")
  learner = lrn("classif.featureless")
  resampling = rsmp("cv", folds = 3L)
  design = benchmark_grid(task, learner, resampling)
  bmr = benchmark(design)
  expect_error(bmr$aggregate("classif.auc", "predict_type"))
})

test_that("custom resampling (#245)", {
  task_boston = tsk("boston_housing")
  lrn = lrn("regr.featureless")

  rdesc = rsmp("custom")
  train_sets = list((1:200), (1:300), (1:400))
  test_sets = list((201:301), (301:401), (401:501))
  rdesc$instantiate(task_boston, train_sets, test_sets)

  expect_resample_result(mlr3::resample(task_boston, lrn, rdesc))

  design = data.table(task = list(task_boston), learner = list(lrn), resampling = list(rdesc))
  bmr = benchmark(design)
  expect_benchmark_result(bmr)

  # Issue #451
  design = benchmark_grid(tasks = task_boston, learners = lrn, resamplings = rdesc)
  expect_data_table(design, nrows = 1)
})

test_that("extract params", {
  # some params, some not
  lrns = mlr_learners$mget(c("classif.rpart", "classif.rpart", "classif.rpart"))
  lrns[[1]]$param_set$values = list()
  lrns[[2]]$param_set$values = list(xval = 0, cp = 0.1)
  bmr = benchmark(benchmark_grid(tsk("wine"), lrns, rsmp("cv", folds = 3)))
  aggr = bmr$aggregate(params = TRUE)
  expect_list(aggr$params[[1]], names = "unique", len = 0L)
  expect_list(aggr$params[[2]], names = "unique", len = 2L)
  expect_list(aggr$params[[3]], names = "unique", len = 1L)

  # only one params
  lrns = mlr_learners$mget("classif.featureless")
  bmr = benchmark(benchmark_grid(tsk("wine"), lrns, rsmp("cv", folds = 3)))
  aggr = bmr$aggregate(params = TRUE)
  expect_list(aggr$params[[1]], names = "unique", len = 1L)

  # no params
  lrns = mlr_learners$mget("classif.debug")
  bmr = benchmark(benchmark_grid(tsk("wine"), lrns, rsmp("cv", folds = 3)))
  aggr = bmr$aggregate(params = TRUE)
  expect_list(aggr$params[[1]], names = "unique", len = 0L)
})

test_that("benchmark_grid", {
  learner = lrn("classif.rpart")

  tasks = tsks(c("iris", "iris"))
  resamp = rsmp("cv")$instantiate(tasks[[1]])
  expect_data_table(benchmark_grid(tasks, learner, resamp))

  tasks = tsks(c("iris", "sonar"))
  resamp = rsmp("cv")$instantiate(tasks[[1]])
  expect_error(benchmark_grid(tasks, learner, resamp), "rows")
})

test_that("filter", {
  tasks = lapply(c("iris", "sonar"), tsk)
  learners = lapply(c("classif.featureless", "classif.rpart"), lrn)
  resamplings = list(rsmp("cv", folds = 3), rsmp("holdout"))

  design = benchmark_grid(tasks, learners, resamplings)
  bmr = benchmark(design)

  expect_data_table(bmr$data$fact, nrows = 16)

  bmr$filter(task_ids = "sonar")
  expect_data_table(bmr$data$fact, nrows = 8)

  bmr$filter(learner_ids = "classif.rpart")
  expect_data_table(bmr$data$fact, nrows = 4)

  bmr$filter(resampling_ids = "cv")
  expect_data_table(bmr$data$fact, nrows = 3)
})

test_that("parallelization works", {
  skip_on_os("windows") # currently buggy

  grid = benchmark_grid(list(tsk("wine"), tsk("sonar")), replicate(2, lrn("classif.debug")), rsmp("cv", folds = 2))
  njobs = 3L
  bmr = with_future(future::multisession,  {
    benchmark(grid, store_models = TRUE)
  }, workers = njobs)

  expect_benchmark_result(bmr)
  pids = map_int(as.data.table(bmr)$learner, function(x) x$model$pid)
  expect_equal(length(unique(pids)), njobs)
})

test_that("friedman.test", {
  expect_is(friedman.test(bmr), "htest")
})
