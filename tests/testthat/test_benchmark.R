tmp = tsk("iris", id = "iris_small")$select("Sepal.Length")
tasks = c(mlr_tasks$mget(c("iris", "sonar")), list(tmp))
learners = mlr_learners$mget(c("classif.featureless", "classif.rpart"))
resamplings = rsmp("cv", folds = 3)
design = benchmark_grid(tasks, learners, resamplings)
bmr = benchmark(design)

test_that("Basic benchmarking", {
  expect_benchmark_result(bmr)
  expect_names(names(as.data.table(bmr)), permutation.of = c(mlr_reflections$rr_names, "uhash", "prediction", "task_id", "learner_id", "resampling_id"))

  tab = as.data.table(bmr)
  expect_data_table(tab, nrows = 18L, ncols = 9L)
  expect_names(names(tab), permutation.of = c("uhash", "prediction", mlr_reflections$rr_names, "task_id", "learner_id", "resampling_id"))
  measures = list(msr("classif.acc"))

  tab = bmr$score(measures, ids = FALSE, predictions = TRUE)
  expect_data_table(tab, nrows = 18L, ncols = 7L + length(measures))
  expect_names(names(tab), must.include = c("nr", "uhash", "prediction_test", mlr_reflections$rr_names, ids(measures)))
  expect_list(tab$prediction_test, "Prediction")

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
  m = measures[[1L]]
  expect_numeric(tab[[m$id]], any.missing = FALSE, lower = m$range[1], upper = m$range[2])
})

test_that("ResampleResult / hash", {
  m = msr("classif.ce")
  aggr = bmr$aggregate(m, uhashes = TRUE)
  nr = aggr$nr
  expect_integer(nr, len = 6L, any.missing = FALSE, unique = TRUE)

  for (i in nr) {
    rr = aggr$resample_result[[i]]
    expect_resample_result(rr)
    expect_equal(unname(rr$aggregate(m)), aggr[["classif.ce"]][i])
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

    expect_data_table(get_private(bmr)$.data$data$fact, nrows = 18L)
    expect_data_table(get_private(bmr_new)$.data$data$fact, nrows = 6L)
    expect_data_table(get_private(bmr_combined)$.data$data$fact, nrows = 24L)

    expect_false("pima" %chin% bmr$tasks$task_id)
    expect_true("pima" %chin% bmr_new$tasks$task_id)
    expect_true("pima" %chin% bmr_combined$tasks$task_id)
  }

  rr = resample(tsk("zoo"), lrn("classif.rpart"), rsmp("holdout"))
  bmr2 = c(combined[[1]], rr)
  expect_benchmark_result(bmr2)
  expect_data_table(get_private(bmr2)$.data$data$fact, nrows = 25L)
})

test_that("empty bmr", {
  bmr_new = BenchmarkResult$new()
  expect_benchmark_result(bmr_new)

  bmr_new$combine(NULL)
  expect_benchmark_result(bmr_new)

  bmr_new$combine(bmr)
  expect_benchmark_result(bmr_new)
  expect_data_table(get_private(bmr_new)$.data$data$fact, nrows = nrow(get_private(bmr)$.data))
})

test_that("bmr$resample_result()", {
  uhashes = bmr$uhashes
  expect_resample_result(bmr$resample_result(1L))
  expect_resample_result(bmr$resample_result(uhash = uhashes[1]))
  expect_resample_result(bmr$resample_result(learner_id = "classif.featureless", task_id = "iris"))
  expect_error(bmr$resample_result(learner_id = "classif.featureless"), "requires selecting exactly one")
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
  rr = bmr$resample_result(1)

  expect_different_address(task, rr$task)
  expect_different_address(learner, get_private(rr)$.data$data$learner_components$learner[[1L]])
  expect_different_address(resampling, rr$resampling)
})

test_that("memory footprint", {
  expect_equal(uniqueN(map_chr(design$task, address)), 3L)
  expect_equal(uniqueN(map_chr(design$learner, address)), 2L)
  expect_equal(uniqueN(map_chr(design$resampling, address)), 3L)

  x = as.data.table(bmr)
  expect_equal(uniqueN(map_chr(x$task, address)), 3L)
  expect_equal(uniqueN(map_chr(x$learner, address)), 18L)
  expect_equal(uniqueN(map_chr(x$resampling, address)), 3L)
})

test_that("resampling validation in benchmark_grid", {
  task1 = tsk("iris")
  task2 = tsk("pima")
  resampling_1 = rsmp("holdout")
  resampling_2 = rsmp("holdout")

  # should work when resamplings are instantiated on their corresponding tasks
  resampling_1$instantiate(task1)
  resampling_2$instantiate(task2)
  expect_data_table(benchmark_grid(list(task1, task2), lrn("classif.rpart"), list(resampling_1, resampling_2), paired = TRUE))

  # should fail when resamplings are not instantiated
  resampling_1 = rsmp("holdout")
  resampling_2 = rsmp("holdout")
  expect_error(benchmark_grid(list(task1, task2), lrn("classif.rpart"), list(resampling_1, resampling_2), paired = TRUE),
    "is not instantiated")

  # should fail when resampling is instantiated on wrong task
  resampling_1$instantiate(task1)
  resampling_2$instantiate(task1)
  expect_error(benchmark_grid(list(task1, task2), lrn("classif.rpart"), list(resampling_1, resampling_2), paired = TRUE),
    "not instantiated")

  # should fail when task row hashes don't match
  task1 = tsk("iris")
  task2 = tsk("iris")$filter(1:100)
  resampling_1 = rsmp("holdout")
  resampling_1$instantiate(task1)

  expect_error(benchmark_grid(list(task1, task2), lrn("classif.rpart"), list(resampling_1)),
    "not instantiated")

  # should fail when the tasks have the same number of rows but different row hashes
  task1 = tsk("iris")$filter(1:75)
  task2 = tsk("iris")$filter(76:150)
  resampling_1 = rsmp("holdout")
  resampling_1$instantiate(task1)
  expect_error(benchmark_grid(list(task1, task2), lrn("classif.rpart"), list(resampling_1)),
    "not instantiated")

  # should work when all resamplings are uninstantiated
  res1 = rsmp("holdout")
  res2 = rsmp("holdout")
  expect_data_table(benchmark_grid(list(task1, task2), lrn("classif.rpart"), list(res1, res2)))

  # should fail when some resamplings are instantiated and others are not
  res1$instantiate(task1)
  expect_error(benchmark_grid(list(task1, task2), lrn("classif.rpart"), list(res1, res2)),
    "All resamplings must be instantiated, or none at all")
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
  task_boston = tsk("california_housing")
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

test_that("extract params in aggregate and score", {
  # set params differently in a few learners
  lrns = list(
    lrn("classif.rpart", id = "rp1", xval = 0),
    lrn("classif.rpart", id = "rp2", xval = 0, cp = 0.2, minsplit = 2),
    lrn("classif.rpart", id = "rp3", xval = 0, cp = 0.1)
  )
  bmr = benchmark(benchmark_grid(tsk("wine"), lrns, rsmp("cv", folds = 3)))
  aggr = bmr$aggregate(params = TRUE)
  setorder(aggr, "learner_id")
  expect_list(aggr$params[[1]], names = "unique", len = 1L)
  expect_list(aggr$params[[2]], names = "unique", len = 3L)
  expect_list(aggr$params[[3]], names = "unique", len = 2L)

  scores = bmr$score()
  pvs = map(scores$learner, function(l) l$param_set$values)
  expect_true(all(sapply(split(lengths(pvs), scores$nr), uniqueN) == 1))
  expect_set_equal(lengths(pvs), 1:3)

  # only one params
  lrns = mlr_learners$mget("classif.featureless")
  bmr = benchmark(benchmark_grid(tsk("wine"), lrns, rsmp("cv", folds = 3)))
  aggr = bmr$aggregate(params = TRUE)
  expect_list(aggr$params[[1]], names = "unique", len = 1L)

  # no params
  lrns = mlr_learners$mget("classif.debug")
  lrns$classif.debug$param_set$values = list()
  bmr = benchmark(benchmark_grid(tsk("wine"), lrns, rsmp("cv", folds = 3)))
  aggr = bmr$aggregate(params = TRUE)
  expect_list(aggr$params[[1]], names = "unique", len = 0L)

  expect_true(all(c("warnings", "errors") %chin% names(bmr$score(conditions = TRUE))))
})

test_that("benchmark_grid", {
  learner = lrn("classif.rpart")

  tasks = tsks(c("iris", "iris"))
  resamp = rsmp("cv")$instantiate(tasks[[1]])
  expect_data_table(benchmark_grid(tasks, learner, resamp))

  tasks = tsks(c("iris", "sonar"))
  resamp = rsmp("cv")$instantiate(tasks[[1]])
  expect_error(benchmark_grid(tasks, learner, resamp), "not instantiated")
})

test_that("filter", {
  tasks = lapply(c("iris", "sonar"), tsk)
  learners = lapply(c("classif.featureless", "classif.rpart"), lrn)
  resamplings = list(rsmp("cv", folds = 3), rsmp("holdout"))

  design = benchmark_grid(tasks, learners, resamplings)
  bmr = benchmark(design)

  expect_data_table(get_private(bmr)$.data$data$fact, nrows = 16)

  bmr$filter(task_ids = "sonar")
  expect_data_table(get_private(bmr)$.data$data$fact, nrows = 8)
  expect_resultdata(get_private(bmr)$.data, TRUE)

  bmr$filter(learner_ids = "classif.rpart")
  expect_data_table(get_private(bmr)$.data$data$fact, nrows = 4)
  expect_resultdata(get_private(bmr)$.data, TRUE)

  bmr2 = bmr$clone(deep = TRUE)$filter(resampling_ids = "cv")
  expect_data_table(get_private(bmr2)$.data$data$fact, nrows = 3)
  expect_resultdata(get_private(bmr2)$.data, TRUE)

  bmr$filter(i = 2)
  expect_data_table(get_private(bmr)$.data$data$fact, nrows = 1)
  expect_resultdata(get_private(bmr)$.data, TRUE)

  expect_benchmark_result(bmr)
  expect_benchmark_result(bmr2)
})

test_that("aggregated performance values are calculated correctly (#555)", {
  task = tsk("spam")
  learner1 = lrn("classif.featureless")
  learner2 = lrn("classif.rpart")
  resampling = rsmp("subsampling", repeats = 2)

  design = benchmark_grid(task, list(learner1, learner2), resampling)
  bmr = benchmark(design = design, store_models = TRUE)

  y = bmr$aggregate()$classif.ce
  expect_gt(y[1], y[2])

  y = c(
    bmr$resample_result(1)$aggregate(msr("classif.ce")),
    bmr$resample_result(2)$aggregate(msr("classif.ce"))
  )
  expect_gt(y[1], y[2])
})

test_that("save/load roundtrip", {
  path = tempfile()
  saveRDS(bmr, file = path)

  bmr2 = readRDS(path)
  expect_benchmark_result(bmr2)
})

test_that("debug branch", {
  tmp = tsk("iris", id = "iris_small")$select("Sepal.Length")
  tasks = c(mlr_tasks$mget(c("iris", "sonar")), list(tmp))
  learners = mlr_learners$mget(c("classif.featureless", "classif.rpart"))
  resamplings = rsmp("cv", folds = 2)
  design = benchmark_grid(tasks, learners, resamplings)
  bmr = invoke(benchmark, design, .opts = list(mlr3.debug = TRUE))
  expect_benchmark_result(bmr)
})

# uncomment when evaluate 1.0.4 is released

# test_that("encapsulatiion", {
#   learners = list(lrn("classif.debug", error_train = 1), lrn("classif.rpart"))
#   grid = benchmark_grid(tasks, learners, resamplings)

#   expect_error(benchmark(grid), "classif.debug->train()")
#   bmr = benchmark(grid, encapsulate = "evaluate")
#   aggr = bmr$aggregate(conditions = TRUE)
#   expect_true(all(aggr[learner_id == "classif.debug", errors] == 3L))
#   expect_true(all(aggr[learner_id != "classif.debug", errors] == 0L))

#   for (learner in bmr$learners$learner) {
#     expect_class(learner$fallback, "LearnerClassifFeatureless")
#     expect_equal(learner$encapsulation[["train"]], "evaluate")
#     expect_equal(learner$encapsulation[["predict"]], "evaluate")
#   }
# })

test_that("disable cloning", {
  grid = benchmark_grid(
    tasks = tsk("iris"),
    learners = lrn("classif.featureless"),
    resamplings = rsmp("holdout")
  )
  task = grid$task[[1L]]
  learner = grid$learner[[1L]]
  resampling = grid$resampling[[1L]]

  bmr = benchmark(grid, clone = c())

  expect_same_address(task, bmr$tasks$task[[1]])
  expect_same_address(learner, get_private(bmr)$.data$data$learners$learner[[1]])
  expect_same_address(resampling, bmr$resamplings$resampling[[1]])

  expect_identical(task$hash, bmr$tasks$task[[1]]$hash)
  expect_identical(learner$hash, bmr$learners$learner[[1]]$hash)
  expect_identical(resampling$hash, bmr$resamplings$resampling[[1]]$hash)
})

test_that("task and learner assertions", {
  grid = benchmark_grid(
    tasks = tsks(c("iris", "california_housing")),
    learners = lrn("classif.rpart"),
    resamplings = rsmp("holdout")
  )

  expect_error(benchmark(grid), "task types")

  grid = benchmark_grid(
    tasks = tsk("iris"),
    learners = lrns(c("classif.rpart", "regr.rpart")),
    resamplings = rsmp("holdout")
  )

  expect_error(benchmark(grid), "learner types")

  grid = benchmark_grid(
    tasks = tsk("iris"),
    learners = lrn("regr.rpart"),
    resamplings = rsmp("holdout")
  )

  expect_error(benchmark(grid), "not match type")
})


test_that("benchmark_grid works if paired = TRUE", {
  tasks = mlr3::tsks(c("pima", "iris"))
  learners = lrns(c("classif.featureless", "classif.rpart"))
  resampling = rsmp("cv")
  resamplings = pmap(
    list(tasks, rsmps(c("cv", "holdout"))),
    function(task, resampling) resampling$instantiate(task)
  )
  design = benchmark_grid(tasks, learners, resamplings, paired = TRUE)
  expect_class(design, "benchmark_grid")
  # design[, identical(task), by = task]]
  # expect(identical(design$resampling[class(learner)[[1]] ==)]))
  expect_true(nrow(design) == 4L) #
  expect_identical(design$task[[1]], design$task[[2]])
  expect_identical(design$task[[3]], design$task[[4]])
  expect_false(identical(design$task[[1]], design$task[[3]]))

  expect_identical(design$resampling[[1]], design$resampling[[2]])
  expect_identical(design$resampling[[3]], design$resampling[[4]])
  expect_false(identical(design$resampling[[1]], design$resampling[[3]]))

  expect_identical(design$learner[[1]], design$learner[[3]])
  expect_identical(design$learner[[2]], design$learner[[4]])
  expect_false(identical(design$learner[[2]], design$learner[[3]]))


  # Resamplings must be instantiated
  tasks = tsks(c("pima", "iris"))
  learners = lrns(c("classif.featureless", "classif.rpart"))
  resamplings = mlr3::rsmps(c("cv", "holdout"))
  expect_error(benchmark_grid(tasks, learners, resamplings, paired = TRUE))

  # Resamplings and tasks must have the same length
  tasks = tsks(c("pima", "iris"))
  learners = lrns(c("classif.featureless", "classif.rpart"))
  resamplings = pmap(
    list(tasks, mlr3::rsmps(c("cv", "holdout"))),
    function(task, resampling) resampling$instantiate(task)
  )
  resamplings = c(resamplings, resamplings)
  expect_error(benchmark_grid(tasks, learners, resamplings, paired = TRUE))


  # Resamplings and tasks must have corresponding hashes

  tasks = tsks(c("pima", "iris"))
  learners = lrns(c("classif.featureless", "classif.rpart"))
  resamplings = pmap(
    list(tasks, mlr3::rsmps(c("cv", "holdout"))),
    function(task, resampling) resampling$instantiate(task)
  )
  resamplings = rev(resamplings)
  expect_error(benchmark_grid(tasks, learners, resamplings, paired = TRUE))
})

test_that("param_values in benchmark", {
  # setup
  tasks = tsks("iris")
  resamplings = list(rsmp("cv", folds = 3)$instantiate(tasks[[1]]))
  learners = lrns("classif.debug")

  # single parameter set via manual design
  design = data.table(task = tasks, learner = learners, resampling = resamplings, param_values = list(list(list(x = 1))))
  bmr = benchmark(design)
  expect_benchmark_result(bmr)
  expect_equal(bmr$n_resample_results, 1)
  expect_shape(as.data.table(bmr), nrow = 3L)
  learner = bmr$resample_result(1)$learner
  expect_equal(learner$param_set$values$x, 1)
  expect_shape(as.data.table(bmr), nrow = 3L)

  # multiple parameters set via manual design
  design = data.table(task = tasks, learner = learners, resampling = resamplings, param_values = list(list(list(x = 1), list(x = 0.5))))
  bmr = benchmark(design)
  expect_benchmark_result(bmr)
  expect_equal(bmr$n_resample_results, 2)
  expect_shape(as.data.table(bmr), nrow = 6L)
  learner = bmr$resample_result(1)$learner
  expect_equal(learner$param_set$values$x, 1)
  learner = bmr$resample_result(2)$learner
  expect_equal(learner$param_set$values$x, 0.5)

  # benchmark grid does not attach param_values if empty
  design = benchmark_grid(tasks, learners, resamplings)
  expect_names(names(design), permutation.of = c("task", "learner", "resampling"))

  # benchmark grid with param_values
  design = benchmark_grid(tasks, learners, resamplings, param_values = list(list(list(x = 1))))
  expect_data_table(design, nrows = 1)
  expect_names(names(design), permutation.of = c("task", "learner", "resampling", "param_values"))
  bmr = benchmark(design)
  expect_benchmark_result(bmr)

  # benchmark grid with param_values and paired = TRUE
  design = benchmark_grid(tasks, learners, resamplings, param_values = list(list(list(x = 1))), paired = TRUE)
  expect_data_table(design, nrows = 1)
  bmr = benchmark(design)
  expect_benchmark_result(bmr)
  expect_equal(bmr$n_resample_results, 1)

  # benchmark grid with multiple params
  design = benchmark_grid(tasks, learners, resamplings, param_values = list(list(list(x = 1), list(x = 0.5))))
  expect_data_table(design, nrows = 1)
  bmr = benchmark(design)
  expect_benchmark_result(bmr)
  expect_equal(bmr$n_resample_results, 2)


  # benchmark grid with multiple params and multiple learners
  design = benchmark_grid(tasks, lrns(c("classif.debug", "classif.rpart")), rsmp("holdout"), param_values = list(list(list(x = 1), list(x = 0.5)), list()))
  bmr = benchmark(design)
  expect_benchmark_result(bmr)
  expect_equal(bmr$n_resample_results, 3)

  # constant values are inserted
  learners = lrns("classif.rpart", minsplit = 12)
  design = data.table(task = tasks, learner = learners, resampling = resamplings, param_values = list(list(list(cp = 0.1), list(minbucket = 2))))
  bmr = benchmark(design)

  sortnames = function(x) {
    if (!is.null(names(x))) {
      x <- x[order(names(x))]
    }
    x
  }
  trained = bmr$learners$learner
  ii = which(map_lgl(trained, function(x) "cp" %chin% names(x$param_set$values))) # find learner with cp
  expect_count(ii)

  expect_equal(sortnames(bmr$learners$learner[-ii][[1]]$param_set$values), list(minbucket = 2, minsplit = 12, xval = 0))
  expect_equal(sortnames(bmr$learners$learner[[ii]]$param_set$values), list(cp = 0.1, minsplit = 12, xval = 0))
})


test_that("learner's validate cannot be 'test' if internal_valid_set is present", {
  # otherwise, predict_set = "internal_valid" would be ambiguous
  learner = lrn("classif.debug", validate = "test", predict_sets = c("train", "internal_valid"))
  task = tsk("iris")
  task$internal_valid_task = 1
  expect_error(benchmark(benchmark_grid(task, learner, rsmp("holdout"))), "cannot be set to ")
})

test_that("learner's validate cannot be a ratio if internal_valid_set is present", {
  # otherwise, predict_set = "internal_valid" would be ambiguous
  learner = lrn("classif.debug", validate = 0.5, predict_sets = c("train", "internal_valid"))
  task = tsk("iris")
  task$internal_valid_task = 1
  expect_error(benchmark(benchmark_grid(task, learner, rsmp("holdout"))), "cannot be set to ")
})

test_that("properties are also checked on validation task", {
  task = tsk("iris")
  row = task$data(1)
  row[[1]][1] = NA
  row$..row_id = 151
  task$rbind(row)
  task$internal_valid_task = 151
  learner = lrn("classif.debug", validate = "predefined")
  learner$properties = setdiff(learner$properties, "missings")

  suppressWarnings(expect_error(benchmark(benchmark_grid(task, learner, rsmp("holdout"))), "missing values"))
})

test_that("unmarshal parameter is respected", {
  learner = lrn("classif.debug", count_marshaling = TRUE)
  learner$encapsulate("callr", lrn("classif.featureless"))
  task = tsk("iris")
  resampling = rsmp("holdout")
  design = benchmark_grid(task, learner, resampling)
  bmr = with_future(future::multisession, {
    list(
      marshaled = benchmark(design, store_models = TRUE, unmarshal = FALSE),
      unmarshaled = benchmark(design, store_models = TRUE, unmarshal = TRUE)
    )
  })
  expect_false(bmr$unmarshaled$resample_result(1)$learners[[1]]$marshaled)
  expect_true(bmr$marshaled$resample_result(1)$learners[[1]]$marshaled)
})

test_that("BenchmarkResult can be (un)marshaled", {
  bmr = benchmark(benchmark_grid(tsk("iris"), lrn("classif.debug"), rsmp("holdout")), store_models = TRUE)
  expect_false(bmr$resample_result(1)$learners[[1]]$marshaled)
  bmr$marshal()
  expect_true(bmr$resample_result(1)$learners[[1]]$marshaled)
  bmr$unmarshal()
  expect_false(bmr$resample_result(1)$learners[[1]]$marshaled)

  # also works with non-marshalable learner
  bmr1 = benchmark(benchmark_grid(tsk("iris"), lrn("classif.featureless"), rsmp("holdout")), store_models = TRUE)
  model = bmr1$resample_result(1)$learners[[1]]$model
  bmr1$unmarshal()
  expect_equal(bmr1$resample_result(1)$learners[[1]]$model, model)
})

test_that("predictions retrieved with as.data.table and predictions method are equal", {
  tab = as.data.table(bmr)
  predictions = unlist(map(bmr$resample_results$resample_result, function(rr) rr$predictions()), recursive = FALSE)
  expect_equal(tab$prediction, predictions)

  tab = as.data.table(bmr, predict_sets = "train")
  predictions = unlist(map(bmr$resample_results$resample_result, function(rr) rr$predictions(predict_sets = "train")), recursive = FALSE)
  expect_equal(tab$prediction, predictions)
})

test_that("score works with predictions and empty predictions", {
  learner_1 = lrn("classif.rpart", predict_sets = "train", id = "learner_1")
  learner_2 = lrn("classif.rpart", predict_sets = "test", id = "learner_2")
  task = tsk("pima")

  design = benchmark_grid(task, list(learner_1, learner_2), rsmp("holdout"))

  bmr = benchmark(design)

  expect_warning({tab = bmr$score(msr("classif.ce", predict_sets = "test"))}, "Measure")
  expect_equal(tab$classif.ce[1], NaN)
})

test_that("benchmark_grid only allows unique learner ids", {
  task = tsk("iris")
  learner = lrn("classif.rpart")
  resampling = rsmp("holdout")

  expect_error(benchmark_grid(task, list(learner, learner), resampling), "unique")
})

test_that("benchmark allows that param_values overwrites tune token", {
  learner = lrn("classif.rpart", cp = to_tune(0.01, 0.1))
  design = benchmark_grid(tsk("pima"), learner, rsmp("cv", folds = 3), param_values = list(list(list(cp = 0.01))))
  expect_benchmark_result(benchmark(design))

  learner = lrn("classif.rpart", cp = to_tune(0.01, 0.1))
  design = benchmark_grid(tsk("pima"), learner, rsmp("cv", folds = 3))
  expect_error(benchmark(design), "cannot be trained with TuneToken present")
})

test_that("uhash_table works", {
  design = benchmark_grid(tsks(c("iris", "sonar")), lrns(c("classif.debug", "classif.featureless")), rsmps(c("holdout", "insample")))
  bmr = benchmark(design)
  u = bmr$uhash_table

  # results agree with uhash_table from resample result, which is also tested for correctness
  for (i in seq_len(nrow(u))) {
    rr = bmr$resample_result(i)
    learner_id = rr$learner$id
    task_id = rr$task$id
    resampling_id = rr$resampling$id

    expect_equal(u$learner_id[i], learner_id)
    expect_equal(u$task_id[i], task_id)
    expect_equal(u$resampling_id[i], resampling_id)
    expect_equal(u$uhash[i], rr$uhash)
  }

  # uhash is in correct order
  expect_equal(u$uhash, bmr$uhashes)
  expect_equal(u$uhash, as.data.table(bmr)$uhash)
})

test_that("can change the threshold", {
  task = tsk("iris")$filter(1:80)$droplevels("Species")
  design = benchmark_grid(task, lrn("classif.featureless", predict_type = "prob"), rsmp("insample"))
  bmr = benchmark(design)

  # we can set the threshold and pass ties_method correctly
  expect_true(all(bmr$resample_result(1)$prediction()$response == "setosa"))
  bmr$set_threshold(0.9)
  expect_true(all(bmr$resample_result(1)$prediction()$response == "versicolor"))
  bmr$set_threshold(0.1)
  expect_true(all(bmr$resample_result(1)$prediction()$response == "setosa"))
  bmr$set_threshold(0.625, ties_method = "first")
  expect_true(all(bmr$resample_result(1)$prediction()$response == "setosa"))
  bmr$set_threshold(0.625, ties_method = "last")
  expect_true(all(bmr$resample_result(1)$prediction()$response == "versicolor"))
  with_seed(1, {
    bmr$set_threshold(0.625, ties_method = "random")
    expect_true("setosa" %in% bmr$resample_result(1)$prediction()$response && "versicolor" %in% bmr$resample_result(1)$prediction()$response)
  })

  # Don't modify any threshold when at least one operation is invalid
  design = suppressWarnings(benchmark_grid(
    task,
    c(lrn("classif.featureless", predict_type = "prob"), lrn("classif.debug")),
    rsmp("insample")
  ))
  bmr = benchmark(design)
  response = bmr$resample_result(1)$prediction()$response
  expect_error(bmr$set_threshold(0.9), "Cannot set threshold, no probabilities available")
  # the other prediction was also not affected, we want to avoid partial updates
  expect_equal(bmr$resample_result(1)$prediction()$response, response)

  bmr$set_threshold(0.9, uhashes = uhashes(bmr, learner_ids = "classif.featureless"))

  expect_true(all(bmr$resample_result(1)$prediction()$response == "versicolor"))

  # can also use the iters argument
  design = benchmark_grid(
    task,
    c(lrn("classif.featureless", predict_type = "prob"), lrn("classif.debug", predict_type = "prob")),
    rsmp("insample")
  )
  bmr = benchmark(design)
  bmr$set_threshold(0.9, i = 1)
  expect_true(all(bmr$resample_result(1)$prediction()$response == "versicolor"))
  expect_false(all(bmr$resample_result(2)$prediction()$response == "versicolor"))
})

test_that("uhashe(s) work", {
  design = benchmark_grid(
    tsks(c("iris", "sonar")),
    lrns(c("classif.featureless", "classif.rpart")),
    rsmp("holdout")
  )
  bmr = benchmark(design)

  tbl = bmr$uhash_table
  expect_equal(bmr$uhashes, uhashes(bmr))
  expect_equal(tbl[get("learner_id") == "classif.debug", "uhash"]$uhash, uhashes(bmr, learner_ids = "classif.debug"))
  expect_equal(tbl[get("task_id") == "sonar", "uhash"]$uhash, uhashes(bmr, task_ids = "sonar"))
  expect_equal(tbl[get("resampling_id") == "holdout", "uhash"]$uhash, uhashes(bmr, resampling_ids = "holdout"))
  all_uhashes = bmr$uhashes
  expect_equal(length(all_uhashes), 4) # 2 tasks * 2 learners

  # Test filtering by single ID
  featureless_uhashes = uhashes(bmr, learner_ids = "classif.featureless")
  expect_equal(length(featureless_uhashes), 2)
  expect_true(all(featureless_uhashes %in% all_uhashes))

  iris_uhashes = uhashes(bmr, task_ids = "iris")
  expect_equal(length(iris_uhashes), 2)
  expect_true(all(iris_uhashes %in% all_uhashes))

  holdout_uhashes = uhashes(bmr, resampling_ids = "holdout")
  expect_equal(length(holdout_uhashes), 4)
  expect_true(all(holdout_uhashes %in% all_uhashes))

  # Test filtering by multiple IDs
  learner_subset = uhashes(bmr, learner_ids = c("classif.featureless", "classif.rpart"))
  expect_equal(length(learner_subset), 4)
  expect_setequal(learner_subset, all_uhashes)

  task_subset = uhashes(bmr, task_ids = c("iris", "sonar"))
  expect_equal(length(task_subset), 4)
  expect_setequal(task_subset, all_uhashes)

  # Test combined filtering
  iris_featureless = uhashes(bmr,
    learner_ids = "classif.featureless",
    task_ids = "iris"
  )
  expect_equal(length(iris_featureless), 1)

  # Test uhash function with single valid combination
  single_uhash = uhash(bmr,
    learner_id = "classif.featureless",
    task_id = "iris",
    resampling_id = "holdout"
  )
  expect_string(single_uhash)
  expect_true(single_uhash %in% all_uhashes)
  expect_error(uhash(bmr), "got 4")

  # no match
  expect_equal(uhashes(bmr, "not-existing"), character(0))
  expect_error(uhash(bmr, "not-existing"), "Expected exactly one uhash")
  expect_equal(bmr$uhashes, uhashes(bmr))
  expect_equal(bmr$filter(1)$uhashes, uhash(bmr))
})

test_that("resampling validation", {
  # test with uninstantiated resampling
  task = tsk("iris")
  learner = lrn("classif.rpart")
  resampling = rsmp("holdout")
  design = data.table(task = list(task), learner = list(learner), resampling = list(resampling))
  expect_error(benchmark(design), "instantiated")

  # test with resampling instantiated on wrong task
  task1 = tsk("iris")
  task2 = tsk("pima")
  resampling = rsmp("holdout")
  resampling$instantiate(task1)
  design = data.table(task = list(task2), learner = list(learner), resampling = list(resampling))
  expect_error(benchmark(design), "not instantiated")

  # test with resampling instantiated on filtered task
  task = tsk("iris")
  resampling = rsmp("holdout")
  resampling$instantiate(task)
  task$filter(1:100)
  design = data.table(task = list(task), learner = list(learner), resampling = list(resampling))
  expect_error(benchmark(design), "not instantiated")

  # test with resampling instantiated on correct task
  task = tsk("iris")
  resampling = rsmp("holdout")
  resampling$instantiate(task)
  design = data.table(task = list(task), learner = list(learner), resampling = list(resampling))
  expect_benchmark_result(benchmark(design))
})

test_that("warning when mixing predict types", {
  expect_warning(benchmark_grid(
    tsk("iris"),
    list(
      lrn("classif.debug", predict_type = "prob"),
      lrn("classif.featureless", predict_type = "response")
    ),
    rsmp("cv", folds = 3)
  ), regexp = "Multiple predict types detected")
})

test_that("benchmark with tasks with weights", {

  learners = list(
    lrn("classif.featureless", use_weights = "ignore", predict_type = "prob", id = "ignores_weights"),
    lrn("classif.featureless", use_weights = "use", predict_type = "prob", id = "uses_weights")
  )
  measures = list(
    msr("classif.acc", use_weights = "ignore", id = "acc_ignore"),
    msr("classif.acc", use_weights = "use", id = "acc_use")
  )
  tasks = list(
    iris_weights_learner,
    iris_weights_measure,
    tsk("iris")
  )

  resamplings = list(
    rsmp("custom")$instantiate(tsk("iris"),
      train_sets = list(c(1:50, 140:150)),
      test_sets = list(c(1, 150))
    )
  )

  design = benchmark_grid(tasks, learners, resamplings)
  bmr = benchmark(design)

  predictions = map(1:6, function(i) bmr$resample_result(i)$prediction())

  # learner ignores weights, sees 50 'setosa' and 10 'virginica' -> predicts setosa
  expect_equal(predictions[[1]]$response[1], factor("setosa", levels = levels(iris$Species)))


  # learner uses weights, sees 50 'setosa' (weight 1) and 10 'virginica' (weight 100) -> predicts virginica
  expect_equal(predictions[[2]]$response[1], factor("virginica", levels = levels(iris$Species)))

  # task has no weights_learner -> unweighted 'setosa' predictions
  expect_equal(predictions[[3]]$response[1], factor("setosa", levels = levels(iris$Species)))
  expect_equal(predictions[[4]]$response[1], factor("setosa", levels = levels(iris$Species)))
  expect_equal(predictions[[5]]$response[1], factor("setosa", levels = levels(iris$Species)))
  expect_equal(predictions[[6]]$response[1], factor("setosa", levels = levels(iris$Species)))

  # 'weights' is NULL for all predictions not made for iris_weights_measure
  expect_null(predictions[[1]]$weights)
  expect_null(predictions[[2]]$weights)
  expect_null(predictions[[5]]$weights)
  expect_null(predictions[[6]]$weights)

  agpred = bmr$aggregate(measures)

  expect_equal(agpred$acc_ignore, rep(0.5, 6))  # made one correct, one incorrect prediction, unweighted

  # made one correct, one incorrect prediction, but 2nd task weighs the incorrect prediction x100
  expect_equal(agpred$acc_use, c(0.5, 0.5, 1 / 101, 1 / 101, 0.5, 0.5))

  expect_error(bmr$aggregate(msr("classif.acc", use_weights = "error")), "'use_weights' was set to 'error'")

  # no error when task has no weights_measure
  bmr = benchmark(design[c(1:2, 5:6)])
  expect_equal(bmr$aggregate(msr("classif.acc", use_weights = "error"))$classif.acc, c(0.5, 0.5, 0.5, 0.5))

  learners[[1]]$use_weights = "error"

  design = benchmark_grid(tasks, learners, resamplings)
  expect_error(benchmark(design), "'use_weights' was set to 'error'")

  # no error when task has no weights_learner
  design = benchmark_grid(tasks[2:3], learners, resamplings)
  bmr = benchmark(design)

})


test_that("obs_loss works", {
  bmr = benchmark(benchmark_grid(
    tsk("iris"),
    lrn("classif.rpart"),
    rsmp("cv", folds = 3)
  ))

  obs_loss = bmr$obs_loss(msr("classif.acc"))
  expect_data_table(obs_loss, nrows = 150L)
  expect_numeric(obs_loss$classif.acc, len = 150L)
  expect_set_equal(obs_loss$iteration, seq(3L))
  expect_set_equal(obs_loss$resample_result, 1L)

  obs_loss = bmr$obs_loss(msrs(c("classif.acc", "classif.auc")))
  expect_data_table(obs_loss, nrows = 150L)
  expect_numeric(obs_loss$classif.acc, len = 150L)
  expect_true(all(is.na(obs_loss$classif.auc)))
  expect_set_equal(obs_loss$iteration, seq(3L))
  expect_set_equal(obs_loss$resample_result, 1L)

  bmr = benchmark(benchmark_grid(
    tsk("iris"),
    lrn("classif.rpart"),
    c(rsmp("cv", folds = 3), rsmp("holdout"))
  ))

  obs_loss = bmr$obs_loss(msr("classif.acc"))
  expect_data_table(obs_loss, nrows = 200L)
  expect_numeric(obs_loss$classif.acc, len = 200L)
  expect_set_equal(obs_loss$iteration, seq(3L))
  expect_set_equal(obs_loss$resample_result, seq(2L))
})
