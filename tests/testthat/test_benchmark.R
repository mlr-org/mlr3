tmp = tsk("iris", id = "iris_small")$select("Sepal.Length")
tasks = c(mlr_tasks$mget(c("iris", "sonar")), list(tmp))
learners = mlr_learners$mget(c("classif.featureless", "classif.rpart"))
resamplings = rsmp("cv", folds = 3)
design = benchmark_grid(tasks, learners, resamplings)
bmr = benchmark(design)

test_that("Basic benchmarking", {
  expect_benchmark_result(bmr)
  expect_names(names(as.data.table(bmr)), permutation.of = c(mlr_reflections$rr_names, "uhash", "prediction"))

  tab = as.data.table(bmr)
  expect_data_table(tab, nrows = 18L, ncols = 6L)
  expect_names(names(tab), permutation.of = c("uhash", "prediction", mlr_reflections$rr_names))
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

    expect_false("pima" %in% bmr$tasks$task_id)
    expect_true("pima" %in% bmr_new$tasks$task_id)
    expect_true("pima" %in% bmr_combined$tasks$task_id)
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

  expect_true(all(c("warnings", "errors") %in% names(bmr$score(conditions = TRUE))))
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

  expect_data_table(get_private(bmr)$.data$data$fact, nrows = 16)

  bmr$filter(task_ids = "sonar")
  expect_data_table(get_private(bmr)$.data$data$fact, nrows = 8)
  expect_resultdata(get_private(bmr)$.data, TRUE)

  bmr$filter(learner_ids = "classif.rpart")
  expect_data_table(get_private(bmr)$.data$data$fact, nrows = 4)
  expect_resultdata(get_private(bmr)$.data, TRUE)

  bmr$filter(resampling_ids = "cv")
  expect_data_table(get_private(bmr)$.data$data$fact, nrows = 3)
  expect_resultdata(get_private(bmr)$.data, TRUE)

  expect_benchmark_result(bmr)
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

test_that("encapsulatiion", {
  learners = list(lrn("classif.debug", error_train = 1), lrn("classif.rpart"))
  grid = benchmark_grid(tasks, learners, resamplings)

  expect_error(benchmark(grid), "classif.debug->train()")
  bmr = benchmark(grid, encapsulate = "evaluate")
  aggr = bmr$aggregate(conditions = TRUE)
  expect_true(all(aggr[learner_id == "classif.debug", errors] == 3L))
  expect_true(all(aggr[learner_id != "classif.debug", errors] == 0L))

  for (learner in bmr$learners$learner) {
    expect_class(learner$fallback, "LearnerClassifFeatureless")
    expect_equal(learner$encapsulation[["train"]], "evaluate")
    expect_equal(learner$encapsulation[["predict"]], "evaluate")
  }
})

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
    tasks = tsks(c("iris", "boston_housing")),
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
  expect_true(identical(design$task[[1]], design$task[[2]]))
  expect_true(identical(design$task[[3]], design$task[[4]]))
  expect_false(identical(design$task[[1]], design$task[[3]]))

  expect_true(identical(design$resampling[[1]], design$resampling[[2]]))
  expect_true(identical(design$resampling[[3]], design$resampling[[4]]))
  expect_false(identical(design$resampling[[1]], design$resampling[[3]]))

  expect_true(identical(design$learner[[1]], design$learner[[3]]))
  expect_true(identical(design$learner[[2]], design$learner[[4]]))
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
  expect_equal(nrow(as.data.table(bmr)), 3)
  learner = bmr$resample_result(1)$learner
  expect_equal(learner$param_set$values$x, 1)
  expect_equal(nrow(as.data.table(bmr)), 3)

  # multiple parameters set via manual design
  design = data.table(task = tasks, learner = learners, resampling = resamplings, param_values = list(list(list(x = 1), list(x = 0.5))))
  bmr = benchmark(design)
  expect_benchmark_result(bmr)
  expect_equal(bmr$n_resample_results, 2)
  expect_equal(nrow(as.data.table(bmr)), 6)
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
  design = benchmark_grid(tasks, lrns(c("classif.debug", "classif.debug")), rsmp("holdout"), param_values = list(list(list(x = 1), list(x = 0.5)), list()))
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
  ii = which(map_lgl(trained, function(x) "cp" %in% names(x$param_set$values))) # find learner with cp
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

  expect_error(benchmark(benchmark_grid(task, learner, rsmp("holdout"))), "missing values")
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

test_that("obs_loss", {
  bmr = benchmark(benchmark_grid(
    tsk("iris"),
    lrn("classif.rpart"),
    rsmp("holdout")
  ))
  tbl = bmr$obs_loss(msrs(c("classif.acc", "classif.auc")))
  expect_true(all(is.na(tbl$classif.auc)))
  expect_integer(tbl$classif.acc)
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
