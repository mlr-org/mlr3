skip_if_not_installed("mirai")

test_that("parallel resample", {
  with_mirai({
    task = tsk("pima")
    learner = lrn("classif.rpart")
    rr = resample(task, learner, rsmp("cv", folds = 3))
    expect_resample_result(rr)
    expect_data_table(rr$errors, nrows = 0L)
  }, compute = "mlr3_parallelization")
})

test_that("parallel benchmark", {
  task = tsk("pima")
  learner = lrn("classif.rpart")

  with_mirai({
    bmr = benchmark(benchmark_grid(task, learner, rsmp("cv", folds = 3)))
  }, compute = "mlr3_parallelization")
  expect_benchmark_result(bmr)
  expect_equal(bmr$aggregate(conditions = TRUE)$warnings, 0L)
  expect_equal(bmr$aggregate(conditions = TRUE)$errors, 0L)
})

test_that("mirai resample is reproducible", {
  with_mirai({
    task = tsk("pima")
    learner = lrn("classif.debug")
    resampling = rsmp("cv", folds = 3)
    rr = resample(task, learner, resampling, store_models = TRUE)
  }, compute = "mlr3_parallelization", seed = 123)
  random_number_1 = map_int(rr$learners, function(l) l$model$random_number)


  with_mirai({
    task = tsk("pima")
    learner = lrn("classif.debug")
    resampling = rsmp("cv", folds = 3)
    rr = resample(task, learner, resampling, store_models = TRUE)
  }, compute = "mlr3_parallelization", seed = 123)
  random_number_2 = map_int(rr$learners, function(l) l$model$random_number)

  expect_equal(random_number_1, random_number_2)
})

test_that("data table threads are not changed in main session", {
  skip_on_os("mac") # number of threads cannot be changed on mac
  skip_on_cran()

  old_dt_threads = getDTthreads()
  on.exit({
    setDTthreads(old_dt_threads)
  }, add = TRUE)
  setDTthreads(2L)

  task = tsk("sonar")
  learner = lrn("classif.debug", predict_type = "prob")
  resampling = rsmp("cv", folds = 3L)
  measure = msr("classif.auc")

  with_seed(123, with_mirai(resample(task, learner, resampling), compute = "mlr3_parallelization"))
  expect_equal(getDTthreads(), 2L)
})

test_that("parallel resample and encapsulation works", {
  skip_on_cran()

  with_mirai({
    # start encapsulation daemons on all daemons
    mirai::everywhere({mirai::daemons(1, .compute = "mlr3_encapsulation")}, .compute = "mlr3_parallelization")

    task = tsk("pima")
    learner = lrn("classif.debug")
    learner$encapsulate("mirai", lrn("classif.featureless"))
    resampling = rsmp("cv", folds = 3)
    rr = resample(task, learner, resampling, store_models = TRUE)

    expect_resample_result(rr)
    expect_data_table(rr$errors, nrows = 0L)
    # check that all resample iterations are evaluated on daemon
    mirai::status(.compute = "mlr3_parallelization")$mirai["completed"]
    expect_equal(mirai::status(.compute = "mlr3_parallelization")$mirai["completed"], c("completed" = 4L)) # 3 resample iterations + everywhere call

    # check that all encapsulation calls are evaluated on daemon
    status_encapsulation = mirai::collect_mirai(mirai::mirai({mirai::status(.compute = "mlr3_encapsulation")}, .compute = "mlr3_parallelization"))
    expect_equal(status_encapsulation$mirai["completed"], c("completed" = 6L)) # 3 x train + 3 x predict
  }, compute = "mlr3_parallelization")
})

test_that("mirai compute profile can be changed", {
  old_opts = getOption("mlr3.mirai_parallelization")
  on.exit(options(mlr3.mirai_parallelization = old_opts), add = TRUE)

  with_mirai({
    options(mlr3.mirai_parallelization = "mlr3_parallelization2")
    task = tsk("pima")
    learner = lrn("classif.debug")
    resampling = rsmp("cv", folds = 3)
    rr = resample(task, learner, resampling, store_models = TRUE)
  }, compute = "mlr3_parallelization2")

  expect_resample_result(rr)
})

