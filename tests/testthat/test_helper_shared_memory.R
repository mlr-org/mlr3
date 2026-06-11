skip_if_not_installed("mori")

test_that("share_backends() moves backend data into shared memory and unshare_backends() restores it", {
  old_opts = options(mlr3.shared_memory = TRUE)
  on.exit(options(old_opts))

  task = tsk("mtcars")
  hash = task$hash
  original = get_private(task$backend)$.data
  data = task$data()

  restore = share_backends(list(task))
  expect_list(restore, len = 1L)
  expect_true(mori::is_shared(get_private(task$backend)$.data))
  expect_identical(task$hash, hash)
  expect_identical(task$data(), data)

  # sharing an already shared backend is a no-op
  expect_list(share_backends(list(task)), len = 0L)

  unshare_backends(restore)
  expect_identical(get_private(task$backend)$.data, original)
  expect_identical(task$hash, hash)
})

test_that("share_backends() is a no-op if the option is not set", {
  task = tsk("mtcars")
  expect_list(share_backends(list(task)), len = 0L)
  expect_false(mori::is_shared(get_private(task$backend)$.data))
})

test_that("share_backends() skips backends that are not DataBackendDataTable", {
  old_opts = options(mlr3.shared_memory = TRUE)
  on.exit(options(old_opts))

  task = tsk("mtcars")
  task$cbind(data.frame(foo = runif(task$nrow)))
  expect_class(task$backend, "DataBackendCbind")
  expect_list(share_backends(list(task)), len = 0L)
})

test_that("resample() with option mlr3.shared_memory", {
  task = tsk("mtcars")
  learner = lrn("regr.featureless")
  resampling = rsmp("cv", folds = 3L)$instantiate(task)

  rr1 = resample(task, learner, resampling)

  old_opts = options(mlr3.shared_memory = TRUE)
  on.exit(options(old_opts))
  rr2 = resample(task, learner, resampling)

  expect_resample_result(rr2)
  expect_equal(rr1$aggregate(), rr2$aggregate())
  expect_false(mori::is_shared(get_private(task$backend)$.data))
  expect_false(mori::is_shared(get_private(rr2$task$backend)$.data))
})

test_that("benchmark() with option mlr3.shared_memory", {
  design = benchmark_grid(tsks(c("iris", "sonar")), lrn("classif.featureless"), rsmp("holdout"))

  old_opts = options(mlr3.shared_memory = TRUE)
  on.exit(options(old_opts))
  bmr = benchmark(design)

  expect_benchmark_result(bmr)
  expect_data_table(bmr$aggregate(), nrows = 2L)
  for (task in bmr$tasks$task) {
    expect_false(mori::is_shared(get_private(task$backend)$.data))
  }
})
