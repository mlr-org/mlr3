context("parallelization")

test_that("parallel resample", {
  skip_if_not_installed("future")
  skip_if_not_installed("future.callr")
  skip_if_not_installed("progressr")

  with_future(future.callr::callr, {
    expect_true(use_future())

    task = tsk("iris")
    learner = lrn("classif.rpart")

    progressr::with_progress({
      rr = resample(task, learner, rsmp("cv", folds = 3))
    })
    expect_resample_result(rr)
    expect_data_table(rr$errors, nrows = 0L)
  })
})

test_that("parallel benchmark", {
  skip_if_not_installed("future")
  skip_if_not_installed("future.callr")

  with_future(future.callr::callr, {
    expect_true(use_future())

    task = tsk("iris")
    learner = lrn("classif.rpart")

    progressr::with_progress({
      bmr = benchmark(benchmark_grid(task, learner, rsmp("cv", folds = 3)))
    })
    expect_benchmark_result(bmr)
    expect_equal(bmr$aggregate(conditions = TRUE)$warnings, 0L)
    expect_equal(bmr$aggregate(conditions = TRUE)$errors, 0L)
  })
})

test_that("real parallel resample", {
  skip_if_not_installed("future")
  skip_if_not_installed("future.callr")
  skip_if_not_installed("progressr")
  skip_on_os("windows") # currently buggy

  with_future(future::multiprocess, {
    expect_true(use_future())

    task = tsk("iris")
    learner = lrn("classif.rpart")

    progressr::with_progress({
      rr = resample(task, learner, rsmp("cv", folds = 3))
    })
    expect_resample_result(rr)
    expect_data_table(rr$errors, nrows = 0L)
  })
})
