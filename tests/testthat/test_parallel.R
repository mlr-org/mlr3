context("parallelization")

test_that("parallel resample", {
  skip_if_not_installed("future")
  skip_if_not_installed("future.callr")

  with_future(future.callr::callr, {
    expect_true(use_future())

    task = mlr_tasks$get("iris")
    learner = mlr_learners$get("classif.rpart")

    rr = resample(task, learner, "cv3")
    expect_resample_result(rr)
    expect_data_table(rr$errors, nrow = 0L)
  })
})

test_that("parallel benchmark", {
  skip_if_not_installed("future")
  skip_if_not_installed("future.callr")

  with_future(future.callr::callr, {
    expect_true(use_future())

    task = mlr_tasks$get("iris")
    learner = mlr_learners$get("classif.rpart")

    bmr = benchmark(expand_grid(task, learner, "cv3"))
    expect_benchmark_result(bmr)
    expect_equal(bmr$aggregate(warnings = TRUE)$warnings, 0L)
    expect_equal(bmr$aggregate(errors = TRUE)$errors, 0L)
  })
})
