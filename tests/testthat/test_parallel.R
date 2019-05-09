context("parallelization")


test_that("parallel resample", {
  skip_if_not_installed("future")
  skip_if_not_installed("future.callr")

  with_future(future.callr::callr, {
    expect_true(use_future())

    task = mlr_tasks$get("iris")
    learner = mlr_learners$get("classif.rpart")
    resampling = mlr_resamplings$get("cv")
    resampling$param_set$values = list(folds = 3)

    rr = resample(task, learner, resampling)
    expect_resample_result(rr)
    expect_false(any(rr$errors))
  }
  )
}
)

test_that("parallel benchmark", {
  skip_if_not_installed("future")
  skip_if_not_installed("future.callr")

  with_future(future.callr::callr, {
    expect_true(use_future())

    task = mlr_tasks$get("iris")
    learner = mlr_learners$get("classif.rpart")
    resampling = mlr_resamplings$get("cv")
    resampling$param_set$values = list(folds = 3)

    bmr = benchmark(expand_grid(list(task), list(learner), list(resampling)))
    expect_benchmark_result(bmr)
  }
  )
}
)
