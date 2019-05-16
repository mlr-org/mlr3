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
  })
})

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
  })
})


# test_that("parallel resample / duplication", {
#   skip_if_not_installed("future")
#   skip_if_not_installed("future.callr")

#   task = mlr_tasks$get("iris")
#   learner = mlr_learners$get("classif.rpart")
#   resampling = mlr_resamplings$get("cv3")$instantiate(task)
#   ctrl = mlr_control(store_model = FALSE)

#   rr1 = with_future("sequential", {
#     expect_false(use_future())
#     resample(task, learner, resampling, ctrl = ctrl)
#   })

#   rr2 = with_future("multiprocess", {
#     expect_true(use_future())
#     resample(task, learner, resampling, ctrl = ctrl)
#   })

#   pryr::object_size(learner)
#   pryr::object_size(rr1$data$learner[[1]])
#   pryr::object_size(rr2$data$learner[[1]])
# })
