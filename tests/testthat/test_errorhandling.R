context("Error handling")

test_that("Learner exceptions are signaled", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.crashtest")
  e = Experiment$new(task, learner)
  r = mlr_resamplings$get("holdout")

  run_expects = function() {
    expect_error(e$train(), "classif.crashtest")
    expect_error(resample(task, learner, r), "classif.crashtest")
    expect_error(benchmark(list(task), list(learner), list(r)), "classif.crashtest")
  }

  withr::with_options(list(mlr3.use.future = FALSE), run_expects())
  withr::with_options(list(mlr3.use.future = TRUE), run_expects())
})
