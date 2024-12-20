test_that("ContextResample works", {
  task = tsk("pima")
  learner = lrn("classif.rpart")
  resampling = rsmp("cv", folds = 3)
  iteration = 1

  ctx = ContextResample$new(task, learner, resampling, iteration)

  expect_task(ctx$task)
  expect_learner(ctx$learner)
  expect_resampling(ctx$resampling)
  expect_equal(ctx$iteration, iteration)

  expect_error({ctx$task = tsk("spam")}, "read-only")
  expect_error({ctx$resampling = rsmp("cv", folds = 5)}, "read-only")
  expect_error({ctx$iteration = 2}, "read-only")
})
