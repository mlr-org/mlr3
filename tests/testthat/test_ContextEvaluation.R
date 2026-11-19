test_that("ContextResample works", {
  task = tsk("sonar")
  learner = lrn("classif.rpart")
  resampling = rsmp("cv", folds = 3)
  iteration = 1

  ctx = ContextResample$new(task, learner, resampling, iteration)

  expect_task(ctx$task)
  expect_learner(ctx$learner)
  expect_resampling(ctx$resampling)
  expect_equal(ctx$iteration, iteration)

  expect_error(
    {
      ctx$task = tsk("spam")
    },
    "read-only"
  )
  expect_error(
    {
      ctx$resampling = rsmp("cv", folds = 5)
    },
    "read-only"
  )
  expect_error(
    {
      ctx$iteration = 2
    },
    "read-only"
  )
})

test_that("ContextResample checks pdatas and data_extra", {
  task = tsk("sonar")
  learner = lrn("classif.rpart")$train(task)
  ctx = ContextResample$new(task, learner, rsmp("holdout"), 1L)

  expect_null(ctx$pdatas)
  expect_null(ctx$data_extra)

  pdatas = list(test = as_prediction_data(learner$predict(task)))
  ctx$pdatas = pdatas
  expect_equal(ctx$pdatas, pdatas)
  expect_error(
    {
      ctx$pdatas = list(1)
    },
    "PredictionData"
  )

  ctx$data_extra = list(a = 1)
  expect_equal(ctx$data_extra, list(a = 1))
  expect_error(
    {
      ctx$data_extra = 1
    },
    "list"
  )
})
