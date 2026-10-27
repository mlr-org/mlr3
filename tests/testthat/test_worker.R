test_that("learner_predict() invalidates the task hash cache when subsetting rows", {
  LearnerSpy = R6Class("LearnerSpy",
    inherit = LearnerClassifFeatureless,
    public = list(hash_seen = NULL),
    private = list(
      .predict = function(task) {
        self$hash_seen = task$hash
        super$.predict(task)
      }
    )
  )

  task = tsk("iris")
  hash_full = task$hash
  learner = LearnerSpy$new()$train(task)
  hash_full_cached = task$hash

  learner$predict(task, row_ids = 1:10)
  expect_false(learner$hash_seen == hash_full_cached)
  expect_equal(task$hash, hash_full)
  expect_equal(task$row_hash, calculate_hash(task$row_ids))

  # uncached hash before predict must not be poisoned by the subset hash
  task$internal_valid_task = NULL
  learner$predict(task, row_ids = 1:10)
  expect_equal(task$hash, hash_full)

  rr = resample(tsk("iris"), LearnerSpy$new(), rsmp("holdout"))
  expect_equal(rr$task$hash, hash_full)
})
