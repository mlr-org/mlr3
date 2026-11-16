test_that("internal valid score", {
  task = tsk("iris")
  learner = lrn("classif.debug", validate = 0.2)$train(task)
  pred = learner$predict(task)
  rr = resample(task, learner, rsmp("holdout"))
  expect_equal(
    rr$score(msr("internal_valid_score", select = "acc"))$acc,
    rr$learners[[1]]$internal_valid_scores$acc
  )
  expect_equal(
    rr$score(msr("internal_valid_score", select = "wrong_name"))$wrong_name,
    NA_real_
  )
  rr = resample(task, lrn("classif.rpart"), rsmp("holdout"))
  expect_equal(
    rr$score(msr("internal_valid_score", select = "acc"))$acc,
    NA_real_
  )
  expect_measure(msr("internal_valid_score"))

  # learner that does not have it
  m = msr("internal_valid_score", select = "a")
  task = tsk("mtcars")
  learner = lrn("regr.debug")
  learner$train(task)
  pred = learner$predict(task)
  rr = resample(task, learner, rsmp("holdout"))
  expect_equal(rr$score(m)$a, NA_real_)

  task = tsk("iris")
  # the first validation score is taken by default
  rr = resample(task, lrn("classif.debug", predict_type = "prob", validate = 0.2), rsmp("holdout"))

  m = msr("internal_valid_score")
  expect_equal(
    rr$score(m)$internal_valid_score,
    rr$learners[[1]]$internal_valid_scores[[1L]]
  )

  measure = msr("internal_valid_score")
  expect_equal(measure$id, "internal_valid_score")
  expect_equal(measure$minimize, NA)
  measure = msr("internal_valid_score", select = "acc", minimize = TRUE)
  expect_equal(measure$id, "acc")
  expect_equal(measure$select, "acc")
  expect_true(measure$minimize)
})

test_that("internal valid score is NA if the score is not available", {
  task = tsk("iris")
  make_learner = function(scores) {
    LearnerScores = R6Class(
      "LearnerScores",
      inherit = LearnerClassifDebug,
      private = list(.extract_internal_valid_scores = function() scores)
    )
    learner = LearnerScores$new()
    learner$validate = 0.2
    learner$train(task)
  }

  learner = make_learner(list())
  expect_equal(msr("internal_valid_score")$score(NULL, learner = learner), NA_real_)
  expect_equal(msr("internal_valid_score", select = "acc")$score(NULL, learner = learner), NA_real_)

  learner = make_learner(c(acc = 0.5))
  expect_equal(msr("internal_valid_score")$score(NULL, learner = learner), 0.5)
  expect_equal(msr("internal_valid_score", select = "acc")$score(NULL, learner = learner), 0.5)
  expect_equal(msr("internal_valid_score", select = "wrong_name")$score(NULL, learner = learner), NA_real_)

  learner = make_learner(list(acc = c(0.5, 0.6)))
  expect_error(msr("internal_valid_score")$score(NULL, learner = learner), "single number as validation score 'acc'")
})

test_that("select is part of the hash", {
  m1 = msr("internal_valid_score", select = "acc", id = "valid")
  m2 = msr("internal_valid_score", select = "mbrier", id = "valid")
  expect_false(m1$hash == m2$hash)
  expect_equal(m1$hash, msr("internal_valid_score", select = "acc", id = "valid")$hash)
})
