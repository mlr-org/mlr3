test_that("internal valid score", {
  task = tsk("iris")
  learner = lrn("classif.debug", validate = 0.2)$train(task)
  pred = learner$predict(task)
  rr = resample(task, learner, rsmp("holdout"))
  expect_equal(
    rr$score(msr("internal_valid_score", select = "acc"))$internal_valid_score,
    rr$learners[[1]]$internal_valid_scores$acc
  )
  expect_equal(
    rr$score(msr("internal_valid_score", select = "wrong_name"))$internal_valid_score,
    NA_real_
  )
  rr = resample(task, lrn("classif.rpart"), rsmp("holdout"))
  expect_equal(
    rr$score(msr("internal_valid_score", select = "acc"))$internal_valid_score,
    NA_real_
  )
  expect_measure(msr("internal_valid_score"))

  # learner that does not have it
  task = tsk("mtcars")
  learner = lrn("regr.debug")
  learner$train(task)
  pred = learner$predict(task)
  rr = resample(task, learner, rsmp("holdout"))
  expect_equal(rr$score(msr("internal_valid_score", select = "a"))$internal_valid_score, NA_real_)
})
