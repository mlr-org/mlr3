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
  expect_true(measure$minimize)
})
