test_that("inner valid score", {
  task = tsk("iris")
  learner = lrn("classif.debug", validate = 0.2)$train(task)
  pred = learner$predict(task)
  rr = resample(task, learner, rsmp("holdout"))
  expect_equal(
    rr$score(msr("inner_valid_score", select = "acc"))$inner_valid_score,
    rr$learners[[1]]$inner_valid_scores$acc
  )
  expect_equal(
    rr$score(msr("inner_valid_score", select = "wrong_name"))$inner_valid_score,
    NA_real_
  )
  rr = resample(task, lrn("classif.rpart"), rsmp("holdout"))
  expect_equal(
    rr$score(msr("inner_valid_score", select = "acc"))$inner_valid_score,
    NA_real_
  )
  expect_measure(msr("inner_valid_score"))
})
