test_that("autotest catches error in train", {
  learner = lrn("classif.debug", error_train = 1)
  task = tsk("spam")

  result = run_experiment(task, learner)
  expect_false(result$ok)
  expect_integer(result$seed)
  expect_task(result$task)
  expect_learner(result$learner)
  expect_null(result$prediction)
  expect_null(result$score)
  expect_string(result$error)
})

test_that("autotest catches error in predict", {
  learner = lrn("classif.debug", error_predict = 1)
  task = tsk("spam")

  result = run_experiment(task, learner)
  expect_false(result$ok)
  expect_integer(result$seed)
  expect_task(result$task)
  expect_learner(result$learner)
  expect_null(result$prediction)
  expect_null(result$score)
  expect_string(result$error)
})
