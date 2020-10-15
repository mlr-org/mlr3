test_that("autotest", {
  learner = lrn("regr.featureless")
  expect_learner(learner)
  result = run_autotest(learner, exclude = "sanity")
  expect_true(result, info = result$error)
})

test_that("regr.featureless works on featureless task", {
  task = tsk("mtcars")$select(character())
  learner = lrn("regr.featureless")
  rr = resample(task, learner, rsmp("holdout"))
  expect_resample_result(rr)
  expect_number(rr$aggregate())
})
