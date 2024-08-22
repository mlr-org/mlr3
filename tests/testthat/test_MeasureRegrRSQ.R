test_that("MeasureRegrRSQ works", {

  measure = msr("regr.rsq")
  expect_equal(measure$properties, character(0))

  pred = PredictionRegr$new(truth = 0:10, response = 2:12, row_ids = seq(11))
  expect_equal(pred$score(measure), c(rsq = 0.6))

  pred = PredictionRegr$new(truth = seq(0, 2, 0.5), response = seq(0, 2, 0.5), row_ids = seq(5))
  expect_equal(pred$score(measure), c(rsq = 1.0))

  pred = PredictionRegr$new(truth = seq(1, 4), response = c(1, 2, 3, 5), row_ids = seq(4))
  expect_equal(pred$score(measure), c(rsq = 0.8))

  measure = msr("regr.rsq", pred_set_mean = FALSE)
  expect_subset(measure$properties, c("requires_task", "requires_train_set"))

  pred = PredictionRegr$new(truth = 0:10, response = 2:12, row_ids = seq(11))
  task = as_task_regr(data.table(y = 0:10), target = "y")
  expect_equal(pred$score(measure, task = task, train_set = seq(11)), c(rsq = 0.6))

  pred = PredictionRegr$new(truth = seq(0, 2, 0.5), response = seq(0, 2, 0.5), row_ids = seq(5))
  task = as_task_regr(data.table(y = seq(0, 2, 0.5)), target = "y")
  expect_equal(pred$score(measure, task = task, train_set = seq(5)), c(rsq = 1.0))

  pred = PredictionRegr$new(truth = seq(1, 4), response = c(1, 2, 3, 5), row_ids = seq(4))
  task = as_task_regr(data.table(y = seq(1, 4)), target = "y")
  expect_equal(pred$score(measure, task = task, train_set = seq(4)), c(rsq = 0.8))
})
