context("train/predict error handling")

test_that("Simple training", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.rpart")
  e = Experiment$new(task = task, learner = learner)
  e$train()

  # FIXME: temporarily disabled
  # e$train(1:10) # this should fail because we just train on a single class
  # expect_true(e$has_errors)
  # expect_data_table(e$data$train_log, min.rows = 1L)

  # e$predict(11:20)
  # expect_true(e$has_errors)
  # expect_data_table(e$predictions, nrow = 10)
  # expect_atomic_vector(e$predictions$predicted, any.missing = FALSE)
})
