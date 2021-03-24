test_that("Learner$update() method works", {
  task = tsk("iris")

  # updatable learner
  learner = LearnerClassifDebug$new()
  learner$train(task, 1:100)
  retrain_id = learner$model$retrain_id

  expect_true(learner$is_updatable(task, 101:150))
  learner$update(task, 101:150)
  expect_equal(learner$model$retrain_id, retrain_id)
  expect_equal(learner$state$model$update_rows, 50)

  # no model
  learner = LearnerClassifDebug$new()
  learner$update(task, 101:150)
  expect_class(learner$model, "classif.debug_model")

  # no model and disallow train
  learner = LearnerClassifDebug$new()
  
  expect_error(learner$update(task, 101:150, allow_train = FALSE),
    regexp = "Error: <LearnerClassifDebug:classif.debug> is not updatable.",
    fixed = TRUE)

  # non-updatable learner
  learner = LearnerClassifRpart$new()
  learner$train(task, 1:100)
  
  expect_false(learner$is_updatable(task, 101:150))
  learner$update(task, 101:150)
  expect_equal(length(learner$model$y), 50)

  # non-updatable learner and disallow train
  learner = LearnerClassifRpart$new()
  learner$train(task, 1:100)
  
  expect_error(learner$update(task, 101:150, allow_train = FALSE),
    regexp = "Error: <LearnerClassifDebug:classif.debug> is not updatable.",
    fixed = TRUE)

  # different task
  task = tsk("iris")
  learner = LearnerClassifDebug$new()
  learner$train(task)
  task$select("Petal.Length")

  expect_error(learner$update(task),
    regexp = "Assertion on 'task$feature_names' failed: Must be a permutation of set {Petal.Length,Petal.Width,Sepal.Length,Sepal.Width}.",
    fixed = TRUE)
})