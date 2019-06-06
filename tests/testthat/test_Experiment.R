context("Experiment")

test_that("Empty Experiment construction", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.featureless")
  e = Experiment$new()
  expect_error(e$train(), "task")
  expect_different_address(task, e$task)
  expect_different_address(learner, e$learner)
})

test_that("Partial experiments + save/restore", {
  fn = tempfile(pattern = "exp_", fileext = ".rds")
  learner = learner = mlr_learners$get("classif.rpart")

  e = Experiment$new(task = mlr_tasks$get("iris"), learner = learner)
  saveRDS(e, file = fn)
  e = readRDS(fn)
  expect_experiment(e)
  e$train(row_ids = 1:120)
  saveRDS(e, file = fn)
  e = readRDS(fn)
  expect_experiment(e)
  e$predict(row_ids = 121:150)
  saveRDS(e, file = fn)
  e = readRDS(fn)
  expect_experiment(e)
  e$score()
  saveRDS(e, file = fn)
  e = readRDS(fn)
  expect_experiment(e)
})

test_that("inputs are cloned", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.featureless")

  e = Experiment$new(task, learner)
  expect_different_address(task, e$task)
  expect_different_address(learner, e$learner)
})

test_that("$train() + $predict()", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.featureless")
  row_ids = 1:100
  e = Experiment$new(task, learner)

  e$train(row_ids)
  expect_set_equal(e$train_set, row_ids)
  expect_class(e$model, "featureless")
  expect_class(e$model, "featureless")
  expect_equal(task$nrow, 150L)
  expect_equal(e$data$task$nrow, 150L)

  e$predict(1:10)
  expect_set_equal(e$test_set, 1:10)
  expect_data_table(as.data.table(e$prediction), nrow = 10L, any.missing = FALSE)
  expect_equal(task$nrow, 150L)
  expect_equal(e$data$task$nrow, 150L)
  expect_class(e$prediction, "Prediction")

  expect_null(e$data$performance) # performance is unset?
  e$score()
  expect_number(e$performance)
  e$predict(row_ids = 101:150) # performance is reset?

  expect_experiment(e)
})

test_that("Seeting seeds", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.featureless")
  learner$param_set$values = list(method = "sample")

  e = Experiment$new(task, learner)
  with_seed(1, {
    p1 = e$train()$predict()$prediction$response
    p2 = e$train()$predict()$prediction$response
  })
  expect_false(identical(p1, p2))

  e$seeds[] = 1:3
  for (enc in c("none", "evaluate", "callr")) {
    e$ctrl = mlr_control(encapsulate_train = enc, encapsulate_predict = enc)
    p1 = e$train()$predict()$prediction$response
    p2 = e$train()$predict()$prediction$response
    expect_true(identical(p1, p2))
  }
})

test_that("Setting train and test sets via AB", {
  e = Experiment$new("iris", "classif.featureless")
  expect_null(e$train_set)
  e$train_set = 1:120
  expect_set_equal(e$train_set, 1:120)

  expect_null(e$test_set)
  e$test_set = 121:150
  expect_set_equal(e$train_set, 1:120)
  expect_set_equal(e$test_set, 121:150)

  e$run()
  expect_equal(as.character(e$state), "scored")

  e$test_set = 141:150
  expect_equal(as.character(e$state), "trained")

  e$train_set = 1:111
  expect_equal(as.character(e$state), "defined")
})
