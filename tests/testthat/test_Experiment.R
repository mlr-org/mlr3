context("Experiment")

test_that("Partial experiments + save/restore", {
  fn = tempfile(pattern = "exp_", fileext = ".rds")
  learner = learner = mlr.learners$get("classif.rpart")

  e = Experiment$new(task = mlr.tasks$get("iris"), learner = learner)
  saveRDS(e, file = fn); e = readRDS(fn); expect_experiment(e)
  e$train(subset = 1:120)
  saveRDS(e, file = fn); e = readRDS(fn); expect_experiment(e)
  e$predict(subset = 121:150)
  saveRDS(e, file = fn); e = readRDS(fn); expect_experiment(e)
  e$score()
  saveRDS(e, file = fn); e = readRDS(fn); expect_experiment(e)
})
