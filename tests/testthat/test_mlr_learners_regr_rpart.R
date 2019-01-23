context("mlr_learners_regr_rpart")

test_that("autotest", {
  learner = mlr_learners$get("regr.rpart")
  expect_autotest(learner)
})

test_that("variable importance", {
  task = TaskRegr$new("foo", as_data_backend(cbind(iris, data.frame(unimportant = runif(150)))), target = "Sepal.Length")
  learner = mlr_learners$get("regr.rpart")
  e = Experiment$new(task, learner)
  e$train()
  imp = e$learner$importance()
  expect_data_table(imp, nrow = 5L, ncol = 2L)
  expect_numeric(rev(imp$importance), sorted = TRUE, any.missing = FALSE)
})
