context("mlr_learners_classif_rpart")

test_that("autotest", {
  learner = mlr_learners$get("classif.rpart")
  expect_autotest(learner)
})

test_that("variable importance", {
  task = TaskClassif$new("foo", as_data_backend(cbind(iris, data.frame(unimportant = runif(150)))), target = "Species")
  learner = mlr_learners$get("classif.rpart")
  e = Experiment$new(task, learner)
  e$train()
  imp = e$learner$importance()
  expect_numeric(imp, min.len = 1L, any.missing = FALSE)
  expect_names(names(imp), subset.of = task$feature_names)
  expect_false(is.unsorted(rev(imp)))
})
