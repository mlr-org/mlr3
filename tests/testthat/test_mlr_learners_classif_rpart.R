context("mlr_learners_classif_rpart")

test_that("autotest", {
  learner = mlr_learners$get("classif.rpart")
  result = run_autotest(learner)
  expect_true(result, info = result$error)
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

test_that("selected_features", {
  task = TaskClassif$new("foo", as_data_backend(cbind(iris, data.frame(unimportant = runif(150)))), target = "Species")
  learner = mlr_learners$get("classif.rpart")
  learner$param_vals = insert_named(learner$param_vals, list(maxdepth = 2))
  sf = Experiment$new(task, learner)$train()$learner$selected_features()
  expect_subset(sf, task$feature_names, empty.ok = FALSE)
})
