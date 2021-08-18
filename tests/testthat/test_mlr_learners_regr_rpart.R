test_that("autotest", {
  learner = lrn("regr.rpart")
  expect_learner(learner)
  result = run_autotest(learner)
  expect_true(result, info = result$error)

  exclude = c("formula", "data", "weights", "subset", "na.action", "method", "model",
    "x", "y", "parms", "control", "cost")
  result = run_paramtest(learner, rpart::rpart, exclude)
  expect_true(result, info = result$error)
})

test_that("variable importance", {
  task = TaskRegr$new("foo", as_data_backend(cbind(iris, data.frame(unimportant = runif(150)))), target = "Sepal.Length")
  learner = lrn("regr.rpart")
  learner$train(task)
  imp = learner$importance()
  expect_numeric(imp, min.len = 1L, any.missing = FALSE)
  expect_names(names(imp), subset.of = task$feature_names)
  expect_false(is.unsorted(rev(imp)))
})

test_that("selected_features", {
  task = TaskRegr$new("foo", as_data_backend(cbind(iris, data.frame(unimportant = runif(150)))), target = "Sepal.Length")
  learner = lrn("regr.rpart", maxdepth = 2)
  sf = learner$train(task)$selected_features()
  expect_subset(sf, task$feature_names, empty.ok = FALSE)
})

test_that("weights", {
  task = TaskRegr$new("foo", as_data_backend(cbind(iris, data.frame(w = rep(c(1, 10, 100), each = 50)))), target = "Sepal.Length")
  task$set_col_roles("w", character())
  learner = lrn("regr.rpart")

  learner$train(task)
  p1 = learner$predict(task)

  task$set_col_roles("w", "weight")
  learner$train(task)
  p2 = learner$predict(task)

  expect_lt(p1$score(), p2$score())
})
