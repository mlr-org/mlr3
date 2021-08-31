test_that("autotest", {
  learner = lrn("classif.rpart")
  expect_learner(learner)
  result = run_autotest(learner)
  expect_true(result, info = result$error)

  exclude = c("formula", "data", "weights", "subset", "na.action", "method", "model",
    "x", "y", "parms", "control", "cost", "keep_model")
  result = run_paramtest(learner, list(rpart::rpart, rpart::rpart.control), exclude, tag = "train")
  expect_true(result, info = result$error)

  exclude = c(
    "object", # handled internally
    "newdata", # handled internally
    "type", # handled internally
    "na.action" # handled internally
  )
  result = run_paramtest(learner, rpart:::predict.rpart, exclude, tag = "predict")
  expect_true(result, info = result$error)
})

test_that("variable importance", {
  task = TaskClassif$new("foo", as_data_backend(cbind(iris, data.frame(unimportant = runif(150)))), target = "Species")
  learner = lrn("classif.rpart")
  learner$train(task)
  imp = learner$importance()
  expect_numeric(imp, min.len = 1L, any.missing = FALSE)
  expect_names(names(imp), subset.of = task$feature_names)
  expect_false(is.unsorted(rev(imp)))
})

test_that("selected_features", {
  task = TaskClassif$new("foo", as_data_backend(cbind(iris, data.frame(unimportant = runif(150)))), target = "Species")
  learner = lrn("classif.rpart", maxdepth = 2)
  sf = learner$train(task)$selected_features()
  expect_subset(sf, task$feature_names, empty.ok = FALSE)
})


test_that("weights", {
  task = TaskClassif$new("foo", as_data_backend(cbind(iris, data.frame(w = rep(c(1, 10, 100), each = 50)))), target = "Species")
  task$set_col_roles("w", character())
  learner = lrn("classif.rpart")

  learner$train(task)
  c1 = learner$predict(task)$confusion

  task$set_col_roles("w", "weight")
  learner$train(task)
  c2 = learner$predict(task)$confusion

  expect_true(sum(c1[1:2, 3]) > 0)
  expect_true(sum(c2[1:2, 3]) == 0)
})
