test_that("autotest", {
  learner = lrn("classif.rpart")
  expect_learner(learner)
  result = run_autotest(learner)
  expect_true(result, info = result$error)

  exclude = c(
    "formula",
    "data",
    "weights",
    "subset",
    "na.action",
    "method",
    "model",
    "x",
    "y",
    "parms",
    "control",
    "cost",
    "keep_model",
    "use_weights"
  )
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

test_that("use_weights actually influences the model", {
  task = TaskClassif$new(
    "foo",
    as_data_backend(cbind(iris, data.frame(w = rep(c(1, 10, 100), each = 50)))),
    target = "Species"
  )
  task$set_col_roles("w", "weights_learner")
  learner = lrn("classif.rpart", use_weights = "use")
  learner$train(task)
  c1 = learner$predict(task)$confusion
  learner = lrn("classif.rpart", use_weights = "ignore")
  learner$train(task)
  c2 = learner$predict(task)$confusion
  expect_false(all(c1 == c2))
})

test_that("default_values on rpart", {
  learner = lrn("classif.rpart")
  search_space = ps(
    minsplit = p_int(2, 128, logscale = TRUE),
    minbucket = p_int(1, 64, logscale = TRUE),
    cp = p_dbl(1e-04, 1e-1, logscale = TRUE)
  )
  task = tsk("sonar")

  values = default_values(learner, search_space, task)
  expect_names(names(values), permutation.of = c("minsplit", "minbucket", "cp"))
})

test_that("classif.rpart raises an error on a task without features", {
  task = tsk("iris")$select(character())
  expect_error(lrn("classif.rpart")$train(task), "cannot be trained", class = "Mlr3ErrorLearnerTrain")
})

test_that("importance and selected_features of a tree without splits", {
  task = as_task_classif(data.table(y = factor(rep(c("a", "b"), 15L)), x = 1), target = "y")
  learner = lrn("classif.rpart")
  expect_error(learner$importance(), "No model stored")
  expect_error(learner$selected_features(), "No model stored")

  learner$train(task)
  expect_equal(learner$importance(), set_names(numeric()))
  expect_equal(learner$selected_features(), character())
  expect_prediction(learner$predict(task))
})

test_that("predict_raw", {
  task = tsk("iris")
  learner = lrn("classif.rpart")
  learner$predict_raw = TRUE
  p = learner$train(task)$predict(task)
  expect_equal(unname(p$data$raw), p$response)
})
