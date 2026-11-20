test_that("autotest", {
  learner = lrn("regr.rpart")
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
  task = TaskRegr$new(
    "foo",
    as_data_backend(cbind(iris, data.frame(unimportant = runif(150)))),
    target = "Sepal.Length"
  )
  learner = lrn("regr.rpart")
  learner$train(task)
  imp = learner$importance()
  expect_numeric(imp, min.len = 1L, any.missing = FALSE)
  expect_names(names(imp), subset.of = task$feature_names)
  expect_false(is.unsorted(rev(imp)))
})

test_that("selected_features", {
  task = TaskRegr$new(
    "foo",
    as_data_backend(cbind(iris, data.frame(unimportant = runif(150)))),
    target = "Sepal.Length"
  )
  learner = lrn("regr.rpart", maxdepth = 2)
  sf = learner$train(task)$selected_features()
  expect_subset(sf, task$feature_names, empty.ok = FALSE)
})

test_that("default_values on rpart", {
  learner = lrn("regr.rpart")
  search_space = ps(
    minsplit = p_int(2, 128, logscale = TRUE),
    minbucket = p_int(1, 64, logscale = TRUE),
    cp = p_dbl(1e-04, 1e-1, logscale = TRUE)
  )
  task = tsk("sonar")

  values = default_values(learner, search_space, task)
  expect_names(names(values), permutation.of = c("minsplit", "minbucket", "cp"))
})

test_that("use_weights actually influences the model", {
  # Task with weights_learner role defined in helper_misc.R
  task = cars_weights_learner
  learner = lrn("regr.rpart", use_weights = "use")
  learner$train(task)
  p1 = learner$predict(task)

  learner = lrn("regr.rpart", use_weights = "ignore")
  learner$train(task)
  p2 = learner$predict(task)

  # Predictions should differ if weights were used
  expect_false(all(p1$response == p2$response))
})

test_that("regr.rpart raises an error on a task without features", {
  task = tsk("mtcars")$select(character())
  expect_error(lrn("regr.rpart")$train(task), "cannot be trained", class = "Mlr3ErrorLearnerTrain")
})

test_that("importance and selected_features of a tree without splits", {
  task = as_task_regr(data.table(y = rep(1, 30L), x = seq_len(30L)), target = "y")
  learner = lrn("regr.rpart")
  expect_error(learner$importance(), "No model stored")
  expect_error(learner$selected_features(), "No model stored")

  learner$train(task)
  expect_equal(learner$importance(), set_names(numeric()))
  expect_equal(learner$selected_features(), character())
  expect_prediction(learner$predict(task))
})

test_that("predict_raw", {
  task = tsk("mtcars")
  learner = lrn("regr.rpart")
  learner$predict_raw = TRUE
  p = learner$train(task)$predict(task)
  expect_equal(unname(p$data$raw), p$response)
})
