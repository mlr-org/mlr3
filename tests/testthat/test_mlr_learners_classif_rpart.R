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

test_that("classif.rpart works if classes are missing in the training set", {
  task = tsk("iris")
  sets = list(
    first = 51:150,
    middle = c(1:50, 101:150),
    last = 1:100,
    only_first = 1:50,
    only_middle = 51:100,
    only_last = 101:150
  )

  for (row_ids in sets) {
    observed = unique(as.character(task$truth(row_ids)))
    unobserved = setdiff(task$class_names, observed)

    learner = lrn("classif.rpart")
    learner$train(task, row_ids = row_ids)
    p = learner$predict(task)
    expect_prediction(p)
    expect_factor(p$response, levels = task$class_names, any.missing = FALSE)
    expect_subset(as.character(p$response), observed)
    expect_lt(p$score(msr("classif.ce"), task = task), 0.7)

    learner = lrn("classif.rpart", predict_type = "prob")
    learner$train(task, row_ids = row_ids)
    p = learner$predict(task)
    expect_prediction(p)
    expect_equal(colnames(p$prob), task$class_names)
    expect_equal(rowSums(p$prob), rep(1, task$nrow))
    expect_true(all(p$prob[, unobserved] == 0))
    expect_subset(as.character(p$response), observed)
  }
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
