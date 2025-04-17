test_that("Simple training/predict", {
  task = tsk("iris")
  learner = lrn("classif.debug")
  expect_learner(learner, task)

  prediction = learner$train(task)$predict(task)
  expect_class(learner$model, "classif.debug_model")
  expect_character(learner$model$response, len = 1L, any.missing = FALSE)
  expect_factor(prediction$response, any.missing = FALSE, levels = levels(learner$model))
})

test_that("updating model works", {
  task = tsk("iris")
  learner = lrn("classif.debug", save_tasks = TRUE)
  learner$train(task, 1:10)
  expect_task(learner$model$task_train)
  prediction = learner$predict(task, row_ids = 11:20)
  expect_task(learner$model$task_predict)

  itrain = task$clone(TRUE)$filter(1:10)
  ipredict = task$clone(TRUE)$filter(11:20)

  expect_equal(hashes(learner$model[c("task_train", "task_predict")]), hashes(list(itrain, ipredict)))
})

test_that("updating model works / resample", {
  learner = lrn("classif.debug", save_tasks = TRUE)
  rr = resample(tsk("iris"), learner, rsmp("holdout"), store_models = TRUE)
  new_learner = rr$learners[[1]]
  expect_list(new_learner$model, len = 7)
})

test_that("NA predictions", {
  task = tsk("iris")
  learner = lrn("classif.debug", predict_missing = 0.5, predict_type = "response")
  learner$train(task)
  p = learner$predict(task)
  expect_equal(count_missing(p$response), 75L)

  learner = lrn("classif.debug", predict_missing = 0.5, predict_type = "prob")
  learner$train(task)
  p = learner$predict(task)
  expect_equal(count_missing(p$response), 75L)
  expect_equal(is.na(p$response), apply(p$prob, 1, anyMissing))
})

test_that("default_values", {
  learner = lrn("classif.debug")
  search_space = ps(iter = p_int(1, 10))
  task = tsk("pima")

  values = default_values(learner, search_space, task)
  expect_names(names(values), identical.to = "iter")
})

test_that("default_values works with empty search space", {
  learner = lrn("classif.debug")
  expect_list(default_values(learner, ps(), task), len = 0)
})

test_that("validation and internal tuning", {
  task = tsk("iris")
  learner = lrn("classif.debug", iter = 100, early_stopping = TRUE, validate = 0.3, predict_type = "prob")
  learner$train(task)
  expect_list(learner$internal_valid_scores, len = 2L, types = "numeric")
  expect_permutation(names(learner$internal_valid_scores), c("acc", "mbrier"))
})

test_that("disable internal tuning", {
  learner = lrn("classif.debug", early_stopping = TRUE, iter = 100, validate = 0.2)
  learner$param_set$disable_internal_tuning("iter")
  expect_false(learner$param_set$values$early_stopping)
})

test_that("marshaling", {
  l = lrn("classif.debug")
  expect_learner(l, tsk("iris"))
  task = tsk("iris")
  l$train(task)
  p1 = l$predict(task)
  p2 = l$marshal()$unmarshal()$predict(task)
  expect_equal(p1, p2)
})

test_that("importance and selected features", {
  l = lrn("classif.debug")
  task = tsk("iris")
  l$train(task)
  expect_equal(l$selected_features(), character(0))
  expect_equal(l$importance(), set_names(rep(0, task$n_features), task$feature_names))
})

test_that("weights are respected", {
  # Create weighted task
  data = iris
  data$weights = ifelse(data$Species == "setosa", 1000, 1)
  task = TaskClassif$new(
    id = "iris_weights",
    backend = data,
    target = "Species"
  )
  task$set_col_roles("weights", roles = "weights_learner")

  # Create learner
  learner = lrn("classif.debug")

  # Train repeatedly and collect responses
  responses = character(100)
  for (i in 1:100) {
    # learner$reset() # reset not needed as train overwrites model
    learner$train(task)
    responses[i] = learner$model$response
  }

  # Check results
  # Expect 'setosa' to be the overwhelmingly most frequent response
  response_table = table(responses)
  # check if setosa is the most frequent sampled class, probability > 0.9
  expect_true(response_table["setosa"] > 90)
  expect_true(all(names(response_table) %in% task$class_names))
})


