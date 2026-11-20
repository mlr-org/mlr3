test_that("Simple training/predict", {
  task = tsk("iris")
  learner = lrn("classif.debug")
  expect_learner(learner, task)

  prediction = learner$train(task)$predict(task)
  expect_class(learner$model, "classif.debug_model")
  expect_character(learner$model$response, len = 1L, any.missing = FALSE)
  expect_factor(prediction$response, any.missing = FALSE, levels = task$class_names)
  expect_true(all(prediction$response == learner$model$response))
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
  expect_task(new_learner$model$task_train)
  expect_task(new_learner$model$task_predict)
  expect_equal(new_learner$model$task_train$row_ids, rr$resampling$train_set(1))
  expect_equal(new_learner$model$task_predict$row_ids, rr$resampling$test_set(1))
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

test_that("omitted predictions", {
  task = tsk("iris")
  for (predict_type in c("response", "prob")) {
    learner = lrn("classif.debug", predict_missing = 0.1, predict_missing_type = "omit", predict_type = predict_type)
    learner$train(task)
    expect_error(learner$predict(task), "for 15 observations is missing")

    learner$param_set$set_values(predict_missing = 0)
    expect_prediction(learner$predict(task))

    learner$param_set$set_values(predict_missing = 1)
    expect_error(learner$predict(task), "for 150 observations is missing")
  }
})

test_that("default_values", {
  learner = lrn("classif.debug")
  search_space = ps(iter = p_int(1, 10))
  task = tsk("sonar")

  values = default_values(learner, search_space, task)
  expect_names(names(values), identical.to = "iter")
})

test_that("default_values works with empty search space", {
  learner = lrn("classif.debug")
  task = tsk("sonar")
  expect_list(default_values(learner, ps(), task), len = 0)
})

test_that("validation and internal tuning", {
  task = tsk("iris")
  learner = lrn("classif.debug", iter = 100, early_stopping = TRUE, validate = 0.3, predict_type = "prob")
  learner$train(task)
  expect_list(learner$internal_valid_scores, len = 2L, types = "numeric")
  expect_permutation(names(learner$internal_valid_scores), c("acc", "mbrier"))
})

test_that("validation works with missing predictions", {
  task = tsk("iris")
  for (missing_type in c("na", "omit")) {
    learner = lrn("classif.debug", validate = 0.3, predict_missing = 0.5, predict_missing_type = missing_type)
    learner$train(task)
    expect_number(learner$internal_valid_scores$acc, lower = 0, upper = 1)
  }
})

test_that("predict_missing_type without predict_missing", {
  task = tsk("iris")
  learner = lrn("classif.debug", predict_missing_type = "omit")
  expect_prediction(learner$train(task)$predict(task))
  task = tsk("mtcars")
  learner = lrn("regr.debug", predict_missing_type = "omit")
  expect_prediction(learner$train(task)$predict(task))
})

test_that("sleep parameters must be functions", {
  expect_error(lrn("classif.debug", sleep_train = 1), "function")
  expect_error(lrn("classif.debug", sleep_predict = 1), "function")
  learner = lrn("classif.debug", sleep_train = function() 0, sleep_predict = function() 0)
  learner$configure(sleep_train = NULL, sleep_predict = NULL)
  learner$configure(sleep_train = function() 0, sleep_predict = function() 0)
  task = tsk("iris")
  expect_prediction(learner$train(task)$predict(task))
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

test_that("hotstarting continues from the stored model", {
  task = tsk("iris")
  learner_1 = lrn("classif.debug", iter = 1, count_marshaling = TRUE, check_pid = TRUE, save_tasks = TRUE)
  learner_1$train(task)

  learner_2 = lrn("classif.debug", iter = 2, count_marshaling = TRUE, check_pid = TRUE, save_tasks = TRUE)
  learner_2$hotstart_stack = HotstartStack$new(list(learner_1))
  learner_2$train(task)

  expect_class(learner_2$model, "classif.debug_model")
  expect_equal(learner_2$model$iter, 2)
  expect_equal(learner_1$model$iter, 1)
  fields = c("response", "id", "random_number", "marshal_pid", "marshal_count")
  expect_equal(unclass(learner_2$model)[fields], unclass(learner_1$model)[fields])
  expect_task(learner_2$model$task_train)

  learner_2$marshal()$unmarshal()
  expect_equal(learner_2$model$marshal_count, 1L)
  expect_prediction(learner_2$predict(task))
})
  
test_that("check_pid", {
  task = tsk("iris")
  learner = lrn("classif.debug")
  expect_false(learner$param_set$default$check_pid)
  learner$train(task)
  expect_null(learner$model$marshal_pid)

  learner = lrn("classif.debug", check_pid = TRUE)
  learner$train(task)
  expect_equal(learner$model$marshal_pid, Sys.getpid())
  learner$state$model$marshal_pid = -1L
  expect_error(learner$predict(task), "not unmarshaled correctly")
  learner$marshal()$unmarshal()
  expect_prediction(learner$predict(task))
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
  data$weights = fifelse(data$Species == "setosa", 1000, 1)
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

test_that("condition switches", {
  task = tsk("iris")
  expect_message(
    lrn("classif.debug", message_train = 1)$train(task),
    "Message from classif.debug->train()",
    fixed = TRUE
  )
  expect_warning(
    lrn("classif.debug", warning_train = 1)$train(task),
    "Warning from classif.debug->train()",
    fixed = TRUE
  )
  expect_error(
    lrn("classif.debug", error_train = 1)$train(task),
    "classif.debug->train()",
    fixed = TRUE,
    class = "Mlr3ErrorLearnerTrain"
  )
  expect_error(lrn("classif.debug", config_error = TRUE)$train(task), "misconfigured", class = "Mlr3ErrorConfig")

  learner = lrn("classif.debug")$train(task)
  learner$param_set$set_values(message_predict = 1)
  expect_message(learner$predict(task), "Message from classif.debug->predict()", fixed = TRUE)
  learner$param_set$set_values(message_predict = 0, warning_predict = 1)
  expect_warning(learner$predict(task), "Warning from classif.debug->predict()", fixed = TRUE)
  learner$param_set$set_values(warning_predict = 0, error_predict = 1)
  expect_error(learner$predict(task), "classif.debug->predict()", fixed = TRUE, class = "Mlr3ErrorLearnerPredict")

  learner = lrn("classif.debug", message_train = 0, warning_train = 0, error_train = 0)
  expect_silent(learner$train(task))
})

test_that("early stopping requires a validation task", {
  learner = lrn("classif.debug", early_stopping = TRUE, iter = 10)
  expect_error(learner$train(tsk("iris")), "validation task", class = "Mlr3ErrorConfig")

  learner$validate = 0.3
  learner$train(tsk("iris"))
  expect_integerish(learner$internal_tuned_values$iter, lower = 1, upper = 10, len = 1L)
})
