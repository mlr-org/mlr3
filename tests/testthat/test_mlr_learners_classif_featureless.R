test_that("autotest", {
  learner = lrn("classif.featureless")
  expect_learner(learner)
  result = run_autotest(learner, exclude = "sanity")
  expect_true(result, info = result$error)
})

test_that("Simple training/predict", {
  task = tsk("iris")
  learner = lrn("classif.featureless")
  expect_learner(learner, task)

  learner$train(task, row_ids = c(1:50, 51:70, 101:120))
  expect_class(learner$model, "classif.featureless_model")
  expect_numeric(learner$model$tab, len = 3L, any.missing = FALSE)
  prediction = learner$predict(task)
  expect_factor(prediction$response, any.missing = FALSE, levels = levels(iris$Species))
  perf = prediction$score(msr("classif.ce"))
  expect_number(perf, lower = 0.6, upper = 0.7)
})

test_that("Predict with prob", {
  task = tsk("penguins")
  learner = mlr_learners$get("classif.featureless")
  learner$predict_type = "prob"
  expect_learner(learner, task)

  p = learner$train(task)$predict(task)
  expect_matrix(p$prob, nrows = task$nrow, ncols = 3L)
  expect_names(colnames(p$prob), permutation.of = levels(task$truth()))

  p = lrn("classif.featureless", predict_type = "prob", method = "sample")$train(task)$predict(task)
  expect_number(unique(as.numeric(p$prob)), lower = 0.33, upper = 0.34)

  p = lrn("classif.featureless", predict_type = "prob", method = "weighted.sample")$train(task)$predict(task)
  expect_set_equal(unique(as.numeric(p$prob)), c(0, 1))
})

test_that("classif.featureless handles classes missing in the training set", {
  task = tsk("iris")
  row_ids = 1:100

  learner = lrn("classif.featureless", predict_type = "prob")
  learner$train(task, row_ids = row_ids)
  expect_equal(learner$model$tab, c(setosa = 50, versicolor = 50, virginica = 0))
  expect_equal(unname(learner$predict(task)$prob[1L, ]), c(0.5, 0.5, 0))
  learner$predict_type = "response"
  expect_subset(as.character(learner$predict(task)$response), c("setosa", "versicolor"))

  learner = lrn("classif.featureless", method = "sample", predict_type = "prob")
  learner$train(task, row_ids = row_ids)
  expect_equal(unname(learner$predict(task)$prob[1L, ]), rep(1 / 3, 3))
  learner$predict_type = "response"
  expect_set_equal(as.character(learner$predict(task)$response), task$class_names)

  learner = lrn("classif.featureless", method = "weighted.sample", predict_type = "prob")
  learner$train(task, row_ids = row_ids)
  expect_true(all(learner$predict(task)$prob[, "virginica"] == 0))
  learner$predict_type = "response"
  expect_subset(as.character(learner$predict(task)$response), c("setosa", "versicolor"))
})

test_that("classif.featureless raises an error if all weights are 0", {
  data = data.table(y = factor(rep(c("a", "b"), each = 5L)), x = 1, w = 0)
  task = as_task_classif(data, target = "y")
  task$set_col_roles("w", roles = "weights_learner")
  expect_error(lrn("classif.featureless")$train(task), "all weights", class = "Mlr3ErrorLearnerTrain")
})

test_that("classif.featureless works on featureless task", {
  task = tsk("wine")$select(character())
  learner = lrn("classif.featureless")
  rr = resample(task, learner, rsmp("holdout"))
  expect_resample_result(rr)
  expect_number(rr$aggregate())
})

test_that("weights are respected", {
  # Create weighted task
  data = iris
  # Give 'setosa' a much higher weight
  data$weights = fifelse(data$Species == "setosa", 100, 1)
  task = TaskClassif$new(
    id = "iris_weights",
    backend = data,
    target = "Species"
  )
  task$set_col_roles("weights", roles = "weights_learner")

  # --- Response Prediction ---
  learner_resp = lrn("classif.featureless", predict_type = "response", method = "mode")
  learner_resp$train(task, row_ids = c(1, 2, 51:150))
  p_resp_mode = learner_resp$predict(task)
  # With 'mode', the response should always be the highest weighted class
  expect_true(all(p_resp_mode$response == "setosa"))

  learner_resp$param_set$values$method = "weighted.sample"
  learner_resp$train(task) # Re-train needed for model parameters, although method is predict param
  # With 'weighted.sample', the response should heavily favor the highest weighted class
  responses_ws = factor(levels = c("setosa", "versicolor", "virginica"))
  for (i in 1:100) {
    responses_ws[i] = learner_resp$predict(task)$response[1] # predict one sample at a time
  }
  response_table_ws = table(responses_ws)
  expect_true(response_table_ws["setosa"] > 90) # Expect > 90% setosa
  expect_true(all(names(response_table_ws) %in% task$class_names))

  # --- Probability Prediction ---
  learner_prob = lrn("classif.featureless", predict_type = "prob", method = "mode")
  learner_prob$train(task)
  p_prob_mode = learner_prob$predict(task)
  # Probabilities should reflect the weighted frequencies
  expected_probs_mode = c(setosa = 100 / (100 + 1 + 1), versicolor = 1 / (100 + 1 + 1), virginica = 1 / (100 + 1 + 1))
  # Rounding necessary due to potential floating point inaccuracies
  expect_equal(round(p_prob_mode$prob[1, ], 4), round(expected_probs_mode, 4))
})

test_that("classif.featureless predicts tied labels at random", {
  task = tsk("iris")
  learner = lrn("classif.featureless")$train(task, row_ids = c(1:10, 51:60, 101:105))
  response = as.character(learner$predict(task)$response)
  expect_set_equal(response, c("setosa", "versicolor"))

  expect_equal(learner$importance(), set_names(rep(0, task$n_features), task$feature_names))
  expect_equal(learner$selected_features(), character())
})
