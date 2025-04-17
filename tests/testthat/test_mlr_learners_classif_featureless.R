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
  data$weights = ifelse(data$Species == "setosa", 100, 1)
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
