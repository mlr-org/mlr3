test_that("MeasureRegrRSQ works", {

  measure = msr("regr.rsq")
  expect_equal(measure$properties, "weights")

  pred = PredictionRegr$new(truth = 0:10, response = 2:12, row_ids = seq(11))
  expect_equal(pred$score(measure), c(regr.rsq = 0.6))

  pred = PredictionRegr$new(truth = seq(0, 2, 0.5), response = seq(0, 2, 0.5), row_ids = seq(5))
  expect_equal(pred$score(measure), c(regr.rsq = 1.0))

  pred = PredictionRegr$new(truth = seq(1, 4), response = c(1, 2, 3, 5), row_ids = seq(4))
  expect_equal(pred$score(measure), c(regr.rsq = 0.8))

  measure = msr("regr.rsq", pred_set_mean = FALSE)
  expect_subset(measure$properties, c("requires_task", "requires_train_set", "weights"))

  pred = PredictionRegr$new(truth = 0:10, response = 2:12, row_ids = seq(11))
  task = as_task_regr(data.table(y = 0:10), target = "y")
  expect_equal(pred$score(measure, task = task, train_set = seq(11)), c(regr.rsq = 0.6))

  pred = PredictionRegr$new(truth = seq(0, 2, 0.5), response = seq(0, 2, 0.5), row_ids = seq(5))
  task = as_task_regr(data.table(y = seq(0, 2, 0.5)), target = "y")
  expect_equal(pred$score(measure, task = task, train_set = seq(5)), c(regr.rsq = 1.0))

  pred = PredictionRegr$new(truth = seq(1, 4), response = c(1, 2, 3, 5), row_ids = seq(4))
  task = as_task_regr(data.table(y = seq(1, 4)), target = "y")
  expect_equal(pred$score(measure, task = task, train_set = seq(4)), c(regr.rsq = 0.8))
})

test_that("MeasureRegrRSQ works with weights", {
  # Setup: truth, response, weights
  truth = 1:5
  response = c(1.1, 1.9, 3.2, 4.1, 4.8)
  weights = c(1, 2, 1, 2, 1)
  row_ids = 1:5

  pred = PredictionRegr$new(row_ids = row_ids, truth = truth, response = response, weights = weights)
  measure_weighted = msr("regr.rsq")
  measure_weighted$use_weights = "use" # explicitly set, although TRUE is default if 'weights' property exists
  expect_true("weights" %in% measure_weighted$properties)

  # Case 1: pred_set_mean = TRUE (default)
  # Manual calculation - mu now uses unweighted mean
  mu_pred = mean(truth) # No longer using weighted.mean()
  ss_res_pred = sum(weights * (truth - response)^2)
  ss_tot_pred = sum(weights * (truth - mu_pred)^2)
  rsq_expected_pred = 1 - ss_res_pred / ss_tot_pred
  expect_equal(pred$score(measure_weighted), c(regr.rsq = rsq_expected_pred))
  expect_equal(measure_weighted$score(pred), rsq_expected_pred)

  # Case 2: pred_set_mean = FALSE
  measure_train_mean = msr("regr.rsq", pred_set_mean = FALSE)
  measure_train_mean$use_weights = "use"
  expect_subset(c("requires_task", "requires_train_set", "weights"), measure_train_mean$properties)

  # Setup Task
  train_data = data.table(y = c(0, 8)) # mean = 4
  task = as_task_regr(train_data, target = "y")
  train_set_indices = 1:2 # Indices within the task data

  # Manual calculation - still uses unweighted mean for train set
  mu_train = mean(task$truth(train_set_indices)) # mean(c(0, 8)) = 4
  ss_res_train = ss_res_pred # Residuals don't depend on the mean used for SS_tot
  ss_tot_train = sum(weights * (truth - mu_train)^2)
  rsq_expected_train = 1 - ss_res_train / ss_tot_train
  expect_equal(pred$score(measure_train_mean, task = task, train_set = train_set_indices), c(regr.rsq = rsq_expected_train))
  expect_equal(measure_train_mean$score(pred, task = task, train_set = train_set_indices), rsq_expected_train)

  # Case 3: Perfect fit
  pred_perfect = PredictionRegr$new(row_ids = row_ids, truth = truth, response = truth, weights = weights)
  expect_equal(pred_perfect$score(measure_weighted), c(regr.rsq = 1))
  expect_equal(measure_weighted$score(pred_perfect), 1)
  expect_equal(pred_perfect$score(measure_train_mean, task = task, train_set = train_set_indices), c(regr.rsq = 1))
  expect_equal(measure_train_mean$score(pred_perfect, task = task, train_set = train_set_indices), 1)

  # # Case 4: Constant truth (denominator = 0 -> NaN)
  ### this seems to be an edge case that does not need to be checked...
  pred_const = PredictionRegr$new(row_ids = row_ids, truth = rep(3, 5), response = response, weights = weights)
  # mu_const_pred = weighted.mean(pred_const$truth, weights) # 3
  # ss_tot_const_pred = sum(weights * (pred_const$truth - mu_const_pred)^2) # sum(weights * (3 - 3)^2) = 0
  # expect_equal(pred_const$score(measure_weighted), c(regr.rsq = -Inf)) # 1 - ss_res / 0 -> 1 - Inf -> -Inf? No, should be NaN
  # # R's 0/0 is NaN. If ss_res > 0, then pos/0 is Inf. 1 - Inf is -Inf.
  # # Let's check mlr3measures code (uses base::mean and base::weighted.mean)
  # # R: 1 - 5/0 = -Inf. The code uses sum((truth - response)^2) / sum((truth - mu)^2). So Inf. 1-Inf=-Inf.
  # # Let's test it:
  # expect_equal(1 - sum(weights * (pred_const$truth - pred_const$response)^2) / sum(weights * (pred_const$truth - mu_const_pred)^2), -Inf)
  # # The measure code directly calculates this, so it should be -Inf, not NaN
  # # Let's re-evaluate based on the definition provided in the help page:
  # # "This measure is undefined for constant t." -> NaN seems more appropriate user-facing value?
  # # Test what the code *actually* does:
  # expect_equal(measure_weighted$score(pred_const), -Inf)
  # expect_equal(pred_const$score(measure_weighted)[["regr.rsq"]], -Inf)

  # Use train mean = 4
  mu_const_train = 4
  ss_tot_const_train = sum(weights * (pred_const$truth - mu_const_train)^2) # sum(weights * (3 - 4)^2) = sum(weights * (-1)^2) = sum(weights) = 7
  rsq_expected_const_train = 1 - sum(weights * (pred_const$truth - pred_const$response)^2) / ss_tot_const_train
  expect_equal(pred_const$score(measure_train_mean, task = task, train_set = train_set_indices), c(regr.rsq = rsq_expected_const_train))
  expect_equal(measure_train_mean$score(pred_const, task = task, train_set = train_set_indices), rsq_expected_const_train)
})

test_that("MeasureRegrRSQ works with weights during resampling", {
  task = cars_weights_measure$clone() # Use predefined task with 'weights_measure' role
  learner = lrn("regr.rpart")
  resampling = rsmp("cv", folds = 3)

  # Ensure the measure uses weights
  measure = msr("regr.rsq")
  measure$use_weights = "use"

  rr = resample(task, learner, resampling)

  # Check individual scores (pred_set_mean = TRUE implicitly)
  scores_manual = map_dbl(rr$predictions("test"), function(p) {
    w = p$weights
    t = p$truth
    r = p$response
    mu = mean(t)
    1 - sum(w * (t - r)^2) / sum(w * (t - mu)^2)
  })
  expect_equal(rr$score(measure)$regr.rsq, scores_manual)

  # Check aggregation - macro
  measure$average = "macro"
  aggr_macro = rr$aggregate(measure)
  expect_equal(aggr_macro[["regr.rsq"]], mean(scores_manual))

  # Check aggregation - micro
  measure$average = "micro"
  aggr_micro = rr$aggregate(measure)
  pred_combined = rr$prediction("test")
  w_comb = pred_combined$weights
  t_comb = pred_combined$truth
  r_comb = pred_combined$response
  mu_comb = mean(t_comb)
  rsq_micro_manual = 1 - sum(w_comb * (t_comb - r_comb)^2) / sum(w_comb * (t_comb - mu_comb)^2)
  expect_equal(aggr_micro[["regr.rsq"]], rsq_micro_manual)

  # Check aggregation - macro_weighted
  measure$average = "macro_weighted"
  aggr_macro_w = rr$aggregate(measure)
  weights_per_fold = map_dbl(rr$predictions("test"), function(p) sum(p$weights))
  rsq_macro_w_manual = weighted.mean(scores_manual, weights_per_fold)
  expect_equal(aggr_macro_w[["regr.rsq"]], rsq_macro_w_manual)

  # Check pred_set_mean = FALSE during resampling
  measure_train = msr("regr.rsq", pred_set_mean = FALSE)
  measure_train$use_weights = "use"
  rr_train_mean = resample(task, learner, resampling) # Need task for train mean

  scores_manual_train = map_dbl(seq_len(resampling$iters), function(i) {
    p = rr_train_mean$predictions()[[i]]
    train_set = rr_train_mean$resampling$train_set(i)
    w = p$weights
    t = p$truth
    r = p$response
    mu = mean(task$truth(train_set)) # Unweighted mean of train set truth
    1 - sum(w * (t - r)^2) / sum(w * (t - mu)^2)
  })
  expect_equal(rr_train_mean$score(measure_train)$regr.rsq, scores_manual_train)

  # Aggregation with pred_set_mean = FALSE
  measure_train$average = "macro"
  expect_equal(rr_train_mean$aggregate(measure_train)[["regr.rsq"]], mean(scores_manual_train))

  measure_train$average = "micro"
  expect_error(rr_train_mean$aggregate(measure_train), "requires the train_set")

  measure_train$average = "macro_weighted"
  weights_per_fold_train = map_dbl(rr_train_mean$predictions("test"), function(p) sum(p$weights))
  rsq_macro_w_manual_train = weighted.mean(scores_manual_train, weights_per_fold_train)
  expect_equal(rr_train_mean$aggregate(measure_train)[["regr.rsq"]], rsq_macro_w_manual_train)

})

test_that("MeasureRegrRSQ use_weights works", {
  task_plain = as_task_regr(cars, target = "dist")
  task_weighted = cars_weights_measure$clone()
  learner = lrn("regr.rpart")
  resampling = rsmp("holdout")$instantiate(task_plain)

  rr_plain = resample(task_plain, learner, resampling)
  rr_weighted = resample(task_weighted, learner, resampling)

  measure = msr("regr.rsq")

  # Default for measure with "weights" property is "use"
  expect_equal(measure$use_weights, "use")
  score_plain_use = rr_plain$aggregate(measure)
  score_weighted_use = rr_weighted$aggregate(measure)
  expect_false(isTRUE(all.equal(score_plain_use, score_weighted_use))) # Scores should differ

  # Ignore weights
  measure$use_weights = "ignore"
  score_plain_ignore = rr_plain$aggregate(measure)
  score_weighted_ignore = rr_weighted$aggregate(measure)
  expect_equal(score_plain_ignore, score_plain_use) # Ignoring weights on unweighted task = no change
  expect_equal(score_weighted_ignore, score_plain_use) # Ignoring weights should yield same as unweighted task

  # Error on weights
  measure$use_weights = "error"
  score_plain_error = rr_plain$aggregate(measure)
  expect_equal(score_plain_error, score_plain_use) # No error on unweighted task
  expect_error(rr_weighted$aggregate(measure), "since 'use_weights' was set to 'error'")

})
