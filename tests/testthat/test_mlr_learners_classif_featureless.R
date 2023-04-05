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

  learner$train(task)
  learner$predict(task)
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
