# this test / files was missing, only classif.debug was unit-tested
# I added at least a few basic tests when i added methods "importance" and "selected_features"

test_that("Simple training/predict", {
  task = tsk("mtcars")
  learner = lrn("regr.debug")
  expect_learner(learner, task)

  prediction = learner$train(task)$predict(task)
  expect_class(learner$model, "regr.debug_model")
  expect_numeric(learner$model$response, len = 1L, any.missing = FALSE)
  expect_numeric(prediction$response, any.missing = FALSE)
})


test_that("importance and selected features", {
  l = lrn("regr.debug")
  task = tsk("mtcars")
  l$train(task)
  expect_equal(l$selected_features(), character(0))
  expect_equal(l$importance(), set_names(rep(0, task$n_features), task$feature_names))
})



