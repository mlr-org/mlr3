test_that("LearnerClassif predict_newdata_fast response works", {
  learner = lrn("classif.debug")
  task = tsk("pima")
  newdata = task$data()
  learner$train(task)
  pred = learner$predict_newdata_fast(newdata)

  expect_list(pred)
  expect_names(names(pred), subset.of = c("response", "prob"))
  expect_character(pred$response)
})

test_that("LearnerClassif predict_newdata_fast prob works", {
  learner = lrn("classif.debug", predict_type = "prob")
  task = tsk("pima")
  newdata = task$data()
  learner$train(task)
  pred = learner$predict_newdata_fast(newdata)

  expect_list(pred)
  expect_names(names(pred), subset.of = c("response", "prob"))
  expect_matrix(pred$prob, nrows = nrow(newdata), ncols = length(task$class_names))
})

test_that("LearnerClassif predict_newdata_fast works with missing values", {
  learner = lrn("classif.debug", predict_missing = 0.5)
  learner$encapsulate("evaluate", fallback = lrn("classif.featureless"))
  task = tsk("pima")
  newdata = task$data()
  learner$train(task)
  pred = learner$predict_newdata_fast(newdata)

  expect_list(pred)
  expect_names(names(pred), subset.of = c("response", "prob"))
  expect_character(pred$response, any.missing = FALSE)

  learner = lrn("classif.debug", predict_missing = 0.5, predict_type = "prob")
  learner$encapsulate("evaluate", fallback = lrn("classif.featureless", predict_type = "prob"))
  task = tsk("pima")
  newdata = task$data()
  learner$train(task)
  pred = learner$predict_newdata_fast(newdata)

  expect_list(pred)
  expect_names(names(pred), subset.of = c("response", "prob"))
  expect_matrix(pred$prob, nrows = nrow(newdata), ncols = length(task$class_names), any.missing = FALSE)
})

test_that("LearnerClassif predict_newdata_fast works with failed train", {
  learner = lrn("classif.debug", predict_missing = 0.5, error_train = 1)
  learner$encapsulate("evaluate", fallback = lrn("classif.featureless"))
  task = tsk("pima")
  newdata = task$data()
  learner$train(task)
  pred = learner$predict_newdata_fast(newdata)

  expect_list(pred)
  expect_names(names(pred), subset.of = c("response", "prob"))
  expect_character(pred$response, any.missing = FALSE)

  learner = lrn("classif.debug", predict_missing = 0.5, predict_type = "prob", error_train = 1)
  learner$encapsulate("evaluate", fallback = lrn("classif.featureless", predict_type = "prob"))
  task = tsk("pima")
  newdata = task$data()
  learner$train(task)
  pred = learner$predict_newdata_fast(newdata)

  expect_list(pred)
  expect_names(names(pred), subset.of = c("response", "prob"))
  expect_matrix(pred$prob, nrows = nrow(newdata), ncols = length(task$class_names), any.missing = FALSE)
})
