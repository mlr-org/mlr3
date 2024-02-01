test_that("lily", {
  task = tsk("iris")
  learner = lrn("classif.lily")
  learner$train(task)
  expect_false(learner$marshalled)
  learner$marshal()
  expect_true(learner$marshalled)
  expect_error(learner$predict(task), "has not been unmarshalled")
  learner$unmarshal()
  expect_learner(learner, task)
})

test_that("marshal count works", {
  # to mock that marshalling behaves as expected, we need to be know how often it happened
  # note that this means that marshal_model modifies the model in a permanent way, i.e. this is not reversed by
  # unmarshal_model.
  learner = lrn("classif.lily", count_marshalling = TRUE)
  task = tsk("iris")
  learner$train(task)
  expect_equal(learner$model$marshal_count, 0)
  learner$marshal()$unmarshal()
  expect_equal(learner$model$marshal_count, 1)
  learner$marshal()$unmarshal()
  expect_equal(learner$model$marshal_count, 2)

  # TO make the lily learner more realistic (i.e. (un)marshalling leaves the object unchanged)
  # the count_marshalling parameter can also be set to FALSE
  learner2 = lrn("classif.lily", count_marshalling = FALSE)
  learner2$train(task)
  expect_true(is.null(learner2$model$marshal_count))
  model1 = learner2$model
  model2 = learner2$marshal()$unmarshal()$model
  expect_equal(model1, model2)
})
