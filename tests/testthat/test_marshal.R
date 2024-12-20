test_that("learner methods", {
  learner = lrn("classif.debug")
  task = tsk("iris")
  expect_false(learner_marshaled(learner))

  learner$train(task)
  expect_false(learner_marshaled(learner))
  learner$marshal()
  expect_true(learner_marshaled(learner))
  learner$unmarshal()
  expect_false(learner_marshaled(learner))
})

test_that("default method does nothing", {
  x = 1
  expect_equal(x, marshal_model(x))
})

test_that("marshaling a marshaled object does nothing", {
  x = 1
  xm = marshal_model(x)
  expect_equal(marshal_model(xm), xm)
})

test_that("unmarshaling an unmarshaled object does nothing", {
  x = 1
  xm = marshal_model(x)
  expect_equal(unmarshal_model(xm), x)
  expect_equal(unmarshal_model(unmarshal_model(xm)), x)
})


test_that("marshaling for LearnerClassif", {
  task = tsk("iris")
  learner = lrn("classif.debug")
  learner$train(task)
  expect_false(learner$marshaled)
  learner$marshal()
  expect_true(learner$marshaled)
  learner$unmarshal()
  expect_learner(learner, task)
})

test_that("marshal count works for LearnerClassifDebug", {
  # to mock that marshaling behaves as expected, we need to be know how often it happened
  # note that this means that marshal_model modifies the model in a permanent way, i.e. this is not reversed by
  # unmarshal_model.
  learner = lrn("classif.debug", count_marshaling = TRUE)
  task = tsk("iris")
  learner$train(task)
  expect_equal(learner$model$marshal_count, 0)
  learner$marshal()$unmarshal()
  expect_equal(learner$model$marshal_count, 1)
  learner$marshal()$unmarshal()
  expect_equal(learner$model$marshal_count, 2)

  learner2 = lrn("classif.debug", count_marshaling = FALSE)
  learner2$train(task)
  expect_null(learner2$model$marshal_count)
  model1 = learner2$model
  model2 = learner2$marshal()$unmarshal()$model
  expect_equal(model1, model2)
})

test_that("printer", {
  expect_output(print(lrn("classif.debug")$train(tsk("iris"))$marshal()$model), "<classif.debug_model_marshaled>")
})
