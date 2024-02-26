test_that("learner methods", {
  learner = lrn("regr.debug")
  task = tsk("mtcars")
  expect_error(learner_marshal(learner), "not been trained")
  expect_error(learner_unmarshal(learner), "not been trained")
  expect_error(learner_marshaled(learner), "not been trained")

  learner$train(task)
  expect_false(learner_marshaled(learner))
  learner$marshal()
  expect_true(learner_marshaled(learner))
  learner$unmarshal()
  expect_false(learner_marshaled(learner))
})

test_that("default method just changes class", {
  x = 1
  xm = marshal_model(x)
  expect_equal(class(xm), c("numeric_marshaled", "marshaled"))
  expect_equal(xm[["marshaled"]], x)
  expect_equal(x, unmarshal_model(xm))
})

test_that("marshaling a marshaled object does nothing", {
  x = 1
  xm = marshal_model(x)
  expect_equal(marshal_model(xm), xm)
})

test_that("unmarshaling a unmarshaled object does nothing", {
  x = 1
  xm = marshal_model(x)
  expect_equal(unmarshal_model(xm), x)
  expect_equal(unmarshal_model(unmarshal_model(xm)), x)
})


test_that("marshaling for LearnerRegrDebug", {
  task = tsk("mtcars")
  learner = lrn("regr.debug")
  learner$train(task)
  expect_false(learner$marshaled)
  learner$marshal()
  expect_true(learner$marshaled)
  expect_error(learner$predict(task), "has not been unmarshaled")
  learner$unmarshal()
  expect_learner(learner, task)
})

test_that("marshal count works for LearnerRegrDebug", {
  # to mock that marshaling behaves as expected, we need to be know how often it happened
  # note that this means that marshal_model modifies the model in a permanent way, i.e. this is not reversed by
  # unmarshal_model.
  learner = lrn("regr.debug", count_marshaling = TRUE)
  task = tsk("mtcars")
  learner$train(task)
  expect_equal(learner$model$marshal_count, 0)
  learner$marshal()$unmarshal()
  expect_equal(learner$model$marshal_count, 1)
  learner$marshal()$unmarshal()
  expect_equal(learner$model$marshal_count, 2)

  # TO make the lily learner more realistic (i.e. (un)marshaling leaves the object unchanged)
  # the count_marshaling parameter can also be set to FALSE
  learner2 = lrn("regr.debug", count_marshaling = FALSE)
  learner2$train(task)
  expect_true(is.null(learner2$model$marshal_count))
  model1 = learner2$model
  model2 = learner2$marshal()$unmarshal()$model
  expect_equal(model1, model2)
})
