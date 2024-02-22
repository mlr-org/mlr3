test_that("learner methods", {
  learner = lrn("classif.lily")
  task = tsk("iris")
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
